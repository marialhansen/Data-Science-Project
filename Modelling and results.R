# -------------------------
# 1. Libraries & Data Load
# -------------------------
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(randomForest)
library(rpart)
library(rpart.plot)
library(xgboost)
library(caret)
library(vip)

data <- read_csv("~/cleaned_dataset.csv") %>%
  mutate(date = as.Date(date))

View(data)

# -------------------------
# 2. Data Cleaning
# -------------------------
# Remove faulty sensors
bad_sensors <- c("ne2fky", "w95pqz")
data <- data %>% filter(!(sensor_id %in% bad_sensors))

# Create Weekday Variable
data <- data %>% 
  mutate(weekday = lubridate::wday(date, label = TRUE, week_start = 1))

# -------------------------
# 3. Chronological Train-Test Split
# -------------------------
set.seed(123)
data <- data %>% arrange(date)
split_index <- floor(0.8 * nrow(data))
train_data <- data[1:split_index, ]
test_data  <- data[(split_index + 1):nrow(data), ]

# -------------------------
# 4. Feature Engineering
# -------------------------
# Deseasonalisation
lm_model <- lm(fill_level ~ weekday, data = train_data)
train_data$fill_deseasoned <- resid(lm_model)
test_data$fill_deseasoned  <- test_data$fill_level - predict(lm_model, newdata = test_data)

# Delta Fill
train_data <- train_data %>%
  group_by(sensor_id) %>%
  arrange(date) %>%
  mutate(delta_fill = fill_level - lag(fill_level, default = 0)) %>%
  ungroup()

test_data <- test_data %>%
  group_by(sensor_id) %>%
  arrange(date) %>%
  mutate(delta_fill = fill_level - lag(fill_level, default = 0)) %>%
  ungroup()

# Rolling Fill Rate
train_data <- train_data %>%
  group_by(sensor_id) %>%
  arrange(date) %>%
  mutate(rolling_fill_rate = rollapply(delta_fill, width = 3, FUN = mean, align = "right", fill = NA)) %>%
  ungroup()

test_data <- test_data %>%
  group_by(sensor_id) %>%
  arrange(date) %>%
  mutate(rolling_fill_rate = rollapply(delta_fill, width = 3, FUN = mean, align = "right", fill = NA)) %>%
  ungroup()

# Emptied Indicator
train_data <- train_data %>% mutate(emptied = delta_fill < -0.2)
test_data <- test_data %>% mutate(emptied = delta_fill < -0.2)

# Days Since Emptied
train_data <- train_data %>% arrange(sensor_id, date) %>% group_by(sensor_id) %>%
  mutate(fill_cycle_id = cumsum(coalesce(emptied, FALSE)),
         days_since_empty = sequence(rle(fill_cycle_id)$lengths),
         days_since_empty_capped = pmin(days_since_empty, 30)) %>% ungroup()

test_data <- test_data %>% arrange(sensor_id, date) %>% group_by(sensor_id) %>%
  mutate(fill_cycle_id = cumsum(coalesce(emptied, FALSE)),
         days_since_empty = sequence(rle(fill_cycle_id)$lengths),
         days_since_empty_capped = pmin(days_since_empty, 30)) %>% ungroup()


# Target Variable
train_data <- train_data %>% mutate(alert_now = if_else(fill_level >= 0.7, 1, 0))
test_data  <- test_data  %>% mutate(alert_now = if_else(fill_level >= 0.7, 1, 0))

# Remove NAs in Test Data
test_data_clean <- test_data %>%
  filter(!is.na(alert_now), !is.na(fill_deseasoned), 
         !is.na(delta_fill), !is.na(rolling_fill_rate), 
         !is.na(days_since_empty_capped))

# -------------------------
# 5. Modeling
# -------------------------
## Logistic Regression
logit_model <- glm(alert_now ~ fill_deseasoned + delta_fill + rolling_fill_rate + days_since_empty_capped, 
                   data = na.omit(train_data), family = "binomial")
logit_preds <- predict(logit_model, newdata = test_data_clean, type = "response")
logit_pred_class <- ifelse(logit_preds >= 0.2, 1, 0)
summary(logit_model)
table(Predicted = logit_pred_class, Actual = test_data_clean$alert_now)

## Random Forest
train_data$alert_now <- as.factor(train_data$alert_now)
rf_model <- randomForest(alert_now ~ fill_deseasoned + delta_fill + rolling_fill_rate + days_since_empty_capped, 
                         data = na.omit(train_data), ntree = 500, importance = TRUE)
rf_preds <- predict(rf_model, newdata = test_data_clean)
print(rf_model)
table(Predicted = rf_preds, Actual = test_data_clean$alert_now)

## Decision Tree
tree_model <- rpart(alert_now ~ fill_deseasoned + delta_fill + rolling_fill_rate + days_since_empty_capped, 
                    data = na.omit(train_data), method = "class")
tree_preds <- predict(tree_model, newdata = test_data_clean, type = "class")
rpart.plot(tree_model)
table(Predicted = tree_preds, Actual = test_data_clean$alert_now)
summary(tree_model)

## XGBoost
xgb_train <- na.omit(train_data)
train_matrix <- model.matrix(alert_now ~ fill_deseasoned + delta_fill + rolling_fill_rate + days_since_empty_capped - 1, data = xgb_train)
test_matrix  <- model.matrix(alert_now ~ fill_deseasoned + delta_fill + rolling_fill_rate + days_since_empty_capped - 1, data = test_data_clean)
label <- ifelse(xgb_train$alert_now == 1, 1, 0)

xgb_model <- xgboost(data = train_matrix, label = label, nrounds = 100, 
                     objective = "binary:logistic", verbose = 0)
xgb_preds <- predict(xgb_model, newdata = test_matrix)
xgb_pred_class <- factor(ifelse(xgb_preds >= 0.2, 1, 0), levels = c(0, 1))
table(Predicted = xgb_pred_class, Actual = test_data_clean$alert_now)

# -------------------------
# 6. Evaluation
# -------------------------
actual <- factor(test_data_clean$alert_now, levels = c(0, 1))
logit_pred_class <- factor(logit_pred_class, levels = c(0, 1))
tree_pred_class  <- factor(tree_preds, levels = c(0, 1))
rf_pred          <- factor(rf_preds, levels = c(0, 1))

logit_eval <- confusionMatrix(logit_pred_class, actual, positive = "1")
rf_eval    <- confusionMatrix(rf_pred, actual, positive = "1")
tree_eval  <- confusionMatrix(tree_pred_class, actual, positive = "1")
xgb_eval   <- confusionMatrix(xgb_pred_class, actual, positive = "1")

results <- data.frame(
  Model = c("Logistic", "Random Forest", "Decision Tree", "XGBoost"),
  Accuracy = c(logit_eval$overall["Accuracy"],
               rf_eval$overall["Accuracy"],
               tree_eval$overall["Accuracy"],
               xgb_eval$overall["Accuracy"]),
  Recall = c(logit_eval$byClass["Recall"],
             rf_eval$byClass["Recall"],
             tree_eval$byClass["Recall"],
             xgb_eval$byClass["Recall"]),
  Precision = c(logit_eval$byClass["Precision"],
                rf_eval$byClass["Precision"],
                tree_eval$byClass["Precision"],
                xgb_eval$byClass["Precision"]),
  F1 = c(logit_eval$byClass["F1"],
         rf_eval$byClass["F1"],
         tree_eval$byClass["F1"],
         xgb_eval$byClass["F1"])
)
print(results)

# -------------------------
# 7. Variable Importance
# -------------------------
# Logistic Regression
summary(logit_model)

# Random Forest
vip(rf_model)
importance(rf_model)

# Decision Tree
rpart.plot(tree_model, type = 2, extra = 101)
tree_model$variable.importance

# XGBoost
xgb_imp <- xgb.importance(model = xgb_model)
vip(xgb_model, num_features = 10, geom = "col")

# -------------------------
# 8. Misclassification Analysis
# -------------------------
test_data_clean$pred_xgb <- xgb_pred_class
test_data_clean$actual   <- actual

misclassified <- test_data_clean %>% filter(pred_xgb != actual)
View(misclassified)

false_negatives <- test_data_clean %>% filter(pred_xgb == 0 & actual == 1)
View(false_negatives)

# -------------------------
# 9. Accuracy & Error Summary
# -------------------------
acc_logit <- mean(logit_pred_class == actual)
acc_rf    <- mean(rf_pred == actual)
acc_tree  <- mean(tree_pred_class == actual)
acc_xgb   <- mean(xgb_pred_class == actual)

error_logit <- 1 - acc_logit
error_rf    <- 1 - acc_rf
error_tree  <- 1 - acc_tree
error_xgb   <- 1 - acc_xgb

performance_summary <- data.frame(
  Model = c("Logistic", "Random Forest", "Decision Tree", "XGBoost"),
  Accuracy = round(c(acc_logit, acc_rf, acc_tree, acc_xgb), 4),
  Misclassification_Rate = round(c(error_logit, error_rf, error_tree, error_xgb), 4)
)
print(performance_summary)
