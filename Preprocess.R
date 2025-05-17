library(readr)
data <- read_csv("~/cleaned_dataset.csv")
View(data)

# Look at seasonality/fill level (for a random sensor, swap the ID): 
# Seasonal: gdrtdn, 9m77sz, see5qb, 9qd4xa, kxf5hc, fwx5fm, qmt7az, uycv6r, zucqzm, 2nv827, hdgb6e
# qayrqn, b3svq6, 954ayw
library(ggplot2)
ggplot(filter(data, sensor_id == "w95pqz"), aes(x = as.Date(date), y = fill_level)) +
  geom_line() +
  labs(title = "Fill Level Over Time — Bin w95pqz", x = "Date", y = "Fill Level")
# Looks like there is a lot of seasonality here, however..
# The sensors with IDs ne2fky and w95pqz look very different to the rest, they either don't
# reset to 0 or they don't start filling for months. They should be removed.

# Removing the bad sensors
bad_sensors <- c("ne2fky", "w95pqz")
data <- data %>%
  filter(!(sensor_id %in% bad_sensors))
unique(data$sensor_id) # They are gone :)

### Check seasonality
library(forecast)
acf(data$fill_level, lag.max = 30) # For fill
adf.test(data$fill_level) # Stationary data

library(tsibble)
library(feasts)
library(dplyr)

# Aggregate fill_level per day (e.g., average fill across all bins)
daily_avg <- data %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(avg_fill = mean(fill_level, na.rm = TRUE)) %>%
  arrange(date)

# Create time series object
ts_data <- daily_avg %>%
  as_tsibble(index = date)

# STL Decomposition
ts_data %>%
  model(STL(avg_fill)) %>%
  components() %>%
  autoplot()

### Regression with seasonality dummy
library(lubridate)
# Need a weekday variable
data <- data %>%
  mutate(
    date = as.Date(date),
    weekday = lubridate::wday(date, label = TRUE, week_start = 1)
  )
### Checking for seasonality with dummies
ts_fill <- ts(daily_avg$avg_fill, frequency = 7)  # Weekly seasonality
library(forecast)
# Create weekday dummies
season <- seasonaldummy(ts_fill)
# Fit regression model with seasonal dummies
fit <- tslm(ts_fill ~ season) # tslm is time series linear regression
summary(fit) # Super seasonal, like every day is significant in fact but Tuesdays and Sundays slightly less than the
# rest, maybe due to bins being emptied

# Now we know we have some serious seasonality in the fill_level variable (surprise, surprise), we need to get rid
# of it. First, let's make some new variables that are actually useful.
