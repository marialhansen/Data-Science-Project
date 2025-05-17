# Libaries 
library("readxl")
library(dplyr)

# Combining the reports
report_1 <- read_excel("/Users/oliversoeby/Downloads/XXXXX")
report_2 <- read_excel("/Users/oliversoeby/Downloads/XXXXX")
combined_data <- rbind(report_1, report_2)

# Removing row 1, as it doesn't make sense here
combined_data <- select(combined_data, -...1)

# Looking at the sum of the content types
table(combined_data$content_type)

# If fill level is negative replace with 0
combined_data$fill_level[combined_data$fill_level < 0] <- 0

# If fill level is over 1 replace with 1
combined_data$fill_level[combined_data$fill_level > 1] <- 1

# Removing everything thats not Plastik
combined_data <- combined_data[combined_data$content_type == "Plastik", ]

# Checking that everything is there
length(unique(combined_data$address))
length(unique(combined_data$container_id))

combined_data$date <- as.Date(combined_data[[1]], format = "%d-%m-%Y")



# Combining the container id with date
combined_data <- combined_data %>%
  arrange(container_id, date)


# Taking only one date from each day
combined_data <- combined_data %>%
  arrange(combined_data$container_id, date) %>%
  group_by(combined_data$container_id, date) %>%
  slice(1) %>%
  ungroup()

# Checking nothing has been removed
unique(combined_data$container_id)
length(unique(combined_data$container_id))

# Removing the 2023 dates
combined_data <- combined_data[combined_data$date != "2023-12-30", ]
combined_data <- combined_data[combined_data$date != "2023-12-31", ]

# Removing row 10, 11, 12, as it doesn't make sense here
combined_data <- select(combined_data, -10, -11, -12)

# Exporting the CSV file
write.csv(combined_data, "/Users/oliversoeby/Desktop/cleaned_dataset.csv", row.names = FALSE)



