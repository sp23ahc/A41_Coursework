library(ggplot2) # For visualizations
library(readr) # For importing datasets
library(tidyverse)
library(dplyr)

#importing dataset
df <- read_csv("C:\\Users\\Sarthak\\OneDrive\\Documents\\A41_Coursework\\horse.csv")
head(df)

#cleaning the dataset

# Checking for missing values
colSums(is.na(df))

# Function for replacing NAs with mean for numeric columns
impute_mean <- function(x) {
  ifelse(is.na(x), mean(x, na.rm = TRUE), x)
}

# Function for replacing NAs with mode for categorical/character columns
impute_mode <- function(x) {
  ifelse(is.na(x), names(which.max(table(x))), x)
}


# Identifying numeric columns
numeric_cols <- sapply(df, is.numeric)

# Identifying character and factor columns
char_factor_cols <- sapply(df, function(x) is.character(x) || is.factor(x))

# Applying imputation to numeric columns
df[, numeric_cols] <- lapply(df[, numeric_cols], impute_mean)

# Applying the same to character and factor columns
df[, char_factor_cols] <- lapply(df[, char_factor_cols], impute_mode)


# Verifying if there are any more NAs
colSums(is.na(df))


