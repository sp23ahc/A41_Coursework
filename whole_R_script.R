library(ggplot2) # For visualizations
library(readr) # For importing datasets
library(tidyverse)
library(dplyr)

#importing dataset
df <- read_csv("D:\\MSC\\TRD Original\\horse.csv")
#need to change file directory

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



#data Visualisation
#cross-tabulation
df %>%
  count(outcome, surgery) %>%  # Count occurrences, grouping by outcome and then surgery
  pivot_wider(names_from = surgery, values_from = n, values_fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric)))) %>%
  knitr::kable(caption = "Surgery Outcome Table") # Add a title to the table

#plotting the graph

ggplot(df, aes(x = surgery, fill = outcome)) +
  geom_bar(position = "fill") +
  labs(title = "Stacked bar chart of surgery for outcome",
       x = "Surgery",
       y = "Proportion",
       fill = "Outcome")


#Analysis(Chi-squared Test)
# Creating a contingency table
contingency_table <- table(df$outcome, df$surgery)

# Performing the chi-squared test
chisq_test_result <- chisq.test(contingency_table)

# Printing the test results
chisq_test_result


#Results
p_value <- chisq_test_result$p.value

# Determining the outcome based on the p-value
if (p_value < 0.05) {
  outcome <- "Reject the null hypothesis: There is a statistically significant association between outcome and surgery."
} else {
  outcome <- "Fail to reject the null hypothesis: There is no statistically significant association between outcome and surgery."
}

# Printing the results on the basis of the statistical test
# Extracting the p-value from the chi-squared test result
p_value <- chisq_test_result$p.value

# Determining the outcome based on the p-value
if (p_value < 0.05) {
  res_test <- "Reject the null hypothesis:There is a difference in the proportions(s) of outcome and horses that had surgery and horses that did not have surgery. "
} else {
  res_test <- "Fail to reject the null hypothesis:There is no difference in the proportions(s) of outcome between/among horses that had surgery and horses that did not have surgery."
}

# Printing the outcome
res_test
