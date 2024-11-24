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
