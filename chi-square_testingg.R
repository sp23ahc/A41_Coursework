#Analysis(Chi-squared Test)
# Creating a contingency table
contingency_table <- table(df$outcome, df$surgery)

# Performing the chi-squared test
chisq_test_result <- chisq.test(contingency_table)

# Printing the test results
chisq_test_result


#Results
p_value <- chisq_test_result$p.value