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