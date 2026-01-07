# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load the dataset
deliveries <- read_csv("C:/Users/yuvan/Downloads/rdataset.csv")

# Check the structure of the data
str(deliveries)

# Calculate runs scored by each batsman in each over
batsman_performance <- deliveries %>%
  group_by(match_id, over, batsman) %>%
  summarize(total_runs = sum(batsman_runs, na.rm = TRUE)) %>%
  ungroup()

# Predict runs for a batsman in an over based on their performance in the previous over
batsman_performance <- batsman_performance %>%
  arrange(match_id, batsman, over) %>%
  group_by(match_id, batsman) %>%
  mutate(predicted_runs = lag(total_runs)) %>%
  ungroup()

# Top 10 batsmen by total runs
top_batsmen <- deliveries %>%
  group_by(batsman) %>%
  summarize(total_runs = sum(batsman_runs, na.rm = TRUE)) %>%
  arrange(desc(total_runs)) %>%
  slice(1:10)

# Visualize the top 10 batsmen
ggplot(top_batsmen, aes(x = reorder(batsman, -total_runs), y = total_runs, fill = batsman)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Batsmen by Runs", x = "Batsman", y = "Total Runs") +
  theme_minimal()

# Top 10 bowlers by economy rate
top_bowlers <- deliveries %>%
  group_by(bowler) %>%
  summarize(
    total_runs = sum(total_runs, na.rm = TRUE),
    total_overs = n_distinct(paste(match_id, over))
  ) %>%
  mutate(economy_rate = total_runs / total_overs) %>%
  arrange(economy_rate) %>%
  slice(1:10)

# Visualize the top 10 bowlers by economy rate
ggplot(top_bowlers, aes(x = reorder(bowler, economy_rate), y = economy_rate, fill = bowler)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Bowlers by Economy Rate", x = "Bowler", y = "Economy Rate") +
  theme_minimal()

# Save the cleaned data to CSV for validation
write_csv(batsman_performance, "batsman_performance.csv")
write_csv(top_batsmen, "top_batsmen.csv")
write_csv(top_bowlers, "top_bowlers.csv")
