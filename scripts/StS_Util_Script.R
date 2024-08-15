# Utility script for analyzing Slay the Spire data

# Step 1: Explore and inspect data structure
str(data$event[[1]])

# Step 2: Convert the list of nested data to a data frame
nested_df <- bind_rows(data$event)

# Step 3: Display column names and data summary
cat("Column Names in Nested Data:\n")
print(colnames(nested_df))

cat("\nSummary of Nested Data:\n")
print(summary(nested_df))

cat("\nFirst Few Rows of Nested Data:\n")
print(head(nested_df))

# Step 4: Calculate win rate by character
win_rate_by_character <- nested_df %>%
  group_by(character_chosen) %>%
  summarize(
    total_runs = n(),
    victories = sum(victory == TRUE, na.rm = TRUE),
    win_rate = victories / total_runs
  )

# Step 5: Display the win rates
cat("\nWin Rate by Character:\n")
print(win_rate_by_character)

# Step 6: Plot the win rates with custom colors to line up with the character art and percentage y-axis instead of float
library(ggplot2)
library(scales)

# Custom colors for each character
character_colors <- c(
  "DEFECT" = "blue",
  "IRONCLAD" = "red",
  "THE_SILENT" = "green",
  "WATCHER" = "purple"
)

ggplot(win_rate_by_character, aes(x = character_chosen, y = win_rate, fill = character_chosen)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent) +  # Convert y-axis to percentage
  scale_fill_manual(values = character_colors) +  # Apply custom colors
  labs(title = "Win Rate by Character in Slay the Spire",
       x = "Character",
       y = "Win Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")
