# Utility script for analyzing Slay the Spire data

## Step 1: Explore and inspect data structure
#str(data$event[[1]])

## Step 2: Convert the list of nested data to a data frame
nested_df <- bind_rows(data$event)

## Step 3: Display column names and data summary
#cat("Column Names in Nested Data:\n")
#print(colnames(nested_df))

#cat("\nSummary of Nested Data:\n")
#print(summary(nested_df))

#cat("\nFirst Few Rows of Nested Data:\n")
#print(head(nested_df))

# Step 4: Calculate win rate by character (Reddit Data)
win_rate_by_character_data <- nested_df %>%
  group_by(character_chosen) %>%
  summarize(
    total_runs = n(),
    victories = sum(victory == TRUE, na.rm = TRUE),
    win_rate = victories / total_runs
  ) %>%
  mutate(source = "Reddit Data")  # Add source column for identification

# Step 5: Calculate win rate by character (Bryan's Data)
win_rate_by_character_bryan <- bryan_data %>%
  # Extract the relevant data from the nested structure
  mutate(character_chosen = sapply(event, function(x) x$character_chosen),
         victory = sapply(event, function(x) x$victory)) %>%
  # Group by the extracted character_chosen
  group_by(character_chosen) %>%
  summarize(
    total_runs = n(),
    victories = sum(victory == TRUE, na.rm = TRUE),
    win_rate = victories / total_runs
  ) %>%
  mutate(source = "My Data")  # Add source column for identification

# Step 6: Combine the win rate data
combined_win_rates <- bind_rows(win_rate_by_character_data, win_rate_by_character_bryan)

# Step 7: Display the combined win rates
cat("\nCombined Win Rate by Character:\n")
print(combined_win_rates)

# Step 8: Plot the combined win rates with custom colors
library(ggplot2)
library(scales)

# Custom colors for each character and source
character_colors <- c(
  "DEFECT.Reddit Data" = "blue",
  "IRONCLAD.Reddit Data" = "red",
  "THE_SILENT.Reddit Data" = "green",
  "WATCHER.Reddit Data" = "purple",
  "DEFECT.My Data" = "lightblue",
  "IRONCLAD.My Data" = "lightcoral",
  "THE_SILENT.My Data" = "lightgreen",
  "WATCHER.My Data" = "plum"
)

# Custom color vector to plot just the reddit data alone
simple_character_colors <- c(
  "DEFECT" = "blue",
  "IRONCLAD" = "red",
  "THE_SILENT" = "green",
  "WATCHER" = "purple"
)

# Plot win rates by character with internal labels for data source and no legend
print(
  ggplot(combined_win_rates, aes(x = character_chosen, y = win_rate, fill = interaction(character_chosen, source))) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = source), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +  # Add source labels inside the bars
    scale_y_continuous(labels = percent) +  # Convert y-axis to percentage
    scale_fill_manual(values = character_colors) +  # Apply custom colors
    labs(title = "Win Rate by Character in Slay the Spire",
         x = "Character",
         y = "Win Rate (%)") +
    theme_minimal() +
    theme(legend.position = "none")  # Remove the legend
)


# Step 9: Calculate win rates by character by ascension level (Reddit Data)
win_rate_by_character_by_ascension_level <- nested_df %>%
  group_by(character_chosen, ascension_level) %>%
  summarize(
    total_runs = n(),
    victories = sum(victory == TRUE, na.rm = TRUE),
    win_rate = victories / total_runs
  )

# Step 10: Create scatter plot of win rates by character by ascension level (Reddit Data)
# Create the scatter plot with the simplified colors
print(ggplot(win_rate_by_character_by_ascension_level, aes(x = ascension_level, y = win_rate, color = character_chosen)) +
        geom_point(size = 3) +
        geom_smooth(method = "loess", se = FALSE) +  # Add a trend line
        scale_color_manual(values = simple_character_colors) +  # Apply simple colors
        labs(title = "Win Rate by Ascension Level and Character",
             x = "Ascension Level",
             y = "Win Rate (%)") +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
)

# Step 11: Calculate top 5 relics in winning runs for each character, excluding starting relics (Reddit Data)

# Define the starting relics to exclude
starting_relics <- c("Cracked Core", "Burning Blood", "Ring of the Snake", "PureWater")

# Filter out starting relics, rank the remaining ones, and select the top 5
top_5_relics_by_character <- nested_df %>%
  filter(victory == TRUE) %>%
  unnest(cols = relics) %>%
  filter(!relics %in% starting_relics) %>%  # Exclude starting relics
  group_by(character_chosen, relics) %>%
  summarize(count = n()) %>%
  mutate(rank = dense_rank(desc(count))) %>%  # Rank relics by frequency
  filter(rank >= 1 & rank <= 5) %>%  # Select top 5 relics for each character
  ungroup()

# Calculate win rate for each relic
relic_win_rate <- nested_df %>%
  unnest(cols = relics) %>%
  group_by(character_chosen, relics) %>%
  summarize(
    total_runs = n(),
    victories = sum(victory == TRUE),
    win_rate = victories / total_runs
  ) %>%
  ungroup()

# Merge win rate data with the top 5 relics
top_5_relics_with_win_rate <- top_5_relics_by_character %>%
  left_join(relic_win_rate, by = c("character_chosen", "relics"))

# Plot top 5 relics by appearances in winning runs and display win rate (Reddit Data)
# Plot top 5 relics by appearances in winning runs with just the simple colors, will need to update when comparing with bryan_data later
print(ggplot(top_5_relics_with_win_rate, aes(x = relics, y = count, fill = character_chosen)) +
        geom_bar(stat = "identity") +
        geom_line(aes(y = win_rate * max(count), group = 1), color = "black", size = 1, linetype = "dashed") +  # Add win rate line
        geom_text(aes(label = paste0(round(win_rate * 100, 1), "%"), y = win_rate * max(count)), vjust = -0.5, size = 3.5) +  # Annotate with win rates
        facet_wrap(~ character_chosen, scales = "free_x") +
        scale_x_discrete(drop = FALSE) +
        scale_fill_manual(values = simple_character_colors) +
        labs(title = "Top 5 Winning Relics by Character with Win Rates",
             x = "Relic",
             y = "Count",
             sec.axis = sec_axis(~./max(top_5_relics_with_win_rate$count), name = "Win Rate (%)")) +  # Add secondary axis
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
)
