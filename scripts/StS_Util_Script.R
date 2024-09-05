# Utility script for analyzing Slay the Spire data ####

## Step 1: Explore and inspect data structure ====
# str(data$event[[1]])

## Step 2: Convert the list of nested data to a data frame ====
nested_df <- bind_rows(data$event)

## Step 3: Display column names and data summary ====
# cat("Column Names in Nested Data:\n")
# print(colnames(nested_df))

# cat("\nSummary of Nested Data:\n")
# print(summary(nested_df))

# cat("\nFirst Few Rows of Nested Data:\n")
# print(head(nested_df))

## Step 4: Calculate win rate by character (Reddit Data) ====
win_rate_by_character_data <- nested_df %>%
  group_by(character_chosen) %>%
  summarize(
    total_runs = n(),
    victories = sum(victory == TRUE, na.rm = TRUE),
    win_rate = victories / total_runs
  ) %>%
  mutate(source = "Reddit Data")  # Add source column for identification

## Step 5: Calculate win rate by character (Bryan's Data) ====
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

## Step 6: Combine the win rate data ====
combined_win_rates <- bind_rows(win_rate_by_character_data, win_rate_by_character_bryan)

## Step 7: Display the combined win rates ====
cat("\nCombined Win Rate by Character:\n")
print(combined_win_rates)

## Step 8: Define the plots ====
library(ggplot2)
library(scales)

## Custom colors for each character and source 
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

## Custom color vector to plot just the reddit data alone 
simple_character_colors <- c(
  "DEFECT" = "blue",
  "IRONCLAD" = "red",
  "THE_SILENT" = "green",
  "WATCHER" = "purple"
)

## Plot win rates by character with internal labels for data source and no legend
plot_win_rates <- ggplot(combined_win_rates, aes(x = character_chosen, y = win_rate, fill = interaction(character_chosen, source))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = source), position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +  # Add source labels inside the bars
  scale_y_continuous(labels = percent) +  # Convert y-axis to percentage
  scale_fill_manual(values = character_colors) +  # Apply custom colors
  labs(title = "Win Rate by Character in Slay the Spire",
       x = "Character",
       y = "Win Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove the legend

## Step 9: Calculate win rates by character by ascension level (Reddit Data) ====
win_rate_by_character_by_ascension_level <- nested_df %>%
  group_by(character_chosen, ascension_level) %>%
  summarize(
    total_runs = n(),
    victories = sum(victory == TRUE, na.rm = TRUE),
    win_rate = victories / total_runs
  )

## Step 10: Create scatter plot of win rates by character by ascension level (Reddit Data) ====
plot_ascension_level <- ggplot(win_rate_by_character_by_ascension_level, aes(x = ascension_level, y = win_rate, color = character_chosen)) +
  geom_point(size = 3) +
  geom_smooth(method = "loess", se = FALSE) +  # Add a trend line
  scale_color_manual(values = simple_character_colors) +  # Apply simple colors
  labs(title = "Win Rate by Ascension Level and Character",
       x = "Ascension Level",
       y = "Win Rate (%)") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

## Step 11: Calculate top 5 relics in winning runs for each character, excluding starting relics (Reddit Data) ====

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
plot_top_5_relics <- ggplot(top_5_relics_with_win_rate, aes(x = relics, y = count, fill = character_chosen)) +
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


## Step 12: Plot my deaths by character by floor reached ====

bryan_deaths_summary <- lapply(bryan_data$event, function(run) {
  data.frame(
    character_chosen = ifelse(!is.null(run$character_chosen), run$character_chosen, NA),
    killed_by = ifelse(!is.null(run$killed_by), run$killed_by, NA),
    floor_reached = ifelse(!is.null(run$floor_reached), run$floor_reached, NA),
    ascension_level = ifelse(!is.null(run$ascension_level), run$ascension_level, NA),
    victory = ifelse(!is.null(run$victory), run$victory, NA)
  )
})

# Combine the individual dataframes into one
bryan_deaths_summary_df <- do.call(rbind, bryan_deaths_summary)

# Plot the deaths by character by floor reached density plot

ggplot(bryan_deaths_summary_df, aes(x = floor_reached, fill = character_chosen)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = simple_character_colors) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "My Density Plot of Floors Reached by Character",
       x = "Floor Reached",
       y = "Density") +
  theme_minimal()

## Step 13: Plot Reddit deaths by character and floor reached ====

### Step 13.1: Filter relevant data
# Focus on deaths (victory == FALSE) before Act IV (floor_reached < 52)
reddit_data_deaths <- nested_df %>%
  filter(victory == FALSE & floor_reached < 52) %>%
  select(character_chosen, killed_by, floor_reached, ascension_level, victory)


### Step 13.2: Summarize killed_by data
# Count deaths by floor and killer
killed_by_summary <- reddit_data_deaths %>%
  group_by(floor_reached, killed_by) %>%
  summarise(count = n(), .groups = 'drop')  # Summarize deaths per floor


### Step 13.3: Identify top killers for each floor
# Select the most frequent killer per floor
top_killed_by <- killed_by_summary %>%
  group_by(floor_reached) %>%
  top_n(1, count)  # Keep only the most frequent killer per floor


### Step 13.4: Filter for key floors only
# Only include key floors for labeling: 1, 8, 16, 24, 33, 40, 50
key_floors <- c(1, 8, 16, 24, 33, 40, 50)
top_killed_by_filtered <- top_killed_by %>%
  filter(floor_reached %in% key_floors)


### Step 13.5: Calculate density peaks for key floors
# Extract density data and calculate max_y (density peaks) for each key floor
density_data <- ggplot_build(
  ggplot(reddit_data_deaths, aes(x = floor_reached, fill = character_chosen)) +
    geom_density(alpha = 0.5)
)$data[[1]]

density_peaks <- density_data %>%
  group_by(floor_reached = round(x)) %>%
  summarise(max_y = max(density)) %>%
  filter(floor_reached %in% key_floors)

# Merge max_y with top_killed_by_filtered
top_killed_by_filtered <- top_killed_by_filtered %>%
  left_join(density_peaks, by = "floor_reached")


### Step 13.6: Plot density chart with top_killed_by labels
# Plot density with the most frequent killers at the density peaks
plot_killed_by <- ggplot(reddit_data_deaths, aes(x = floor_reached, fill = character_chosen)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = simple_character_colors) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Reddit Density Plot of Floors Reached by Character",
       x = "Floor Reached",
       y = "Density") +
  theme_minimal() +
  geom_text(data = top_killed_by_filtered, aes(x = floor_reached, y = max_y + 0.02, label = killed_by),  
            vjust = -0.5, angle = 0, color = "black", size = 4, inherit.aes = FALSE)



## Step 14: Save plots as PNG files ====
output_dir <- "Slay_The_Spire/images"

# Create the output directory if it does not exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Print each plot
print(plot_win_rates)
print(plot_ascension_level)
print(plot_top_5_relics)
print(plot_killed_by)

# Save each plot
ggsave(filename = file.path(output_dir, "win_rates_by_character.png"), plot = plot_win_rates, width = 10, height = 6)
ggsave(filename = file.path(output_dir, "win_rate_by_ascension_level.png"), plot = plot_ascension_level, width = 10, height = 6)
ggsave(filename = file.path(output_dir, "top_5_relics_with_win_rates.png"), plot = plot_top_5_relics, width = 12, height = 8)
ggsave(filename = file.path(output_dir, "floors_reached_by_character_with_killed_by.png"), plot = plot_killed_by, width = 12, height = 8)