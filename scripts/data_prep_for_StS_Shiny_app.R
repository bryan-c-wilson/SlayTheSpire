library(dplyr)
library(tidyr)

# Load the Reddit data (november.rds)
reddit_data <- readRDS("/Users/bryan/Documents/R_Projects/Slay_The_Spire/data/november.rds")

# Unnest the nested data in the `event` column
unnested_data <- reddit_data %>%
  unnest_wider(event)

# Ensure key columns exist
required_cols <- c("floor_reached", "killed_by", "ascension_level", "character_chosen")
missing_cols <- setdiff(required_cols, colnames(unnested_data))
if (length(missing_cols) > 0) {
  stop(paste("Error: Missing columns in the data:", paste(missing_cols, collapse = ", ")))
}

# 1. PREP FOR FLOOR REACHED DENSITY PLOT
# Filter floors below 50 (victory floor)
floor_reached_data <- unnested_data %>%
  filter(floor_reached < 51) %>%  # Only keep floors less than 51
  select(character_chosen, floor_reached, ascension_level)  # Columns needed for density plot

# 2. PREP FOR KILLED_BY ENEMY TABLE WITH FLOOR TYPE GROUPING
# Add floor_type for bosses and non-boss floors
deaths_with_act <- unnested_data %>%
  filter(floor_reached < 51) %>%
  mutate(floor_type = case_when(
    floor_reached < 16 ~ "Act I Non-Boss",
    floor_reached == 16 ~ "Act I Boss",
    floor_reached < 33 ~ "Act II Non-Boss",
    floor_reached == 33 ~ "Act II Boss",
    floor_reached < 50 ~ "Act III Non-Boss",
    floor_reached == 50 ~ "Act III Boss",
    TRUE ~ "Unknown"
  )) %>%
  select(character_chosen, floor_reached, killed_by, floor_type, ascension_level)  # Columns for table

# 3. Save both prepped datasets for use in the Shiny app
# Save for floor reached density plot
saveRDS(floor_reached_data, "Slay_The_Spire/Slay_The_Spire_Exploration/floor_reached_data.rds")

# Save for killed_by enemy table with floor types
saveRDS(deaths_with_act, "Slay_The_Spire/Slay_The_Spire_Exploration/deaths_with_act.rds")


