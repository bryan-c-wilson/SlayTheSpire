# Snippet that was used to help explore data. Particularly useful was print(colnames(nested_df))
#Which let me see what columns I'm working with when exploring complex JSON structures.

# Step 1: Inspect the structure of the first element
str(data$event[[1]])

# Step 2: Convert the list of nested data to a data frame
# Here, we assume that each event is structured similarly
nested_df <- bind_rows(data$event)

# Step 3: Display the column names and a summary of the nested data
cat("Column Names in Nested Data:\n")
print(colnames(nested_df))

cat("\nSummary of Nested Data:\n")
print(summary(nested_df))

cat("\nFirst Few Rows of Nested Data:\n")
print(head(nested_df))



#######################

