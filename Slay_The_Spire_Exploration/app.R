library(shiny)
library(dplyr)
library(ggplot2)

# Load the prepared data
deaths_with_act <- readRDS("deaths_with_act.rds")

# Define custom colors for characters
simple_character_colors <- c(
  "DEFECT" = "blue",
  "IRONCLAD" = "red",
  "THE_SILENT" = "green",
  "WATCHER" = "purple"
)

ui <- fluidPage(
  titlePanel("Slay the Spire Deaths Analysis"),
  sidebarLayout(
    sidebarPanel(
      checkboxInput("select_all_characters", "Select All Characters", value = FALSE),
      selectInput("character_chosen", "Choose Character", 
                  choices = unique(deaths_with_act$character_chosen), 
                  multiple = TRUE, 
                  selected = unique(deaths_with_act$character_chosen)[1]),
      checkboxInput("select_all_ascensions", "Select All Ascensions", value = FALSE),
      sliderInput("ascension_level", "Choose Ascension Level", 
                  min = 0, max = 20, value = 1)
    ),
    mainPanel(
      plotOutput("floorDensityPlot", height = "500px"),
      tableOutput("killedByTable")
    )
  )
)

server <- function(input, output, session) {
  
  # Update the character selection if "Select All" is checked
  observe({
    if (input$select_all_characters) {
      updateSelectInput(session, "character_chosen", selected = unique(deaths_with_act$character_chosen))
    }
  })
  
  # Update the ascension selection if "Select All" is checked
  observe({
    if (input$select_all_ascensions) {
      updateSliderInput(session, "ascension_level", value = 0:20)
    }
  })
  
  # Filtered data for the density plot based on character and ascension level
  filtered_density_data <- reactive({
    deaths_with_act %>%
      filter((character_chosen %in% input$character_chosen | input$select_all_characters),
             (ascension_level == input$ascension_level | input$select_all_ascensions))
  })
  
  # Output the density plot of floor reached
  output$floorDensityPlot <- renderPlot({
    ggplot(filtered_density_data(), aes(x = floor_reached, fill = character_chosen)) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = simple_character_colors) +  # Apply custom colors
      labs(title = paste("Death Density Plot for", paste(input$character_chosen, collapse = ", ")),
           x = "Floor Reached", y = "Density") +
      theme_minimal()
  })
  
  # Filtered data for the killed by enemy table
  filtered_killed_by <- reactive({
    deaths_with_act %>%
      filter((character_chosen %in% input$character_chosen | input$select_all_characters),
             (ascension_level == input$ascension_level | input$select_all_ascensions))
  })
  
  # Output the killed by enemy table, sorted by Act and floor type
  output$killedByTable <- renderTable({
    filtered_killed_by() %>%
      count(floor_type, killed_by, sort = TRUE) %>%
      filter(!is.na(killed_by)) %>%  # Ensure no NAs
      mutate(floor_type = factor(floor_type, levels = c(
        "Act I Non-Boss", "Act I Boss", 
        "Act II Non-Boss", "Act II Boss", 
        "Act III Non-Boss", "Act III Boss"
      ))) %>%  # Ensure custom order of floor_type
      arrange(floor_type)  # Sort the table by floor_type
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
