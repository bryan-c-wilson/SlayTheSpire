library(shiny)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Slay the Spire: Win Rate Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("character", "Select Character:",
                  choices = c("DEFECT", "IRONCLAD", "THE_SILENT", "WATCHER"),
                  selected = "IRONCLAD", multiple = TRUE)
    ),
    mainPanel(
      plotOutput("winRatePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$winRatePlot <- renderPlot({
    # Filter data based on selected characters
    filtered_data <- combined_win_rates %>%
      filter(character_chosen %in% input$character)
    
    # Plot win rate by character
    ggplot(filtered_data, aes(x = character_chosen, y = win_rate, fill = source)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Win Rate by Character",
           x = "Character",
           y = "Win Rate (%)")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
