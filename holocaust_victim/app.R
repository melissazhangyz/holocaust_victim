#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)
library(tidyverse)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("Number of Victims in Holocaust"),
  
  # Sidebar layout with a simple input and output
  sidebarLayout(
    sidebarPanel(      
      selectInput("rr",
                  label = "Select Religion:",
                  choices = unique(analysis_data$Religion),
                  selected = unique(analysis_data$Religion)[1])
    ),
    mainPanel(
      # Output: Display the interactive plot
      plotOutput("deathPlot"),
      DTOutput("deathTable")
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  analysis_data <- read_csv("cleaned.csv")
  
  # Filter data based on selected religion
  filtered_data <- 
    reactive({
    analysis_data %>% 
      filter(Religion == input$rr)
  })
  
  output$deathPlot <- renderPlot({
    filtered_data() %>%
      group_by(Death_Year_Month) %>%
      summarise(Deaths = n()) %>%
      ggplot(aes(x = Death_Year_Month, y = Deaths)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Monthly Deaths for", input$rr),
           x = "Month of Death",
           y = "Number of Deaths") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels for better readability
  })
  output$deathTable <- renderDT({
    filtered_data()
  }, options = list(pageLength = 10)) # Set initial number of rows to display
}

# Run the application 
shinyApp(ui = ui, server = server)
