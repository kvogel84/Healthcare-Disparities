library(shiny)
library(ggplot2)
library(plotly)
library(bslib)
library(shinycssloaders)

# Define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$style(HTML("
    .center-content {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100vh; /* Full viewport height */
    }
    .center-panel {
      text-align: center;
      width: 60%; /* Adjust width as needed */
    }
  ")),
  
  div(
    class = "center-content",
    div(
      class = "center-panel",
      titlePanel("ER Visit Statistics Based on Age, Race/Ethnicity, and Income"),
      
      selectInput(
        inputId = "year",
        label = "Select a Year:",
        choices = c(2019, 2020, 2021),
        selected = 2019
      ),
      
      tabsetPanel(
        tabPanel("Risk by Age", plotlyOutput("risk_by_age") %>% withSpinner()),
        tabPanel("Risk by Income", plotlyOutput("risk_by_income") %>% withSpinner()),
        tabPanel("Risk by Race/Ethnicity", plotlyOutput("risk_by_race_ethnicity") %>% withSpinner())
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Histogram for Age
  output$risk_by_age <- renderPlotly({
    data <- switch(as.character(input$year),
                   "2019" = ER2019_age1,
                   "2020" = ER2020_age1,
                   "2021" = ER2021_age1)
    p <- ggplot(data, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(
        title = paste("Risk by Age (", input$year, ")", sep = ""),
        x = "Age of Patient", y = "ER Visits per 100,000 Population"
      ) +
      theme_classic()
    ggplotly(p)
  })
  
  # Histogram for Income
  output$risk_by_income <- renderPlotly({
    data <- switch(as.character(input$year),
                   "2019" = ER2019_income1,
                   "2020" = ER2020_income1,
                   "2021" = ER2021_income1)
    p <- ggplot(data, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      labs(
        title = paste("Risk by Income (", input$year, ")", sep = ""),
        x = "Income Level", y = "ER Visits per 100,000 Population"
      ) +
      theme_classic()
    ggplotly(p)
  })
  
  # Histogram for Race/Ethnicity
  output$risk_by_race_ethnicity <- renderPlotly({
    data <- switch(as.character(input$year),
                   "2019" = ER2019_race1,
                   "2020" = ER2020_race1,
                   "2021" = ER2021_race1)
    p <- ggplot(data, aes(x = `Characteristic Levels`, y = `Measure Values`)) +
      geom_bar(stat = "identity", fill = "pink") +
      labs(
        title = paste("Risk by Race/Ethnicity (", input$year, ")", sep = ""),
        x = "Race/Ethnicity", y = "ER Visits per 100,000 Population"
      ) +
      theme_classic()
    ggplotly(p)
  })
}

# Run the App
shinyApp(ui = ui, server = server)


