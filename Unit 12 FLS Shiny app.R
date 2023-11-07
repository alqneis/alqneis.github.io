library(shiny)
library(ggplot2)
library(dplyr)

#loadin data
beer_data <- Beers_1

# UI interface part
ui <- fluidPage(
  titlePanel("Beer Data Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Beer Data (CSV file)"),
      radioButtons("plot_type", "Select Plot Type:",
                   choices = c("Histogram", "Boxplot")),
      selectInput("state_filter", "Filter by State:",
                  choices = c("All", unique(beer_data$state))),
      checkboxInput("add_regression_line", "Add Regression Line", value = FALSE)
    ),
    
    mainPanel(
      plotOutput("ibu_abv_plot"),
      plotOutput("additional_plot")
    )
  )
)

# Server part work
server <- function(input, output, session) {
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  plot_data <- reactive({
    data_filtered <- data()
    if (input$state_filter != "All") {
      data_filtered <- data_filtered %>% filter(state == input$state_filter)
    }
    data_filtered
  })
  
  output$ibu_abv_plot <- renderPlot({
    plot_type <- input$plot_type
    p <- ggplot(plot_data(), aes(x = IBU, y = ABV)) +
      geom_point() +
      labs(x = "IBU", y = "ABV") +
      theme_minimal()
    
    if (input$add_regression_line) {
      p <- p + geom_smooth(method = "lm", se = FALSE)
    }
    
    if (plot_type == "Boxplot") {
      p <- p + geom_boxplot(aes(x = "Boxplot"))
    } else {
      p <- p + geom_histogram(aes(x = "Histogram"), bins = 30)
    }
    
    p
  })
  
  output$additional_plot <- renderPlot({
    # Create additional plots here
    # Replace this with your specific analysis and visualization
    p <- ggplot(plot_data(), aes(x = your_x_variable, y = your_y_variable)) +
      geom_your_plot_type() +
      labs(x = "X-axis Label", y = "Y-axis Label") +
      theme_minimal()
    
    p
  })
}

# Running the app or apps
shinyApp(ui, server)