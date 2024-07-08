setwd("C:/Users/micha/Downloads")
install.packages("shinylive")
library(shinylive)
library(shiny)
library(pwr)
library(ggplot2)

# Define UI for application
ui <- fluidPage(
  titlePanel("Sample Size Calculator for Correlation"),
  sidebarLayout(
    sidebarPanel(
      numericInput("correlation", "Expected Correlation Coefficient:", value = 0.5, min = 0, max = 1, step = 0.01),
      numericInput("power", "Desired Power (e.g., 0.80):", value = 0.80, min = 0, max = 1, step = 0.01),
      numericInput("alpha", "Significance Level (e.g., 0.05):", value = 0.05, min = 0, max = 1, step = 0.01),
      actionButton("calculate", "Calculate Sample Size")
    ),
    mainPanel(
      textOutput("result"),
      plotOutput("powerPlot")
    )
  )
)

# Define server logic required to calculate sample size and render plot
server <- function(input, output) {
  observeEvent(input$calculate, {
    correlation <- input$correlation
    power <- input$power
    alpha <- input$alpha
    
    result <- pwr.r.test(r = correlation, power = power, sig.level = alpha)
    
    output$result <- renderText({
      paste("Required Sample Size:", ceiling(result$n))
    })
    
    output$powerPlot <- renderPlot({
      sample_sizes <- seq(5, 50, by = 1)
      power_values <- sapply(sample_sizes, function(n) {
        pwr.r.test(n = n, r = correlation, sig.level = alpha)$power
      })
      
      plot_data <- data.frame(
        sample_size = sample_sizes,
        power = power_values
      )
      
      ggplot(plot_data, aes(x = sample_size, y = power)) +
        geom_line(color = "red") +
        geom_point() +
        geom_vline(xintercept = ceiling(result$n), linetype = "dashed", color = "blue") +
        annotate("text", x = ceiling(result$n), y = 0.1, label = paste("optimal sample size\nn =", ceiling(result$n)), color = "blue", angle = 90, vjust = -0.5) +
        labs(
          title = "Proportion Power Calculation for Binomial Distribution (Arcsine Transformation)",
          x = "Sample Size",
          y = "Test Power = 1 - β"
        ) +
        theme_minimal()
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shinylive)

shinylive::export(appdir ="shinylive_test", output_dir="strona_test")
httpuv::runStaticServer("strona_test")
