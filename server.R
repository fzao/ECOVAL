library(shiny)
library(plotly)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$Test <- renderPlotly({
    x <- c(1:100)
    random_y <- rnorm(100, mean = 0)
    data <- data.frame(x, random_y)
    p <- plot_ly(data, x = ~x, y = ~random_y, type = 'scatter', mode = 'lines')
  })
})

# test