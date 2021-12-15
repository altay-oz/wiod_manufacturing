library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  output$Plot <- renderPlot({
    plot(diamonds$carat, diamonds$price)
  })
})