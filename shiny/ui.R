library(shiny)

shinyUI(fluidPage(
  titlePanel("Test shiny"),
  
  sidebarLayout(
    sidebarPanel(
      helpText('carat and price Graph')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
    )
  )
))
[출처] [R] Shiny package로 웹 상에 그래프 띄우기|작성자 라랙