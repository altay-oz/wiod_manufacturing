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
[��ó] [R] Shiny package�� �� �� �׷��� ����|�ۼ��� ��