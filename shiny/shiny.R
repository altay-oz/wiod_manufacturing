install.packages('shiny')
library(shiny)

# 웹에서 어떻게 보여줄 것인지 결정하는 ui.R 파일과 
# 정리된 값( 그래프(plot, graph)나 표 를) 띄울 수 있도록 결정해주는 server.R 파일

ui<-fluidPage(
  sliderInput(inputId="num_1",
              label="숫자를 고르세요",
              value=25, min=1, max=80, step=1),
  plotOutput("hist")
)
server<-function(input, output) {
  output$hist <- renderPlot({
    title <- "80 random normal values "
    hist(rnorm(input$num_1),main=title)
  })
}
shinyApp(ui=ui, server=server)


mapCountryData(sPDF
               ,nameColumnToPlot='eigen.cent')