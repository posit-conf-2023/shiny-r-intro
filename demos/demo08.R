library(tidyverse)
library(shiny)

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        fileInput("upload", "Upload a file")
      ),
      mainPanel( 
        h3("Result"),
        tableOutput("result"),
        h3("Content:"),
        tableOutput("data")
      )
    )
  ),
  server = function(input, output, session) {
    output$result = renderTable({
      input$upload
    })
    
    output$data = renderTable({
      req(input$upload)
      ext = tools::file_ext(input$upload$datapath)
      validate(need(ext == "csv", "Please upload a csv file"))
      readr::read_csv(input$upload$datapath)
    })
  }
)
