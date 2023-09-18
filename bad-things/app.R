library(shiny)

shinyApp(
  ui = fluidPage(
    numericInput("n", "n", 0),
    actionButton("add_one", "Add one")
  ),
  server = function(input, output, session) {
    observeEvent(input$add_one, {
      updateNumericInput(inputId = "n", value = input$n + 1)
    })
  }
)