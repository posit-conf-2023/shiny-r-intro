library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = c("Chicago", "Durham", "Sedona", "New York", "Los Angeles")
        ),
        checkboxInput("forecast", "Highlight forecasted data", value = FALSE)
      ),
      mainPanel( plotOutput("plot") )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      
      d_city = filter(d, city %in% input$city)
      
      if (input$forecast) {
        ggplot(d_city, aes(x=time, y=temp, color=source)) +
          geom_line() +
          scale_color_manual(values = c("red","black"))
      } else {
        ggplot(d_city, aes(x=time, y=temp), color=1) +
          geom_line()
      }
    })
  }
)
