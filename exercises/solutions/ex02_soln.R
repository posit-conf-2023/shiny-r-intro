library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "city", "Select a city",
          choices = c("Chicago", "Durham", "Sedona", "New York", "Los Angeles"),
          multiple = TRUE, selected = "Chicago"
        ) 
      ),
      mainPanel( plotOutput("plot") )
    )
  ),
  server = function(input, output, session) {
    output$plot = renderPlot({
      d |>
        filter(city %in% input$city) |>
        ggplot(aes(x=time, y=temp, color=city)) +
        geom_line()
    })
  }
)
