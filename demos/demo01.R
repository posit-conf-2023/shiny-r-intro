library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))
d_other_thing <- 

shinyApp(
  ui = fluidPage(
    titlePanel("Temperature Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = c("Chicago", "Durham", "Sedona", "New York", "Los Angeles", "San Antonio")
        ) 
      ),
      mainPanel( 
        plotOutput("plot")
      )
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
