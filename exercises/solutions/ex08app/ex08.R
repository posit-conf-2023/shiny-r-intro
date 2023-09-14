library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

d_vars = d |>
  select(where(is.numeric)) |>
  names()

shinyApp(
  ui = fluidPage(
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "region", "Select a region",
          choices = sort(unique(d$region))
        ),
        selectInput(
          "city", "Select a city",
          choices = c(),
          multiple = TRUE
        ),
        selectInput(
          "var", "Select a variable",
          choices = d_vars, selected = "temp"
        )
      ),
      mainPanel( 
        plotOutput("plot")
      )
    )
  ),
  server = function(input, output, session) {
    
    d_city = reactive({
      req(input$city)
      d |>
        filter(city %in% input$city)
    })
    
    observe({
      cities = d |>
        filter(region == input$region) |>
        pull(city) |>
        unique() |>
        sort()
      
      updateSelectInput(
        inputId = "city", 
        choices = cities
      )
    })
    
    output$plot = renderPlot({
      d_city() |>
        ggplot(aes(x=time, y=.data[[input$var]], color=city)) +
        ggtitle(input$var) +
        geom_line()
    })
  }
)
