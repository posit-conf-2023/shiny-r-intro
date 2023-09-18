library(tidyverse)
library(shiny)
d = readr::read_csv(here::here("data/weather.csv"))

shinyApp(
  ui = fluidPage(
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "city", "Select a city",
          choices = c("Chicago", "Durham", "Sedona", "New York", "Los Angeles")
        ),
        selectInput(
          "var", "Select a variable",
          choices = NULL
          # choices = d_vars, selected = "temp"
        )
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax")
      )
    )
  ),
  server = function(input, output, session) {
    
    d_city = reactive({
      d |>
        filter(city %in% input$city)
    })
    
    observe({
      updateSelectInput(inputId = "var", choices = d_vars())
    })
    
    d_vars = reactive({
      d_city() |>
        select(where(is.numeric)) |>
        select(where(function(x) var(x) != 0)) %>%
        names()
    })
    
    output$plot = renderPlot({
      req(input$var)
      
      d_city() |>
        ggplot(aes(x=time, y=.data[[input$var]], color=city)) +
        ggtitle(input$var) +
        geom_line()
    })
    
    output$minmax = renderTable({
      req(input$var)
      
      d_city() |>
        mutate(
          day = lubridate::wday(time, label = TRUE, abbr = FALSE),
          date = as.character(lubridate::date(time))
        ) |>
        group_by(date, day) |>
        summarize(
          `min` = min(.data[[input$var]]),
          `max` = max(.data[[input$var]]),
          .groups = "drop"
        )
    })
  }
)
