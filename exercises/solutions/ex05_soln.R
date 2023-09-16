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
        fileInput("upload", "Upload a file", accept = ".csv"),
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
    
    d = reactive({
      req(input$upload)
      readr::read_csv(input$upload$datapath)
    })
    
    d_city = reactive({
      req(input$city)
      d() |>
        filter(city %in% input$city)
    })
    
    d_vars = reactive({
      d() %>%
        select(where(is.numeric)) %>%
        names()
    })
    
    observe({
      updateSelectInput(
        inputId = "var", 
        choices = d_vars(), selected = d_vars()[1]
      )
    })
    
    observe({
      updateSelectInput(
        inputId = "city", 
        choices = unique(d()$city)
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
