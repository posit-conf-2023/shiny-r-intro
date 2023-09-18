library(tidyverse)
library(shiny)
library(bslib)

d = readr::read_csv(here::here("data/weather.csv"))

d_vars = d |>
  select(where(is.numeric)) |>
  names()

thematic::thematic_shiny(bg = "auto", fg = "auto", font = "auto")

shinyApp(
  ui = fluidPage(
    theme = bslib::bs_theme(
      bootswatch = "minty",
      base_font = font_google("Quicksand"),
      fg = "purple",
      bg = "white",
      primary = "steelblue",
      heading_font = font_google("Bitter"),
      "border-width" = "4px"
    ),
    titlePanel("Weather Forecasts"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "city", "Select a city",
          choices = unique(d$city),
          multiple = TRUE
        ),
        selectInput(
          "var", "Select a variable",
          choices = d_vars, selected = "temp"
        )
      ),
      mainPanel( 
        plotOutput("plot"),
        
        actionButton("b1", "primary",   class = "btn-primary"),
        actionButton("b2", "secondary", class = "btn-secondary"),
        actionButton("b3", "success",   class = "btn-success"),
        actionButton("b4", "info",      class = "btn-info"),
        actionButton("b5", "warning",   class = "btn-warning"),
        actionButton("b6", "danger",    class = "btn-danger")
      )
    )
  ),
  server = function(input, output, session) {
    bslib::bs_themer()
    
    d_city = reactive({
      req(input$city)
      d |>
        filter(city %in% input$city)
    })
    
    output$plot = renderPlot({
      d_city() |>
        ggplot(aes(x=time, y=.data[[input$var]], color=city)) +
        ggtitle(input$var) +
        geom_line()
    })
  }
)
