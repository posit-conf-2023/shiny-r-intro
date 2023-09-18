library(tidyverse)
library(shiny)
library(bslib)
library(thematic)

d = readr::read_csv(here::here("data/weather.csv"))
d_vars = d |>
  select(where(is.numeric)) |>
  names()


thematic_shiny(bg = "auto", fg = "auto", font = "auto")

shinyApp(
  ui = fluidPage(
    theme = bs_theme(
      bootswatch = "darkly"
    ),
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
        ),
        actionButton("download_modal", "Download...")
      ),
      mainPanel( 
        plotOutput("plot"),
        tableOutput("minmax")
      )
    )
  ),
  server = function(input, output, session) {
    
    observe({
      user_selected <- input$dl_vars
      if (is.null(user_selected)) {
        user_selected <- names(d)
      }
      
      showModal(
        modalDialog(
          title = "Download data",
          checkboxGroupInput(
            "dl_vars", "Select variables to download",
            choices = names(d), selected = user_selected, inline = TRUE
          ),
          actionButton("select_none", "Deselect all"),
          actionButton("select_all", "Select all"),
          footer = list(
            downloadButton("download"),
            modalButton("Cancel")
          )
        )
      )
    }) |>
      bindEvent(input$download_modal)
    
    observeEvent(input$select_none, {
      updateCheckboxGroupInput(inputId = "dl_vars", selected = "")
    })
    
    observeEvent(input$select_all, {
      updateCheckboxGroupInput(inputId = "dl_vars", selected = names(d))
    })
    
    output$download = downloadHandler(
      filename = function() {
        paste0(
          paste(input$city,collapse="_"), 
          ".csv"
        )
      },
      content = function(file) {
        readr::write_csv(
          d_city() |>
            select(input$dl_vars), 
          file
        )
      }
    )
    
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
    
    output$minmax = renderTable({
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

