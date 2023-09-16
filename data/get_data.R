library(tidyverse)
library(httr2)

get_weather_data = function(
    lat, long, 
    from = Sys.Date()-5,
    to = Sys.Date()+2,
    api_key = Sys.getenv("VISUALCROSSING_KEY")
) {
  req = request(
    "https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline/"
  ) |>
    req_url_path_append(
      paste0(lat,",",long),
      as.character(from), 
      as.character(to)
    ) |>
    req_url_query(
      key = api_key,
      include = "hours"
    )
  #print(req)
    
  
  resp = req |> 
    req_perform()
  #print(resp)
  
  d = resp_body_json(resp)
  
  tz = d$timezone
  
  days = tibble(days = d$days) |>
    unnest_wider(days) |> 
    mutate(
      date = as.Date(datetime),
      sunrise = as_datetime(paste(datetime, sunrise),tz=tz),
      sunset  = as_datetime(paste(datetime, sunset),tz=tz)
    ) |>
    relocate(date) |>
    select(-ends_with("Epoch"), -stations, -preciptype, -datetime)
  
  hours = days |>
    select(date, hours) |>
    unnest_longer(hours) |>
    unnest_wider(hours) |>
    mutate(
      time = as_datetime(paste(date, datetime),tz=tz),
    ) |>
    relocate(time) |>
    select(-ends_with("Epoch"), -stations, -preciptype, -date, -datetime)
  
  list(
    days = days |> select(-hours),
    hours = hours
  )
}


get_city_weather = function(city, state, region, lat, long) {
  get_weather_data(lat, long) |>
    map(
      function(d) {
        d |> 
          mutate(city = city, state = state, region = region) |>
          relocate(city, state, region)
      }
    )
}




cities = readr::read_csv(here::here("data/cities.csv")) |>
  pmap(purrr::safely(get_city_weather))

daily = map_dfr(cities, c("result","days"))
hourly = map_dfr(cities, c("result","hours"))

#readr::write_csv(daily, here::here("data/daily_weather.csv"))
readr::write_csv(hourly, here::here("data/weather.csv"))

hourly |>
  filter(city == "Sedona") |>
  readr::write_csv(here::here("data/sedona.csv"))

hourly |>
  filter(city == "Chicago") |>
  readr::write_csv(here::here("data/chicago.csv"))
