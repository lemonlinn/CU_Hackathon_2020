library(leaflet)
library(data.table)
library(tidyverse)
library(shiny)
library(htmltools)

setwd("/Users/greatyifan/Desktop/@Columbia/2020fall/CU 2020 Data Hackathon/Mobility Dataset")
WorldCountry <-geojsonio::geojson_read("./world_geo_json/countries.geo.json", what = "sp")
world_df <- fread('total_data_real_final_version.csv')
dim(world_df)
colnames(world_df)
world_df$date <- as.Date(world_df$date)

stringency_df <- world_df %>%
  group_by(country_name, longitude, latitude) %>%
  summarise(mean_stringency = mean(stringency_index)) 

# stringency_df %>%
#   leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(lng = ~longitude, lat = ~latitude, 
#              radius = ~ mean_stringency/10, 
#              stroke = FALSE, fillOpacity = 1)

base <- WorldCountry[WorldCountry$name %in% world_df$country_name, ]%>%
  leaflet() %>% 
  addTiles() %>% 
  addProviderTiles(provider = 'CartoDB.VoyagerNoLabels') 


bin1 <- c(0, 15, 55, 75, 100)
pal <- colorBin("YlOrRd", domain = world_df$stringency_index, bins = bin1)

base %>%
  addPolygons(stroke = 0) %>%
  addPolygons(stroke = 0,
              fillColor = ~ pal(world_df$stringency_index),
              fillOpacity = .5) %>%
  addLegend(pal = pal, 
            values = drop_na(world_df)$stringency_index, 
            title = 'Stringency')

world_df$school_closing <- round(world_df$school_closing, digits = 0)
# bins <- c(0, 1, 2, 3)
pal2 <- colorFactor("Purples", domain = world_df$school_closing)

# by(world_df$school_closing, world_df$country_name, tail, n = 1)

map_jan <- base %>% 
  addPolygons(stroke = 0) %>% 
  addPolygons(stroke = FALSE, smoothFactor = 0.5,
              weight=1, opacity=1,
              fillColor = ~pal2(world_df[date == '2020-01-01', ]$school_closing),
              fillOpacity = .8) %>%
  addLegend(pal = pal2, 
            values = world_df[date == '2020-01-01', ]$school_closing, 
            title = 'School Closing')

map_march <- base %>% 
  addPolygons(stroke = 0) %>% 
  addPolygons(stroke = FALSE, smoothFactor = 0.5,
              weight=1, opacity=1,
              fillColor = ~pal2(world_df[date == '2020-03-01', ]$school_closing),
              fillOpacity = .8) %>%
  addLegend(pal = pal2, 
            values = world_df[date == '2020-03-01', ]$school_closing, 
            title = 'School Closing')

map_may <- WorldCountry[WorldCountry$name %in% world_df$country_name, ]%>%
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(stroke = 0) %>% 
  addPolygons(stroke = FALSE, smoothFactor = 0.5,
              weight=1, opacity=1,
              fillColor = ~pal2(world_df[date == '2020-05-01', ]$school_closing),
              fillOpacity = .8) %>%
  addLegend(pal = pal2, 
            values = world_df[date == '2020-05-01', ]$school_closing, 
            title = 'School Closing')


map_jul <- WorldCountry[WorldCountry$name %in% world_df$country_name, ]%>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(stroke = 0) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.5,
              weight=1, opacity=1,
              fillColor = ~pal2(world_df[date == '2020-07-01', ]$school_closing),
              fillOpacity = .8) %>%
  addLegend(pal = pal2, 
            values = world_df[date == '2020-07-01', ]$school_closing, 
            title = 'School Closing')


map_sept <- WorldCountry[WorldCountry$name %in% world_df$country_name, ]%>%
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(stroke = 0) %>% 
  addPolygons(stroke = FALSE, smoothFactor = 0.5,
              weight=1, opacity=1,
              fillColor = ~pal2(world_df[date == '2020-09-01', ]$school_closing),
              fillOpacity = .8) %>%
  addLegend(pal = pal2, 
            values = world_df[date == '2020-09-01', ]$school_closing, 
            title = 'School Closing')


leaflet_grid <- 
  tagList(
    tags$table(width = "100%",
               tags$tr(
                 tags$td(map_march),
                 tags$td(map_may)
               ),
               tags$tr(
                 tags$td(map_jul),
                 tags$td(map_sept)
               )
    )
  )

browsable(leaflet_grid)

















