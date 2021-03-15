library(aussiemaps)
library(customthemes)
library(tidyverse)
library(sf)
library(leaflet)

filtert<- locations.table %>%
          filter(LGA %in% c("Albury","Wodonga")) %>%
          select(LOCALITY,LGA,State) %>% unique(.)

sydney <- load_aussie_map(filtert,aggregation=c("LOCALITY","State"))

sydney %>% ggplot() + geom_sf(aes(colour=State,fill=LOCALITY)) + custom_map_theme()

leaflet(sydney) %>%
  addProviderTiles("CartoDB.Positron")  %>%
  addPolygons()

locations.table %>% filter(LOCALITY=="Bondi")

###Problem on Wodonga - Killara and Gateway Island
### Adelaida
