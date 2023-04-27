library(aussiemaps)
library(leaflet)
library(sf)
library(fs)
library(tidyverse)

nt <-get_map(filters=list(STATE_NAME_2021="Northern Territory"),
             aggregation = "LGA_NAME_2021",
             fill_holes = TRUE,
             year=2021)
nt$id <- 1:nrow(nt)

nt <- nt |> st_cast("POLYGON")
nt <- nt |> st_make_valid()
nt_c <-st_covers(nt)
nt_c[4]
l <-c()
for(i in 1:length(nt_c)){
  if(length(nt_c[[i]])>1) l <-c(l,i)

}

for(i in l){
  
  diff <- nt[i,] 
  
  small <- nt[nt_c[[i]][nt_c[[i]]!=i],]
  for(j in 1:nrow(small)){
    diff <- st_difference(diff,small[j,]) 
    diff <- st_make_valid(diff)
    diff$area <- st_area(diff)
    diff <- diff |> filter(area==max(area))
  
  }
  
  diff <- diff |> select(any_of(colnames(nt)))
  
  nt <- nt |>
    mutate(rn=row_number()) |>
    filter(rn!=i)           |>
    select(-any_of("rn"))     |>
    bind_rows(diff)
 
}

nt <- 
  nt |>
  group_by(LGA_NAME_2021) |>
  summarise(geom=st_union())


nt |>
  leaflet() |>
  addTiles() |>
  addPolygons(label=~id, fillColor = "blue")
