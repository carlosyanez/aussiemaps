library(sf)
library(here)
library(fs)
library(stringr)

source(here("data-raw","aux_save.R"))

source(here("data-raw","functions.R"))
source(here("R","internal.R"))
files <- dir_ls(here("data-raw"),regexp = "gpkg$")

nsw <- files[str_detect(files,"Wales")]
nsw_map <- st_read(nsw)
splits <- 5
delimiters <- floor((1:splits/splits)* nrow(nsw_map))
delimiters  <- c(1,delimiters)

for(i in 1:splits){
  partial <- nsw_map[delimiters[i]:(delimiters[i+1]),]
  st_write(partial,(here("data-raw",glue::glue("2011_New.South.Wales.{i}.gpkg"))))

}
files <- files[str_detect(files,"Wales.gpkg",TRUE)]
for(file in files){
    fs::file_move(file,str_replace_all(file," ","\\."))
}

files2 <-files
files <- dir_ls(here("data-raw"),regexp = "gpkg$")
files <- files[str_detect(files,"New.South.Wales.gpkg",TRUE)]
files <- files[str_detect(files,"2011")]

for(file in files){
  save_zip_gpkg(file,here("data-raw"),here("data-raw","processed"))
  #save_zip_gpkg(file,find_maps_cache(),here("data-raw","processed"))

}
