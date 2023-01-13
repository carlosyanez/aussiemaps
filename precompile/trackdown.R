### This file intends to be a shortcut to install and use {trackdown}, an R package to use Google Docs to edit the "narrative" part of a RMD file
### {trackdown} can be downloaded from CRAN
### This file is not intended to be run as one piece!
library(tidyverse)
library(fs)
library(here)

## Settings #####
gdrive_path <- "trackdown"              # the folder on google drive


# Executive Summary ####
## Commands to run #####

#upload for first time

files <- fs::dir_info(here("precompile")) %>% filter(str_detect(path,"\\.Rmd")) %>% pull(path)

for(file in files)
  trackdown::upload_file(file, gpath="aussiemaps",hide_code=TRUE)


#sync RMD back with google
for(file in files)
  trackdown::download_file(file,gpath="aussiemaps")




