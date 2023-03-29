library(piggyback)
library(here)
library(fs)
library(tidyverse)


files_dir      <- here("data-raw","processed")
repo           <- "carlosyanez/aussiemaps"
version       <- "data"


files <- tibble(file=dir_ls(files_dir),
                     Year=str_remove(file,str_c(files_dir,"/")))


set.seed(123)
files <- slice_sample(files,prop=1)
#create new release
#pb_new_release(repo,version)

group_size <- round(nrow(files)/6,0)

steps <- seq(1,nrow(files),by=round(nrow(files)/group_size,0))

# upload catalogue items ---
for(file in files$file){
  message(file)
  #files_list <- files[steps[i-1]:steps[i],]
  pb_upload(file,repo,version)
  #tryCatch(pb_upload(file=file,repo,version),
  #         error=function(e){print(e)})
  message("next")
  Sys.sleep(60)
}


