library(piggyback)
library(here)
library(fs)
library(tidyverse)


files_dir      <- here("data-raw","processed")
repo           <- "carlosyanez/aussiemaps"
version       <- "data"


files <- tibble(path=dir_ls(files_dir),
                file=str_remove(path,str_c(files_dir,"/"))) |>
         mutate(file_mod=str_replace_all(file," ","."))


set.seed(123)
files <- slice_sample(files,prop=1)
#create new release
#pb_new_release(repo,version)

group_size <- round(nrow(files)/6,0)

steps <- seq(1,nrow(files),by=round(nrow(files)/group_size,0))

# upload catalogue items ---
for(file in files$path){
  message(file)
  #files_list <- files[steps[i-1]:steps[i],]
  pb_upload(file,repo,version)
  #tryCatch(pb_upload(file=file,repo,version),
  #         error=function(e){print(e)})
  message("next")
  #Sys.sleep(60)
}


today <- today |> filter(str_detect(file_name,"2021")) |>
  filter(lubridate::date(timestamp)!=lubridate::today())

files <- files |>
         filter(file_mod %in% today$file_name)

for(file in files$path){
  message(file)
  #files_list <- files[steps[i-1]:steps[i],]
  pb_upload(file,repo,version)
  #tryCatch(pb_upload(file=file,repo,version),
  #         error=function(e){print(e)})
  message("next")
  Sys.sleep(60)
}
