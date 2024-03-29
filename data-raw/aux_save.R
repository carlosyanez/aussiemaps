save_zip_parquet <- function(df,name,dest_dir){

  zipname     <- str_c(name,".zip")
  parquetname <- str_c(name,".parquet")

  write_parquet(df,path(dest_dir,parquetname),compression="brotli")
  zip::zip(zipfile=path(dest_dir,zipname),
           files=path(dest_dir,parquetname),
           mode = "cherry-pick")
  fs::file_delete(path(dest_dir,parquetname))

}

save_zip_gpkg <- function(file,source_dir,dest_dir){

  if(!dir_exists(source_dir)) stop("source dir does not exist")
  if(!dir_exists(dest_dir)) stop("dest dir does not exist")

 file_zip  <- str_remove(file,source_dir) %>%
      str_remove(.,"/") %>%
      str_remove(.,".gpkg") %>%
      str_replace_all(.," ","\\.") %>%
      str_c(.,".zip")

  zip::zip(zipfile=path(dest_dir,file_zip),
           files=file,
           mode = "cherry-pick")

}
