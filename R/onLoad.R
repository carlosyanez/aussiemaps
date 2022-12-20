.onLoad <- function(libname, pkgname){
 # packageStartupMessage("Loading {aussiemaps}")

  #default cache
  home       <- Sys.getenv("HOME")
  cache_path <- file.path(home, ".aussiemaps_cache")

  if(!dir.exists(cache_path))
    manage_maps_cache_dir(cache_path)

  # download latest version of available files in repo
  #packageStartupMessage("Checking available files online")

}
