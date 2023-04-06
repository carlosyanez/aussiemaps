library(sf)
library(here)

source(here("data-raw","aux_save.R"))

source(here("data-raw","functions.R"))
source(here("R","internal.R"))

nsw <- st_read(here("data-raw","2021_New South Wales.gpkg"))

half_way <- floor(3*nrow(nsw)/4)

nsw1 <- nsw[1:half_way,]
nsw2 <- nsw[(half_way+1):nrow(nsw),]

st_write(nsw1,here("data-raw","2021_New South Wales 1.gpkg"))
st_write(nsw2,here("data-raw","2021_New South Wales 2.gpkg"))

save_zip_gpkg(here("data-raw","2021_New South Wales 1.gpkg"),
              here("data-raw"),
              here("data-raw","processed"))
save_zip_gpkg(here("data-raw","2021_New South Wales 2.gpkg"),
              here("data-raw"),
              here("data-raw","processed"))
