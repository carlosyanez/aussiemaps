# make sure that {librarian} and is there
if(!require(librarian)) install.packages("librarian")

librarian::shelf("tidyverse",
                 "here",
                 "ggtext",
                 "ggfx",
                 "hexSticker",
                 "carlosyanez/customthemes",
                 "ozmaps",                   # aussiemaps is very heavy, using ozmaps for logo! (thanks ozmaps!)
                 "sf",
)


#aussie_map <-locations.table %>% load_map(aggregation="State")
aussie_map <- ozmap_data("states")

p <- ggplot() +
    geom_sf(data=aussie_map,aes(geometry=geometry,fill=NAME)) +
    custom_map_theme(legend_pos = "none") +
    scale_fill_manual(values=c(rep("#D22631",3),rep("#000000",3),rep("#FFC72C",3)))

height <- 1.6

s <- sticker(p, package="aussiemaps", p_size=6, s_x=1, s_y=.75, s_width=1.3*height, s_height=height,
             h_color="#FFCD00", h_fill="#00843D",
             filename=here("hex","hexSticker.png"))


