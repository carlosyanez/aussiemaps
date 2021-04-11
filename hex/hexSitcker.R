# make sure that {librarian} and is there
if(!require(librarian)) install.packages("librarian")

librarian::shelf("tidyverse",
                 "here",
                 "ggtext",
                 "ggfx",
                 "hexSticker",
                 "carlosyanez/customthemes",
                 "carlosyanez/aussiemaps",
                 "sf",
)


all_aussie_map <-locations.table %>% load_map(aggregation="State")

p <- ggplot() +
    geom_sf(data=all_aussie_map,aes(geometry=geometry),fill="#D22631",colour="#000000") +
    custom_map_theme(legend_pos = "none") +
   scale_fill_ochre()

s <- sticker(p, package="aussiemaps", p_size=20, s_x=1, s_y=.75, s_width=1.3, s_height=1,
             h_fill="#FFCD00", h_color="#00843D",
             filename=here("hex","hexSticker.png"))

ggsave(s,here("hex","hexSticker.png"))
