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
                 "png",
                 "magick",                   #for twitter card
                 "dmi3kno/bunny"             #helper for magick

)


#aussie_map <-locations.table %>% load_map(aggregation="State")
aussie_map <- ozmap_data("states")

p <- ggplot() +
    geom_sf(data=aussie_map,aes(geometry=geometry,fill=NAME),colour=NA,size=0) +
    custom_map_theme(legend_pos = "none") +
    scale_fill_manual(values=c(rep("#D22631",3),rep("#000000",3),rep("#FFC72C",3)))

height <- 1.6

s <- sticker(p, package="aussiemaps", p_size=6, s_x=1, s_y=.75, s_width=1.3*height, s_height=height,
             h_color="#FFCD00", h_fill="#00843D",
             filename=here("img","hexSticker.png"))

#https://www.ddrive.no/post/making-hex-and-twittercard-with-bunny-and-magick/


img_hex_gh <- image_read(here("img","hexSticker.png")) %>%
  image_scale("400x400")

# https://www.pngfind.com/download/hobbwx_github-clipart-github-logo-github-hd-png-download/
gh_logo <- image_read(here("hex","gh.png")) %>%
  image_scale("50x50")

bg_colour <-"#a86030"

gh <- image_canvas_ghcard(bg_colour) %>%
  image_compose(img_hex_gh, gravity = "East", offset = "+100+0") %>%
  image_annotate("aussiemaps", gravity = "West", location = "+100-30",
                 color="white", size=60, font="Aller", weight = 700) %>%
  image_compose(gh_logo, gravity="West", offset = "+100+40") %>%
  image_annotate("carlosyanez/aussiemaps", gravity="West", location="+160+45",
                 size=40, font="Ubuntu Mono",color="white") %>%
  image_border_ghcard(bg_colour)

gh %>%
  image_write(here::here("img", "bbox_ghcard.png"))


