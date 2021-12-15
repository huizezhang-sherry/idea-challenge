library(tidyverse)
library(sf)

vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria") %>% st_transform(4326)

#############################
elev <- st_read(here::here("data-raw/VMELEV/EL_CONTOUR.shp"))
# some data in this shape file does not belong to victoria
# the transformation takes a while to run - save the data
vic_elev <- elev %>%
  st_transform(4326) %>%
  filter(s2::s2_within(geometry, vic_map))
#save(vic_elev, file = "data/vic_elev.rda")

#############################
# There are four different FTYPE_CODE: contour, contour_dep,
# contour_index, and contour_dep_index. Most observations are in category contour
# and contour_index.
# use contour_index for the analysis below


# The shape file gives very detailed contour line of the elevation (every 100m
# from 100 to 1900). There are still 38,545 rows after narrowing it down to
# contour_index category. Besides, the line strings are pretty long for each
# row and points on the linestring differs in 0.0001 degree lat/ long.
# These facts shows we have very rich data available for usage but great
# simplification need to be done before producing any meaningful plot.

# Select those linestrings that are long enough
# (contain more than 400 points), simplify the linestring by keeping 1% of the
# points and only plot contour line every 200m from 200 to 800m.
load(here::here("data/vic_elev.rda"))
contour <- vic_elev %>%
  filter(FTYPE_CODE == "contour_index")

find_good <- function(dt = contour, alt, threshold = 400, keep_ratio = 0.01){
  coords <- dt %>%
    filter(ALTITUDE == alt) %>%
    st_coordinates() %>%
    as_tibble()

  good <- coords %>%
    count(L1) %>%
    filter(n > threshold)

  good_rows <- coords %>%
    filter(L1 %in% good$L1) %>%
    pull(L1) %>%
    unique()

  dt %>%
    filter(ALTITUDE == alt) %>%
    filter(row_number() %in% good_rows) %>%
    rmapshaper::ms_simplify(keep = keep_ratio)
}

elev2468 <- map_dfr(seq(200, 800, 200), ~find_good(alt = .x))
#save(elev2468, file = "data/elev2468.rda")

ggplot() +
  geom_sf(data = vic_map, fill = "transparent") +
  geom_sf(data = out, aes(color = as.factor(ALTITUDE)), size = 0.2) +
  geom_point(data = wind_meta, aes(x = lon, y = lat)) +
  scale_color_brewer(palette = "Dark2", name = "altitude") +
  theme_bw()

ggsave(filename = here::here("figures/contour2468.png"))

