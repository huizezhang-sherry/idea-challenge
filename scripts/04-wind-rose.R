library(tidyverse)
load(here::here("data/wind_data.rda"))
load(here::here("data/wind_meta.rda"))

# Some stations have half hourly data, some have duplicate records at the same time point
# Also aggregate the direction to 20 degree since 36 (306/ 10) bins are too many in the wind rose.
wind <- wind_data %>%
  filter(lubridate::minute(time) == 0) %>%
  group_by(usaf, time) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(dir2 = ceiling(direction/20) * 20) %>%
  left_join(wind_meta %>% select(usaf, lon, lat))

#############################
# Create wind rose for each location as single glyph,  and inset the image into Victoria map
# Individual glyph counts the wind direction from "2019-10-01" to "2020-03-31".
# Round the location to 0.4 long/ lat to avoid glyph overlapping.
single_windrose <- function(usaf_id){
  dt <- wind %>%
    filter(usaf == usaf_id)

  dt %>%
    ggplot(aes(x = dir2)) +
    geom_bar() +
    coord_polar(start = -pi/18) +
    scale_x_continuous(breaks = seq(0, 360, 20)) +
    theme_bw() +
    theme(
      legend.position = "none",
      aspect.ratio = 1,
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      plot.margin=grid::unit(c(0,0,0,0), "mm")) +
    labs(x="", y="")

  ggsave(filename = here::here(glue::glue("figures/wind-rose/{usaf_id}.png")),
         width = 5, height = 5, unit = "cm")
}

#purrr::walk(wind_meta$usaf, single_windrose)

wind_rose <- tibble::tibble(
  img = glue::glue("figures/wind-rose/{wind_meta$usaf}.png"),
  usaf = wind_meta$usaf,
  lon = wind_meta$lon,
  lat = wind_meta$lat) %>%
  mutate(lon = round(lon/0.4) * 0.4,
         lat = round(lat/0.4) * 0.4)

#############################
vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")
bbox <- tibble::tibble(lat = -37, long = (147.5 + 150) /2)
ggplot() +
  ggimg::geom_point_img(data = wind_rose, aes(x = lon, y = lat, img = img), size = 1) +
  geom_sf(data = vic_map, fill = "transparent",
          linetype = "dashed", color = "lightgrey", size = 0.3) +
  geom_rect(data = bbox, aes(xmin = long - 1.35, xmax = long + 1.25,
                             ymin = lat - 1.2, ymax = lat + 1.2),
            fill = "transparent", color = "black", linetype = "dashed") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm"))

# ggsave(filename = here::here("figures/wind-rose.png"),
#        width = 25, height = 15, unit = "cm", dpi = 320)

###############################################
# Plot the count of each wind direction by month for the eastern stations
# no particular month difference observed
east_most <- wind_meta %>% arrange(-lon) %>% head(7)

dt <- wind %>%
  filter(usaf %in% east_most$usaf) %>%
  mutate(month = lubridate::month(time))

p1 <- dt %>%
  ggplot(aes(x = dir2)) +
  geom_bar() +
  #coord_polar(start = -pi / 18) +
  scale_x_continuous(breaks = seq(0, 360, 20)) +
  facet_grid(month ~ usaf) +
  theme_bw() +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.margin = grid::unit(c(0, 0, 0, 0), "mm")
  ) +
  labs(x = "", y = "")

p2 <- ggplot() +
  geom_sf(data = vic_map, fill = "transparent") +
  geom_point(data = east_most, aes(x = lon, y = lat)) +
  ggrepel::geom_label_repel(data = east_most,
                            aes(x = lon, y = lat, label = usaf), nudge_x = 0.1) +
  theme_void() +
  coord_sf(xlim = c(147.5, 150))

library(patchwork)
(p1| p2) + plot_layout(width = c(3,1))

