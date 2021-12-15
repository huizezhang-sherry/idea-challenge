library(tidyverse)
load(here::here("data/wind_data.rda"))
load(here::here("data/wind_meta.rda"))

# Some stations have half hourly data, some have duplicate records at the same time point
# Also aggregate the direction to 20 degree since 36 (306/ 10) bins are too many in the wind rose.
stations <- wind_meta %>% arrange(-lon) %>% head(7)
vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")
wind <- wind_data %>%
  filter(usaf %in% stations$usaf) %>%
  filter(lubridate::minute(time) == 0) %>%
  group_by(usaf, time) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  left_join(wind_meta %>% select(usaf, lon, lat))

# https://www.cfa.vic.gov.au/warnings-restrictions/total-fire-bans-and-ratings/history-of-tfbs
fire_wind <- wind %>%
  filter(lubridate::date(time) == as.Date("2019-11-21")) %>%
  mutate(
    # for geom_spoke: The angles start from east and increase counterclockwise.
    dir_to = (direction + 180) %% 360,
    dir_spoke = ((360 - dir_to + 90)/180 * pi))

ggplot(data = fire_wind) +
  geom_spoke(aes(x = lon, y = lat, angle = dir_spoke,
                 group = usaf,
                 radius = scales::rescale(speed, c(0.1, .2))),
             arrow = arrow(length = unit(0.05, "inches"))) +
  geom_point(data = stations, aes(x = lon, y = lat)) +
  geom_sf(data = vic_map, fill = "transparent") +
  theme_bw() +
  coord_sf(xlim = c(147.5, 150), ylim = c(-38.2, -35.8)) +
  gganimate::transition_states(time) +
  labs(title = "Time: {next_state}")

gganimate::anim_save(filename = here::here("figures/wind-dir-11-21-19.gif"),
                     height = 10, width = 10)

###################
ggplot(data = fire_wind) +
  geom_spoke(aes(x = lon, y = lat, angle = dir_spoke,
                 group = usaf,
                 radius = scales::rescale(speed, c(0.1, 0.3))),
             arrow = arrow(length = unit(0.05, "inches"))) +
  geom_point(data = stations, aes(x = lon, y = lat), size = 0.5) +
  geom_point(data = ~.x %>% filter(usaf == "949080", lubridate::hour(time) %in% c(10, 11)),
             aes(x = lon, y = lat), color = "red") +
  geom_sf(data = vic_map, linetype = "dotted", color = "grey20", fill = "transparent") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        #panel.border = element_blank(),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  coord_sf(xlim = c(147.5, 150), ylim = c(-38.2, -35.8)) +
  scale_x_continuous(breaks = c(148, 149, 150)) +
  facet_wrap(vars(lubridate::hour(time)), ncol = 4) +
  labs(x = "Longitude", y = "Latitude")

ggsave(filename = here::here("figures/wind-dir-11-21-19-facets.png"), height = 10, width = 7)


###################
all_vic <- wind_data %>%
  filter(lubridate::minute(time) == 0) %>%
  group_by(usaf, time) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(dir2 = ceiling(direction/20) * 20) %>%
  left_join(wind_meta %>% select(usaf, lon, lat)) %>%
  filter(lubridate::date(time) == as.Date("2019-11-21"))

p <- ggplot(data = all_vic) +
  geom_spoke(aes(x = lon, y = lat, angle = direction,
                 group = usaf,
                 radius = scales::rescale(speed, c(0.1, .2))),
             arrow = arrow(length = unit(0.05, "inches"))) +
  geom_point(aes(x = lon, y = lat)) +
  geom_sf(data = vic_map, fill = "transparent") +
  theme_bw() +
  gganimate::transition_states(time) +
  labs(title = "Time: {next_state}")



###################
# looks like there's a data quality issue
weatherdata::wind %>%
  cubble::stretch() %>%
  filter(usaf %in% stations$usaf) %>%
  mutate(day = as.Date(time)) %>%
  as_tibble() %>%
  group_by(day)  %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  ggplot(aes(x = day, y = n)) +
  geom_line()
