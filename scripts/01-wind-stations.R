library(tidyverse)
library(cubble)

#############################
# The wind data from ISD is obtained using the `rnoaa` package. The data is
# stored in my `weatherdata` package, which collects a series of large
# Australian climate data. The script that process this data can be found at
# https://raw.githubusercontent.com/huizezhang-sherry/weatherdata/master/data-raw/wind.R
wind_meta <- weatherdata::wind %>%
  as_tibble() %>%
  select(-ts)

wind_data <- weatherdata::wind %>% stretch() %>% as_tibble()
save(wind_meta, file = "data/wind_meta.rda")
save(wind_data, file = "data/wind_data.rda")

plot_map(vic_map) +
  geom_point(data = wind_meta, aes(x = lon, y = lat))
ggsave(filename = "figures/wind_stations.png")
#############################
# Reconcile the ISD stations with the list of station Carolyn provided.
# The plot shows that all the ISD station has a match with Carolyn's source.
wind_stations <- weatherdata::wind %>%
  select(usaf: elev) %>%
  as_tibble()

locations <- read_csv(here::here("data-raw/wind-locations-carolyn.csv"))

vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")

combined <- wind_stations %>%
  select(usaf, lat, lon) %>%
  rename(name = usaf) %>%
  mutate(source = "ISD") %>%
  bind_rows(locations %>%
              select(Number, Latitude, Longitude) %>%
              rename(name = Number, lat = Latitude, lon = Longitude) %>%
              mutate(source = "New"))

plot_map(vic_map) +
  geom_point(data = combined, aes(x = lon, y = lat, color = source)) +
  scale_color_brewer(palette = "Dark2")

########################################################################
# More details:
# Find the closest match from one set to the other to look at the mis-match
# All the ISD station has a match while a few stations from Carolyn
# don't have a match in ISD. These stations don't seem to appear in the
# NOAA FTP server: `rnoaa::isd_stations()`
dist <- wind_stations %>%
  rowwise() %>%
  mutate(new_loc = list(as_tibble(locations))) %>%
  unnest(new_loc) %>%
  mutate(dist = rnoaa::meteo_spherical_distance(lat1 = lat, long1 = lon,
                                                lat2 = Latitude,
                                                long2 = Longitude)) %>%
  group_by(usaf) %>%
  filter(dist == min(dist)) %>%
  select(usaf, lat, lon, Number, Latitude, Longitude, dist) %>%
  ungroup() %>%
  mutate(match = ifelse(dist > 10, FALSE, TRUE))

isd_unmatched <- dist %>% filter(!match) %>% select(lon, lat) %>% mutate(source = "ISD")

dist2 <- locations %>%
  rowwise() %>%
  mutate(new_loc = list(as_tibble(wind_stations))) %>%
  unnest(new_loc) %>%
  mutate(dist = rnoaa::meteo_spherical_distance(lat1 = lat, long1 = lon,
                                                lat2 = Latitude,
                                                long2 = Longitude)) %>%
  group_by(Number) %>%
  filter(dist == min(dist)) %>%
  select(usaf, lat, lon, Number, Latitude, Longitude, dist) %>%
  ungroup() %>%
  mutate(match = ifelse(dist > 10, FALSE, TRUE))

new_unmatched <- dist2 %>% filter(!match) %>%  select(Longitude, Latitude) %>% rename(lon = Longitude, lat = Latitude)  %>% mutate(source =  "New")

unmatched <- bind_rows(isd_unmatched, new_unmatched)

plot_map(vic_map) +
  geom_point(data = unmatched, aes(x = lon, y = lat, color = source)) +
  scale_color_brewer(palette = "Dark2")

