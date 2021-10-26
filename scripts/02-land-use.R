library(tidyverse)
library(sf)

vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria") %>% st_transform(4326)

#############################
vic <- st_read(here::here("data-raw/CATCHMENTS/LANDUSE_2017.shp"))
vic_landuse <- vic %>% st_transform(4326)
#save(vic_landuse, file = "data/vic_landuse.rda")

#############################
# Similar to the elevation data in the next script, this data is also huge.
load(here::here("data/vic_landuse.rda"))

class123 <- vic_landuse %>%
  filter(HECTARES > 4) %>%
  select(LGA, ALUMV8, geometry) %>%
  mutate(level1 = str_sub(ALUMV8, 1, 1),
         level2 = str_sub(ALUMV8, 1, 2),
         level3 = str_sub(ALUMV8, 1, 3)) %>%
  filter(level1 %in% 1:3)

simplified <- class123 %>%
  # 113: national park
  # 220: commercial production from native forests
  # 311: plantation forests: hardwood plantation forestry
  # 312: plantation forests: softwood plantation forestry
  # 320: grazing modified pastures
  # 330: cropping, take 330 as representative of 331, 334, and 338
  filter(level3 %in% c(113, 220, 311, 312, 320, 330)) %>%
  mutate(level3 = ifelse(level3 %in% c(311, 312), 310, level3))

ggplot() +
  geom_sf(data = vic_map, fill = "lightgrey") +
  geom_sf(data = simplified, aes(fill = level3, color = level3)) +
  theme_bw() +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

ggsave(filename = "figures/landuse.png")
