library(cubble)
library(tidyverse)
library(patchwork)
load(here::here("data/wind_data.rda"))
load(here::here("data/wind_meta.rda"))
load(here::here("data/elev2468.rda"))
vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")

############################
# distance list and pair-wise distance
d <- wind_meta %>%
  select(usaf, lon, lat) %>%
  filter(usaf %in% unique(wind_data$usaf)) %>%
  filter(!usaf %in% c("948890"))

station_dist <- crossing(from = d$usaf, to = d$usaf) %>%
  left_join(d, by = c("from" = "usaf")) %>%
  rename(lon_from = lon, lat_from = lat) %>%
  left_join(d, by = c("to" = "usaf")) %>%
  rename(lon_to = lon, lat_to = lat) %>%
  rowwise() %>%
  mutate(d = geosphere::distHaversine(c(lon_from, lat_from), c(lon_to, lat_to)) /1000) %>%
  filter(d < 100) %>%
  ungroup() %>%
  select(from, to, d)


############################
# useful functions
# calculate_corr: compile the pairwise correlation and distance dataframe
# input: a wide form dataframe with each date in a row and each station in a column
# output: a data frame with column station from, station to, correlation between the pair and its distance
calculate_corr <- function(dt) {
  meta_column <- colnames(dt)[is.na(as.numeric(colnames(dt)))]
  correlation <- cor(dt %>% select(-all_of(meta_column)),
                     use = "pairwise.complete.obs")

  out <- correlation %>%
    as_tibble() %>%
    mutate(from = d$usaf) %>%
    pivot_longer(-from, names_to = "to", values_to = "corr") %>%
    mutate(to = as.numeric(to)) %>%
    left_join(station_dist %>% select(from, to, d), by = c("from", "to")) %>%
    filter(!is.na(d), corr != 1)

  out
}

# get_gippsland: extract the station pair in the east gippsland region
# input: a pair-wise correlation-distance data frame from calculate_corr
# ouput: a subset of the input that contains pairs where both from and to are in the gippsland region
get_gippsland <- function(dt){
  gstations <- wind_meta %>% arrange(-lon) %>% head(7) %>% pull(usaf)
  gippsland <- dt %>% filter(from %in% gstations, to %in% gstations)

  gippsland

}

# get_mallee: extract the station pair in the mallee region
# input/ output: see get_gippsland
get_mallee <- function(dt){
  mstations <- wind_meta %>% filter(lon < 144, lat > -37) %>% pull(usaf)
  mallee <- dt %>% filter(from %in% mstations, to %in% mstations)

  mallee
}

#############################
# summarise into hourly measure to deal with records that are not in full hour and duplicates
clean <- wind_data %>%
  filter(!usaf %in% c("948890")) %>%
  mutate(h = lubridate::hour(time), day = as.Date(time)) %>%
  group_by(usaf, day, h) %>%
  summarise(speed = mean(speed)) %>%
  ungroup() %>%
  pivot_wider(names_from = usaf, values_from = speed)

dt <- calculate_corr(clean)
# negative correlation are dropped in the subsequent analysis
neg <- dt %>% filter(corr < 0)
pos <- dt %>% filter(corr >= 0)
gippsland <- get_gippsland(pos)
mallee <- get_mallee(pos)

p1 <- ggplot(pos, aes(x = d, y = corr)) +
  geom_point() +
  geom_point(data = gippsland, color = "red") +
  geom_point(data = mallee, color = "orange") +
  geom_smooth(se = FALSE) +
  theme_bw()

p2 <- ggplot() +
  geom_sf(data = vic_map, fill = "transparent") +
  # this line would be useful to see the station label on the map
  #ggrepel::geom_label_repel(data = wind_meta, aes(x = lon, y = lat, label = usaf)) +
  geom_sf(data = elev2468,
          aes(color = as.factor(ALTITUDE)),
          alpha = 0.3, size = 0.2) +
  geom_point(data = wind_meta, aes(x = lon, y = lat), color = "grey30") +
  geom_point(data = wind_meta %>% filter(usaf %in% gippsland$from),
             aes(x = lon, y = lat), color = "red") +
  geom_point(data = wind_meta %>% filter(usaf %in% mallee$from),
             aes(x = lon, y = lat), color = "orange") +
  scale_color_brewer(palette = "Dark2", name = "altitude") +
  theme_bw()

(p1 | p2) + plot_layout(guides = "collect") & theme(legend.position = "bottom")
ggsave(filename = here::here("figures/cor-with-map.png"),
       width = 15, height = 10)

################################################################
# tod: time of day
tod <- clean %>% mutate(
  tod = case_when(
    between(h, 0, 6) ~ "early morning",
    between(h, 7, 12) ~ "morning",
    between(h, 13, 18) ~ "afternoon",
    between(h, 18, 23) ~ "night"
  ),
  tod = factor(tod, levels = c(
    "early morning", "morning", "afternoon", "night"
  ))
) %>%
  group_split(tod)


res <- map_dfr(tod, calculate_corr, .id = "tod") %>%
  mutate(
    tod = case_when(
      tod == 1 ~ "early morning",
      tod == 2 ~ "morning",
      tod == 3 ~ "afternoon",
      tod == 4 ~ "night"
    ),
    tod = factor(tod, levels = c(
      "early morning", "morning", "afternoon", "night"
    ))
  )

pos <- res %>% filter(corr >= 0)
gippsland <- get_gippsland(pos)
mallee <- get_mallee(pos)

ggplot(pos, aes(x = d, y = corr)) +
  geom_point(alpha = 0.1) +
  geom_point(data = gippsland, alpha = 0.5, color = "red") +
  geom_point(data = mallee, alpha = 0.5, color = "orange") +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(tod)) +
  coord_cartesian(ylim = c(0, 1))

ggsave(filename = here::here("figures/cor-by-tod.png"),
       width = 10, height = 7)

################################################################
# month
m <- clean %>%
  mutate(month = lubridate::month(day)) %>%
  group_split(month)


res <- map_dfr(m, calculate_corr, .id = "month") %>%
  mutate(month = as.numeric(month),
         month = as.numeric(case_when(
           month == 4 ~ 10,
           month == 5 ~ 11,
           month == 6 ~ 12,
           TRUE ~ month
         )))

pos <- res %>% filter(corr >= 0)
gippsland <- get_gippsland(pos)
mallee <- get_mallee(pos)

ggplot(pos, aes(x = d, y = corr)) +
  geom_point(alpha = 0.1) +
  geom_point(data = gippsland, alpha = 0.5, color = "red") +
  geom_point(data = mallee, alpha = 0.5, color = "orange") +
  geom_smooth(se = FALSE) +
  facet_wrap(vars(month)) +
  coord_cartesian(ylim = c(0, 1))

ggsave(filename = here::here("figures/cor-by-month.png"),
       width = 10, height = 7)
