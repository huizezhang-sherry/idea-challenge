library(sf)
library(osmdata)
vic_map <- ozmaps::abs_ste %>% filter(NAME == "Victoria")
bbox <- sf::st_bbox(c(xmin = 147.5, ymin = -38.2,
                      xmax =150, ymax = -35.8), crs = 4326) %>% sf::st_as_sfc()
###################
# This file query various components from the OpenStreetMap to block out
# areas that are invalid to place wind sensors. See analysis/2021-11-03.rmd
# for how these areas look like in the map

query_osm <- function(string){
  opq(bbox = c(147.5, -38.2, 150, -35.8)) %>%
    add_osm_features(features = c(string)) %>%
    osmdata_sf()
}

process_poly <- function(osm_obj){
  osm_obj$osm_polygons %>%
    st_make_valid() %>%
    filter(s2::s2_within(geometry, vic_map)) %>%
    st_crop(bbox)
}

process_multipoly <- function(osm_obj){
  osm_obj$osm_multipolygons %>%
    st_make_valid() %>%
    filter(s2::s2_within(geometry, vic_map)) %>%
    st_crop(bbox)
}

process_line <- function(osm_obj){
  osm_obj$osm_lines %>%
    st_make_valid() %>%
    filter(s2::s2_within(geometry, vic_map)) %>%
    st_crop(bbox)
}

#######
# Water & Lake

# natural/water: Any inland body of water,
# from natural such as a lake or pond to artificial like a moat or canal
osm_water <- query_osm("\"natural\" = \"water\"")
save(osm_water,  file = here::here("data/osm/osm_water.rda"))

water <- process_poly(osm_water)
save(water,  file = here::here("data/osm-processed/water.rda"))

# natural/wetland: A natural area subject to inundation or with waterlogged ground
osm_wl <- query_osm("\"natural\" = \"wetland\"")
save(osm_wl,  file = here::here("data/osm/osm_wl.rda"))

wetland <- process_poly(osm_wl)
wetland_multipoly <- process_multipoly(osm_wl)
save(wetland,  file = here::here("data/osm-processed/wetland.rda"))
save(wetland_multipoly,  file = here::here("data/osm-processed/wetland_multipoly.rda"))

# water/lake: A natural or semi-natural body of relatively still fresh or salt
# water which is surrounded by land
osm_lake <- query_osm("\"water\" = \"lake\"")
save(osm_lake,  file = here::here("data/osm/osm_lake.rda"))

lake <- process_poly(osm_lake)
lake_multipoly <- process_multipoly(osm_lake)
save(lake,  file = here::here("data/osm-processed/lake.rda"))
save(lake_multipoly,  file = here::here("data/osm-processed/lake_multipoly.rda"))

osm_ww <- query_osm("\"waterway\" ~ \".*\"")
save(osm_ww,  file = here::here("data/osm/osm_ww.rda"))

waterway <- process_line(osm_ww)
waterway_poly <- process_poly(osm_ww)
save(waterway,  file = here::here("data/osm-processed/waterway.rda"))
save(waterway_poly,  file = here::here("data/osm-processed/waterway_poly.rda"))

#######
# Natural Reserve

# no entry
# Boundary/national_park: Area of outstanding natural beauty,
# set aside for conservation and for recreation
osm_np <- query_osm("\"boundary\" = \"national_park\"")

# protected area covers slightly more area than nature reserve
# boundary/protected_area: Protected areas, such as for national parks,marine
# protection areas, heritage sites, wilderness, cultural assets and similar.
osm_pa <- query_osm("\"boundary\" = \"protected_area\"")
save(osm_pa,  file = here::here("data/osm/osm_pa.rda"))

protected_area <- process_poly(osm_pa)
protected_area_multipoly <- process_multipoly(osm_pa)
save(protected_area,  file = here::here("data/osm-processed/protected_area.rda"))
save(protected_area_multipoly,  file = here::here("data/osm-processed/protected_area_multipoly.rda"))

osm_nature_reserve <- query_osm("\"leisure\" = \"nature_reserve\"")
save(osm_nature_reserve,  file = here::here("data/osm/osm_nature_reserve.rda"))
nature_reserve <- process_poly(osm_nature_reserve)
nature_reserve_multipoly <- process_multipoly(osm_nature_reserve)
save(nature_reserve,  file = here::here("data/osm-processed/nature_reserve.rda"))
save(nature_reserve_multipoly,  file = here::here("data/osm-processed/nature_reserve_multipoly.rda"))


#######
# Highway

osm_hw <- query_osm("\"highway\" ~ \".*\"")
save(osm_hw,  file = here::here("data/osm/osm_hw.rda"))

hw <- process_line(osm_hw)
highway <- hw %>%
  filter(maxspeed >= 40,
         st_length(geometry) > units::set_units(1, "km")) %>%
  st_buffer(dist = units::set_units(100, "m"))
save(highway,  file = here::here("data/osm-processed/highway.rda"))

#######
# Wood, tree, srub, and glassland

# natural/wood: Tree-covered area (a 'forest' or 'wood')
# natural/tree: A single tree
feats <- c("\"natural\" = \"wood\"",
           "\"natural\" = \"tree\""
           )
osm_wt <- query_osm(feats)
save(osm_wt,  file = here::here("data/osm/osm_wt.rda"))

woodtree <- process_poly(osm_wt)
woodtree_multipoly <- process_multipoly(osm_wt)
save(woodtree,  file = here::here("data/osm-processed/woodtree.rda"))
save(woodtree_multipoly,  file = here::here("data/osm-processed/woodtree_multipoly.rda"))


# doesn't have much srub or glassland
feats <- c("\"natural\" = \"srub\"",
           "\"natural\" = \"grassland\"")
osm_sg <- query_osm(feats)


######################################
######################################
######################################
# some other material to clean up:

landuse <- opq(bbox = c(148, -38, 150, -37)) %>%
  add_osm_features(features = c(
    "\"landuse\" ~ \"forest\"",
    "\"landuse\" ~ \"farmland\""
  )) %>%
  osmdata_sf()

forest <- landuse$osm_polygons %>%
  filter(s2::s2_within(geometry, vic_map))

######################################
load(here::here("data/vic_elev.rda"))
contour_raw <- vic_elev %>%
  filter(FTYPE_CODE == "contour_index") %>%
  filter(s2::s2_within(geometry, bbox))

contour <- map_dfr(200, ~find_good(dt = contour_raw, alt = .x, keep_ratio = 0.7))

