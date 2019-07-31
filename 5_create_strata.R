library(here)
library(sf)
library(tidyverse)
library(tigris)
# 1. SUBURBS
# Load data
strata <- read.csv(here("outputs", "ind_vars.csv")) %>%
  select(GEOID, pct_coll, pop_dens, bl_pov, ta_dens, road_dens) %>%
  mutate_at(vars(matches("GEOID")), as.factor) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  drop_na()
all_shp <- st_read(here("outputs", "./act_peds.shp")) %>%
  select(GEOID, e_wlk_den) %>%
  mutate(cty = str_sub(GEOID, 3, 5)) %>%
  st_transform(26918)
sch_public <- st_read(here("proprietary", "sch_public.shp")) %>%
  filter(!(HIGHESTGRA %in% c("Prekindergarten",
                             "9th Grade",
                             "10th Grade",
                             "11th Grade",
                             "12th Grade"))) %>%
  select(SCHOOLNAME) %>%
  st_transform(26918)

# fc <- st_read(here("proprietary", "func_class.shp")) %>%
#   filter(FUNC_CLS == "14" | FUNC_CLS == "16") %>% # only major and minor arterials
#   filter(SIDE_IND == "1") %>% # only one side of road
#   st_transform(26918) %>%
#   rownames_to_column(.) %>%
#   mutate(leng = as.numeric(st_length(.))) %>%
#   filter(leng > 152.4) %>%
#   select(geometry)
# Functional class shapefile is masked to buffered sidewalks already
fc <- st_read(here("proprietary", "func_class_cut.shp")) %>%
  mutate(leng = as.numeric(st_length(.))) %>%
    filter(leng > 152.4) %>%
    select(geometry)
# For non-transit only: remove buffered bus lines
bus_mask <- st_read(here("proprietary", "all_lines_union.shp")) %>%
  st_transform(., 26918) %>%
  st_buffer(., 15.24) %>%
  st_union(.)

# Make strata
sub <- strata %>%
  filter(stcty %in% c("42017", "42029", "42045", "42091")) %>%
  mutate_if(is.numeric, funs(ifelse(. > median(.), 1, 0))) %>%
  mutate(stratum = paste(pop_dens, pct_coll, road_dens, sep = ".")) %>%
  select(GEOID, stratum)
sub_shp <- inner_join(all_shp, sub) %>%
  st_transform(., 26918)
sub_mask <- st_union(sub_shp)

# 1a. Schools
# Use this one for sampling
sch_stratum <- st_intersection(sch_public, sub_shp) %>%
  st_buffer(., 402.336) %>% # 0.25 mi buffer
  left_join(., sub) %>%
  select(GEOID, SCHOOLNAME, stratum)
# Use this one for masking from tract strata
sch_mask <- st_union(sch_stratum)

# 1b. Transit arterial strata (presence or absence of transit stops)
fc_seg <- st_segmentize(fc$geometry, 402.336) %>%
  st_cast(., "MULTIPOINT")
new_fc_seg <- NULL
for(l in 1:length(fc_seg)){
  segment <- fc_seg[[l]]
  segment_points <- st_cast(x = st_sfc(segment), to = "POINT")
  qual <- st_distance(segment_points)
  qual <- qual[1,]
  k <- 1
  drop_vals <- NULL
  keep_vals <- 1
  while (k < length(qual)) {
    i <- k
    j <- k + 1
    screen <- 0
    while (screen < 201.168) {
      if (j == length(qual)){
        break
      }
      screen <- qual[j] - qual[i]
      drop_vals <- c(drop_vals, j)
      j <- j + 1
    }
    keep_vals <- c(keep_vals, j)
    k <- j
  }
  if(!(is_empty(drop_vals))){
    new_segment_points <- segment_points[-drop_vals]
  } else {
    new_segment_points <- segment_points
  }
  temp <- st_sf(geometry = new_segment_points) %>%
    rownames_to_column(.) %>%
    mutate(SEGID = l) %>%
    rename(POINTID = rowname)
  new_fc_seg <- rbind(new_fc_seg, temp)
}
new_fc_seg <- new_fc_seg %>%
  st_set_crs(26918)
nhsl <- st_read(here("proprietary", "nhsl_stations.shp")) %>%
  st_transform(26918) %>%
  select(geometry)
bsl <- st_read(here("proprietary", "bsl_stations.shp")) %>%
  st_transform(26918) %>%
  select(geometry)
mfl <- st_read(here("proprietary", "mfl_stations.shp")) %>%
  st_transform(26918) %>%
  select(geometry)
bus <- st_read(here("proprietary", "bus_stops.shp")) %>%
  st_transform(26918) %>%
  select(geometry)
trolley <- st_read(here("proprietary", "trolley_stops.shp")) %>%
  st_transform(26918) %>%
  select(geometry)
riders <- rbind(nhsl, bsl) %>%
  rbind(., mfl) %>%
  rbind(., bus) %>%
  rbind(., trolley)
# st_buffer 0.25 mi
riders_buff <- st_buffer(riders, 402.336)
riders_buff$ID <- 1:nrow(riders_buff)
# Intersect riders_buff and arterials
# If new_total and OBJECTID rows identical,
# This means a blob of stops intersects an arterial segment
# Remove these duplicates.
# FOR BURBS: var `present` indicates if arterial has transit
ridership_by_arterial <- st_intersection(new_fc_seg, riders_buff) %>%
  distinct(., SEGID, POINTID, .keep_all = TRUE) %>%
  mutate(present = 1)
final_arterial <- left_join(new_fc_seg, ridership_by_arterial %>% st_set_geometry(NULL),
                            by = c("SEGID" = "SEGID", "POINTID" = "POINTID")) %>%
  replace_na(list(present = 0))
# Voronoi polygons transform sample points to areal
vor <- new_fc_seg %>%
  st_geometry(.) %>%
  st_union(.) %>%
  st_voronoi(.) %>%
  st_collection_extract(type = "POLYGON") %>%
  st_sf(crs = 26918)
# Voronoi polygons don't preserve order or ID so spatial intersect with final_arterial
# (aka point centroids of voronoi)
ridership_presence <- NULL
vor_key <- st_contains(vor, final_arterial)
for (i in 1:nrow(vor_key)){
  targets <- unlist(vor_key[[i]])
  values <- final_arterial %>%
    slice(targets) %>%
    summarize(presence = max(present))
  ridership_presence <- c(ridership_presence, values$presence)
}
vor <- vor %>%
  rownames_to_column(.) %>%
  rename(ID = rowname) %>%
  mutate_at(vars(ID), as.numeric) %>%
  mutate(present = ridership_presence)
# Chop up feature class shapefile by Voronoi polygons
# Use this one for sampling
fc <- st_transform(fc, st_crs(vor))
sub_shp <- st_transform(sub_shp, st_crs(vor))
fc_assign <- st_intersection(fc, vor) %>%
  st_intersection(., sub_shp) %>%
  select(ID, GEOID, present, stratum)
# Use this one for masking from tract strata
fc_mask <- st_union(fc_assign) %>%
  st_buffer(., 15.24)
# Sometimes segments cross tract boundaries. Drop these.
# Hacky way to pull IDs with more than 1 record
drop_idx <- fc_assign %>%
  group_by(ID) %>%
  slice(., 2) %>%
  pull(ID)
fc_assign <- fc_assign %>%
  filter(!(ID %in% drop_idx)) %>%
  mutate(lens = as.numeric(st_length(.))) %>% # ADDITIONAL FILTER: MINIMUM DISTANCE
  filter(lens >= 76.2)
# Also mask schools from transit arterials
fc_assign <- fc_assign %>%
  st_difference(., sch_mask)

# 1c. Areal sampling strata
sub_shp <- sub_shp %>%
  st_difference(., sch_mask) %>%
  st_difference(fc_mask)

# We are going to have to split transit and non-transit arterials
# into two and mask bus lines only from non-transit arterials
fc_non_trans <- fc_assign %>%
  filter(present == 0) %>%
  st_difference(., bus_mask) %>%
  distinct(ID, .keep_all = TRUE)
fc_trans <- fc_assign %>%
  filter(present == 1) %>%
  distinct(ID, .keep_all = TRUE)

# 1d. Export
st_write(sch_stratum, here("outputs", "sub_school_stratum.shp"), delete_dsn = TRUE)
st_write(fc_trans, here("outputs", "sub_transit_arterial_strata.shp"), delete_dsn = TRUE)
st_write(fc_non_trans, here("outputs", "sub_non_transit_arterial_strata.shp"), delete_dsn = TRUE)
st_write(sub_shp, here("outputs", "sub_strata.shp"), delete_dsn = TRUE)

# 2. PHILADELPHIA
# Load base and mask files
strata <- read_csv(here("outputs", "ind_vars.csv")) %>%
  select(GEOID, ta_dens, pct_coll) %>%
  mutate_at(vars(matches("GEOID")), as.factor) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  drop_na()
all_shp <- st_read(here("outputs", "./act_peds.shp")) %>%
  select(GEOID, e_wlk_den) %>%
  mutate(cty = str_sub(GEOID, 3, 5)) %>%
  st_transform(26918)
phila <- strata %>%
  filter(stcty == "42101") %>%
  mutate_if(is.numeric, funs(ifelse(. > median(.), 1, 0))) %>%
  mutate(stratum = paste(pct_coll, ta_dens, sep = ".")) %>%
  select(GEOID, stratum)
phila_shp <- inner_join(all_shp, phila)
phila_mask <- st_union(phila_shp) %>%
  st_set_crs(26918)

# Prep roads by segment
roads <- st_read(here("proprietary", "Data_for_DVRPC", "STREET_CENTERLINE.shp")) %>%
  filter(!(CLASS %in% c(1, 5, 9, 10, 12, 14, 15))) %>%
  mutate(ROADID = 1:nrow(.)) %>%
  st_transform(., 26918)

# Load transit stops and stations
# Merge into single file
bsl_stations <- st_read(here("proprietary", "bsl_stations.shp")) %>%
  mutate(weekday_total = Average_We) %>%
  st_transform(., st_crs(phila_mask)) %>%
  select(weekday_total)
mfl_stations <- st_read(here("proprietary", "mfl_stations.shp")) %>%
  mutate(weekday_total = Average_We) %>%
  st_transform(., st_crs(phila_mask)) %>%
  select(weekday_total)
bus_stations <- st_read(here("proprietary", "bus_stops.shp")) %>%
  st_transform(., st_crs(phila_mask)) %>%
  mutate(weekday_total = WEEKDAY_BO + WEEKDAY_LE) %>%
  select(weekday_total)
trolley_stations <- st_read(here("proprietary", "trolley_stops.shp")) %>%
  st_transform(., st_crs(phila_mask)) %>%
  mutate(weekday_total = WEEKDAY_BO + WEEKDAY_LE) %>%
  select(weekday_total)
# PATCO
patco_flat <- read_csv(here("proprietary", "PATCO_point_rider.csv")) %>%
  filter(!(Station %in% c("Ashland Parking Lot",
                          "Ferry Avenue Parking Lot",
                          "Haddonfield  Parking Lot",
                          "Lindenwold Parking Lot",
                          "Westmont Parking Lot",
                          "Woodcrest Parking Lot"))) %>%
  group_by(Station) %>%
  summarize(weekday_total = sum(Total) / length(unique(TransDay)))
# Geocode PATCO
patco_stations <- tribble(~Station, ~Y, ~X,
                          "12th-13th", 39.948002, -75.162374,
                          "15th-16th", 39.948631, -75.167813,
                          "8th-Market", 39.951000, -75.153540,
                          "9th-10th", 39.947369, -75.157519,
                          "Ashland", 39.858703, -75.009226,
                          "Broadway", 39.943153, -75.120367,
                          "City Hall", 39.945495, -75.121266,
                          "Collingswood", 39.913588, -75.064559,
                          "Ferry Avenue", 39.922593, -75.091795,
                          "Haddonfield", 39.897558, -75.037082,
                          "Lindenwold", 39.834474, -75.001221,
                          "Westmont", 39.907144, -75.046713,
                          "Woodcrest", 39.870109, -75.011536) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>%
  st_transform(26918) %>%
  left_join(., patco_flat) %>%
  select(-Station)
station <- rbind(bsl_stations,
                 mfl_stations) %>%
  rbind(., bus_stations) %>%
  rbind(., trolley_stations) %>%
  rbind(., patco_stations) %>%
  st_intersection(., phila_mask) %>%
  rownames_to_column(.) %>%
  mutate_at(vars(rowname), as.numeric) %>%
  rename(STOPID = rowname)

# Point-in-polygon ridership in station areas
median(roads$Shape_STLe) # 246.7 feet!
station_buff_threshold <- 500 * 0.3048 # 500 feet in meters

station_buff <- st_buffer(station, station_buff_threshold) %>%
  select(STOPID) %>%
  rename(STOPID_buff = STOPID)
station_intersection <- st_intersection(station, station_buff)
station_collapse <- station_intersection %>%
  st_set_geometry(NULL) %>%
  group_by(STOPID_buff) %>%
  summarize(ridership_potential = sum(weekday_total))
station_potential <- left_join(station, station_collapse, by = c("STOPID" = "STOPID_buff"))

# Overlay station level potential ridership with road segments
roads_buff_threshold <- 50 * 0.3048 # 50 feet in meters
roads_buff <- st_buffer(roads, roads_buff_threshold) %>%
  select(ROADID)
roads_intersection <- st_intersection(station_potential, roads_buff)
roads_collapse <- roads_intersection %>%
  st_set_geometry(NULL) %>%
  group_by(ROADID) %>%
  summarize(mean_rider = mean(ridership_potential, na.rm = TRUE))
roads_potential <- left_join(roads, roads_collapse) %>%
  drop_na(mean_rider) %>%
  mutate(ab_med = ifelse(mean_rider > median(mean_rider), 1, 0)) %>%
  select(ROADID, ST_NAME, ab_med, mean_rider)

# Overlay road segments with arterials
roads_potential <- st_intersection(roads_potential, phila_shp)

# 2b. Export
st_write(phila_shp, here("outputs", "phila_strata.shp"), delete_dsn = TRUE)
st_write(roads_potential, here("outputs", "phila_transit_arterial_strata.shp"), delete_dsn = TRUE)
