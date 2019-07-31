library(here); library(tidyverse); library(tidycensus);
library(sf); library(magrittr); library(RColorBrewer)

# 1. Check for low-population tracts. Ensure removed from regressions
# 2. Download and prep ind_vars data

# 1. Check for low-population tracts.
h2o <- st_read(here("data", "h2o.shp")) %>%
  st_transform(26918) %>%
  st_union(.)
open_space <- st_read(here("proprietary", "protected_open_space.shp")) %>%
  st_transform(26918) %>%
  st_buffer(., 0.5) %>%
  st_union(.)
h2o_open_space <- st_union(h2o, open_space)
# Get ACS estimates
# https://api.census.gov/data/2017/acs/acs5/variables.html
acs <- get_acs(geography = "tract",
               state = c(42),
               output = "wide",
               geometry = TRUE,
               variables = c(totPop = "B01003_001E"))
# redo sqmi s.t. parks and water are excluded
acs <- acs[, -( grep("\\M$" , colnames(acs), perl = TRUE))] %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  filter(stcty %in% c("42017", "42029", "42045", "42091", "42101")) %>%
  st_transform(26918) %>%
  mutate(sqmi_full = as.numeric(st_area(.) * 0.00000038610))
# Open space and water sometimes overlap.
# One scenario for just water
# Another for water + open space that accounts for overlap
acs_h2o <- st_intersection(acs, h2o) %>%
  mutate(sqmi_h2o = as.numeric(st_area(.) * 0.00000038610)) %>%
  select(GEOID, sqmi_h2o) %>%
  st_set_geometry(NULL)
# WARNING: THIS TAKES HOURS TO RUN
acs_parks <- st_intersection(acs, h2o_open_space) %>%
  mutate(sqmi_parks = as.numeric(st_area(.) * 0.00000038610)) %>%
  select(GEOID, sqmi_parks) %>%
  st_set_geometry(NULL)
acs_subtract <- left_join(acs, acs_h2o) %>%
  left_join(., acs_parks) %>%
  replace_na(list(sqmi_parks = 0, sqmi_h2o = 0)) %>%
  mutate(sqmi_waterless = sqmi_full - sqmi_h2o,
         sqmi_protected = sqmi_full - sqmi_parks) %>%
  select(GEOID, sqmi_waterless, sqmi_protected) %>%
  st_set_geometry(NULL)

acs <- left_join(acs, acs_subtract)
zero_pop <- acs %>%
  filter(totPop == 0)
hund_pop <- acs %>%
  filter(totPop < 100)
png(here("maps", "zero_pop.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(st_geometry(acs), main = "Zero Population Tracts",
     col = "gainsboro", border = NA, reset = FALSE)
plot(st_geometry(zero_pop), add = TRUE, col = brewer.pal(8, "YlOrRd")[6], border = NA)
plot(st_geometry(h2o),
     add = TRUE, col = "lightcyan", border = NA)
dev.off()
png(here("maps", "hundred_pop.png"), width = 10, height = 7.5, units = "in", res = 500)
plot(st_geometry(acs), main = "Tracts with Fewer than 100 Residents",
     col = "gainsboro", border = NA, reset = FALSE)
plot(st_geometry(hund_pop), add = TRUE, col = brewer.pal(8, "YlOrRd")[6], border = NA)
plot(st_geometry(h2o),
     add = TRUE, col = "lightcyan", border = NA)
dev.off()

# 2. Download and prep ind_vars data
# a = Persons (1000s) per sq mi
# b = Percentage of population enrolled in college or university
# c = Number of jobs (1000s) per sq mi
# d = Percentage of households below FPL
# e = Transit activity density
# f = Sidewalk density
# g = Median household income
# h = Percentage of zero-car households
# i = Percentage of nonwhite residents
# j = Pedestrian crashes
# k = Philadelphia Litter Index
# l = DVRPC TransitScore
# m = Land use mix, Herfindahl-Hirschman Index / 100
# n = Percentage of pedestrian commuters (will use in regressions)
# o = Sidewalk to road ratio
# p = Road density
# q = People density
# r = People interaction

# a b d g h i n
# Get ACS estimates
# https://api.census.gov/data/2017/acs/acs5/variables.html
acs <- get_acs(geography = "tract",
               state = c(42),
               output = "wide",
               geometry = TRUE,
               variables = c(
                 totPop = "B01003_001E",        # a
                 edUniverse = "B14001_001E",    # start b
                 edBach = "B14001_008E",
                 edGrad = "B14001_009E",        # end b
                 povUniverse = "B17001_001E",   # d
                 blPov = "B17001_002E",         # d
                 medInc = "B19013_001E",        # g
                 zcUniverse = "B08014_001E",    # h
                 zcEst = "B08014_002E",         # h
                 nwUniverse = "B03002_001E",    # i
                 nwEst = "B03002_003E",         # i
                 wlkComm = "B08111_021E",       # n
                 wlkUniverse = "B08111_001E"    # n
                 ))
acs <- acs[, -( grep("\\M$" , colnames(acs), perl = TRUE))] %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  filter(stcty %in% c("42017", "42029", "42045", "42091", "42101")) %>%
  filter(totPop >= 100) %>% # Minimum tract population is 100
  left_join(., acs_subtract)

# c
# Get LODES data, aggregate to tract, join to ACS table
# Uncomment the two lines below to download LEHD WAC file
# jobsPA <- "https://lehd.ces.census.gov/data/lodes/LODES7/pa/wac/pa_wac_S000_JT00_2015.csv.gz"
# download.file(jobsPA, here("data", "pa_wac_S000_JT00_2015.csv.gz"), mode = "wb")
jobs <- read.csv(gzfile(here("data", "pa_wac_S000_JT00_2015.csv.gz"))) %>%
  select(w_geocode, C000) %>%
  mutate(stcty = substr(as.character(w_geocode), 1, 5),
         GEOID = substr(as.character(w_geocode), 1, 11)) %>%
  filter(stcty %in% c("42017", "42029", "42045", "42091", "42101")) %>%
  group_by(GEOID) %>%
  summarize(totJobs = sum(C000))
acs %<>% left_join(., jobs, by = "GEOID")

# e
# Get ridership for SEPTA and PATCO, geocode PATCO, count by tract,
# div. by land area
# NHSL
# http://septaopendata-septa.opendata.arcgis.com/datasets/septa-norristown-highspeed-line-stations
# Average weekday boards and alights
nhsl <- st_read(here("proprietary", "nhsl_stations.shp")) %>%
  st_transform(26918) %>%
  select(Weekday_To) %>%
  rename(weekday_total = Weekday_To)
# BSL
# http://septaopendata-septa.opendata.arcgis.com/datasets/septa-broad-street-line-stations
# Average weekday boards and alights
bsl <- st_read(here("proprietary", "bsl_stations.shp")) %>%
  st_transform(26918) %>%
  select(Average_We) %>%
  rename(weekday_total = Average_We)
# MFL
# http://septaopendata-septa.opendata.arcgis.com/datasets/septa-market-frankford-line-stations
# Average weekday boards and alights
mfl <- st_read(here("proprietary", "mfl_stations.shp")) %>%
  st_transform(26918) %>%
  select(Average_We) %>%
  rename(weekday_total = Average_We)
# Bus
# http://septaopendata-septa.opendata.arcgis.com/datasets/septa-bus-stops
# Average weekday boards and alights
bus <- st_read(here("proprietary", "bus_stops.shp")) %>%
  st_transform(26918) %>%
  mutate(weekday_total = WEEKDAY_BO + WEEKDAY_LE) %>%
  select(weekday_total)
# Trolley
# http://septaopendata-septa.opendata.arcgis.com/datasets/septa-trolley-stops
# Average weekday boards and alights
trolley <- st_read(here("proprietary", "trolley_stops.shp")) %>%
  st_transform(26918) %>%
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
patco_geo <- tribble(~Station, ~Y, ~X,
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
# Bind all
riders <- rbind(nhsl, bsl) %>%
  rbind(., mfl) %>%
  rbind(., bus) %>%
  rbind(., trolley) %>%
  rbind(., patco_geo) %>%
  st_transform(st_crs(acs))
# Overlay with tracts
riders_by_tract <- st_intersection(riders, acs) %>%
  st_set_geometry(NULL) %>%
  group_by(GEOID) %>%
  summarize(transit_activity = sum(weekday_total))
# If tract missing, append ridership of 0
# Divide by sqmi
ids <- unique(acs$GEOID)
GEOID <- ids[!(ids %in% riders_by_tract$GEOID)]
transit_activity <- rep(0, length(GEOID))
appends <- tibble(GEOID, transit_activity)
riders_by_tract <- bind_rows(riders_by_tract, appends)
acs %<>% left_join(., riders_by_tract)

# f
# Get sidewalks, compute length, join to ACS table
swalk_burbs <- st_read(here("proprietary", "sidewalk_burbs.shp")) %>%
  select(geometry) %>%
  st_transform(., st_crs(acs))
swalk_phila <- st_read(here("proprietary", "sidewalk_phila.shp")) %>%
  select(geometry) %>%
  st_transform(., st_crs(acs))
swalk <- rbind(swalk_burbs, swalk_phila)
intersects_s <- st_intersection(swalk, acs) %>% # Get length
  mutate(length = st_length(.)) %>%
  mutate_at(c("length"), as.numeric) %>%
  select(length, GEOID)
intersects_s %<>% st_set_geometry(NULL) %>%
  group_by(GEOID) %>%
  summarize(swalk_length = sum(length) * 0.000621371) # Convert to miles
acs %<>% left_join(., intersects_s, by = "GEOID") %>%
  mutate(swalk_length = replace_na(swalk_length, 0))

# j
# Get pedestrian crashes, count by tract, join to ACS table
crash <- st_read(here("data", "ped_crash.shp")) %>%
  st_transform(., st_crs(acs))
intersects <- st_intersection(crash, acs) %>%
  select(GEOID)
intersects <- as.data.frame(table(intersects$GEOID)) %>%
  rename(GEOID = Var1, crash_cnt = Freq)
acs %<>% left_join(., intersects, by = "GEOID") %>%
  mutate(crash_cnt = replace_na(crash_cnt, 0))

# k
# Get Litter Index, transform to tract, join to ACS table
# Uncomment the two lines below to download 2017 Litter Index
# url <- "https://phl.carto.com/api/v2/sql?q=SELECT+*+FROM+litter_index_polygon&filename=litter_index_polygon&format=shp&skipfields=cartodb_id"
# download.file(url, here("data", "litter_idx.zip"), mode = "wb")
unzip(here("data", "litter_idx.zip"), exdir = here("data"))
litter <- st_read(here("data", "./litter_index_polygon.shp")) %>%
  st_transform(., st_crs(acs)) %>%
  rename(score = division_s,
         BLOCKID = objectid) %>%
  select(BLOCKID, score) %>%
  mutate(BLOCKAREA = st_area(.) * 0.00000038610) %>%
  mutate_at(c("BLOCKAREA"), as.numeric)

bt <- st_intersection(litter, acs) # block - tract
split_by_tract <- bt %>% group_by(GEOID) %>%
  do(data=(.)) %>%
  pull(data)
res <- data.frame()
for(i in 1:length(split_by_tract)){
  data <- split_by_tract[[i]]
  data %<>% st_set_geometry(NULL) %>%
    mutate(weight = BLOCKAREA / sum(BLOCKAREA)) %>%
    group_by(BLOCKID) %>%
    summarize(blockWEIGHT = sum(weight), # Contribution of each block to each tract
              GEOID = split_by_tract[[i]]$GEOID[1])
  res <- rbind(res, data)
}
res %<>% left_join(., litter, by = "BLOCKID") %>%
  group_by(GEOID) %>%
  summarize(litter_scr = sum(score * blockWEIGHT))
acs %<>% left_join(., res, by = "GEOID") %>%
  mutate(litter_scr = replace_na(litter_scr, 0))

# l
# Get DVRPC TransitScore, transform to tract, join to ACS table
transit <- st_read(here("proprietary", "./transit_score.shp")) %>%
  st_transform(., st_crs(acs)) %>%
  rename(score = TRANSC10,
         TAZID = TAZ) %>%
  select(TAZID, score) %>%
  mutate(TAZAREA = st_area(.) * 0.00000038610) %>%
  mutate_at(c("TAZAREA"), as.numeric)

tt <- st_intersection(transit, acs) # TAZ - tract
split_by_tract <- tt %>% group_by(GEOID) %>%
  do(data=(.)) %>%
  pull(data)
res <- data.frame()
for(i in 1:length(split_by_tract)){
  data <- split_by_tract[[i]]
  data %<>% st_set_geometry(NULL) %>%
    mutate(weight = TAZAREA / sum(TAZAREA)) %>%
    group_by(TAZID) %>%
    summarize(tazWEIGHT = sum(weight), # Contribution of each TAZ to each tract
              GEOID = split_by_tract[[i]]$GEOID[1])
  res <- rbind(res, data)
}
res %<>% left_join(., transit, by = "TAZID") %>%
  group_by(GEOID) %>%
  summarize(trans_scr = sum(score * tazWEIGHT))
acs %<>% left_join(., res, by = "GEOID") %>%
  mutate(trans_scr = replace_na(trans_scr, 0)) %>%
  st_transform(., st_crs(26918))

# m
# Get DVRPC land use, compute HHI at tract level, join to ACS table
# https://www.investopedia.com/terms/h/hhi.asp
land <- st_read(here("proprietary", "./land_use.shp")) %>%
  st_transform(., st_crs(26918)) %>%
  mutate(simple_lu = substr(LU_TYPE, 1,2),
         LUAREA = st_area(.)) %>%
  mutate_at(c("LUAREA"), as.numeric) %>%
  dplyr::select(OBJECTID, simple_lu, LUAREA)

lt <- st_intersection(land, acs)
split_by_tract <- lt %>% group_by(GEOID) %>%
  do(data=(.)) %>%
  pull(data)
res <- data.frame()
for(i in 1:length(split_by_tract)){
  data <- split_by_tract[[i]]
  data %<>% st_set_geometry(NULL) %>%
    mutate(weight = LUAREA / sum(LUAREA) * 100) %>%
    group_by(OBJECTID) %>%
    summarize(luWEIGHT = sum(weight),
              GEOID = split_by_tract[[i]]$GEOID[1])
  res <- rbind(res, data)
}
res %<>% group_by(GEOID) %>%
  summarize(lu_mix = sum(luWEIGHT ^ 2) / 100) # Normally HHI ranges 0-10,000; rescale 0-100
acs %<>% left_join(., res, by = "GEOID")

# o p
# Get roads, compute length, join to ACS table
roads <- st_read(here("proprietary", "PACenterline.shp")) %>%
  filter(RTETYP != 5400) %>% # Remove use code 5400, limited access roads
  select(geometry) %>%
  st_transform(., st_crs(acs))
intersects_r <- st_intersection(roads, acs) %>% # Get length
  mutate(length = st_length(.)) %>%
  mutate_at(c("length"), as.numeric) %>%
  select(length, GEOID)
intersects_r %<>% st_set_geometry(NULL) %>%
  group_by(GEOID) %>%
  summarize(road_length = sum(length) * 0.000621371) # Convert to miles
acs %<>% left_join(., intersects_r, by = "GEOID") %>%
  mutate(road_length = replace_na(road_length, 0))

# Aggregate and compute final variables
ind_vars <- acs %>%
  mutate(pop_dens = totPop / sqmi_protected / 1000, # a
         pct_coll = (edBach + edGrad) / edUniverse * 100, # b
         job_dens = totJobs / sqmi_protected / 1000, # c
         bl_pov = blPov / povUniverse * 100, # d
         ta_dens = transit_activity / sqmi_protected, # e
         swalk_dens = swalk_length / sqmi_waterless, # f
         med_inc = medInc / 1000, # g
         pct_zc = zcEst / zcUniverse * 100, # h
         pct_nw = (1 - (nwEst / nwUniverse)) * 100, # i
         # j, k, l, and m already calculated
         pct_wlk = wlkComm / wlkUniverse * 100, # n
         s_r_ratio = swalk_length / road_length, # o
         road_dens = road_length / sqmi_waterless, # o
         ppl_dens = pop_dens + job_dens, # q
         ppl_interaction = pop_dens * job_dens # r
         ) %>%
  select(GEOID, pop_dens, pct_coll, job_dens, bl_pov, ta_dens,
         swalk_dens, road_dens, med_inc, pct_zc, pct_nw, crash_cnt,
         litter_scr, trans_scr, lu_mix, pct_wlk, s_r_ratio,
         ppl_dens, ppl_interaction)
st_write(ind_vars, here("outputs", "ind_vars.shp"), delete_dsn = TRUE)
ind_vars %<>% st_set_geometry(NULL)
write.csv(ind_vars, here("outputs", "ind_vars.csv"), row.names = FALSE)
