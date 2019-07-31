library(here); library(tidyverse); library(sf)
# AREAL STRATA
# Because we are placing 10 counts per stratum and hope to divide
# somewhat evenly among 4 counties, we start by placing 2 counts
# per stratum per county for areal sampling and creating a
# "pooled_fund" with the remainder.
# ARTERIAL STRATA
# Select 1 per areal stratum for even distribution among place-types.
# Place remaining four into pooled fund.
# SCHOOL STRATUM
# Select 1 per areal stratum for even distribution among place-types.
# Place remaining two into pooled fund.

# Don't overwrite what's already done.
if(!(file.exists(here("outputs", "sub_areal_sel.shp")))){
  strata <- read.csv(here("outputs", "ind_vars.csv")) %>%
    select(GEOID, pct_coll, pop_dens, bl_pov, ta_dens, road_dens) %>%
    mutate_at(vars(matches("GEOID")), as.factor) %>%
    mutate(stcty = str_sub(GEOID, 1, 5)) %>%
    drop_na()
  sub <- strata %>%
    filter(stcty %in% c("42017", "42029", "42045", "42091")) %>%
    mutate_if(is.numeric, funs(ifelse(. > median(.), 1, 0))) %>%
    mutate(stratum = paste(pop_dens, pct_coll, road_dens, sep = ".")) %>%
    select(GEOID, stcty, stratum)
  sch <- st_read(here("outputs", "sub_school_stratum.shp")) %>%
    mutate(stcty = str_sub(GEOID, 1, 5))
  arterial <- st_read(here("outputs", "sub_transit_arterial_strata.shp")) %>%
    mutate(stcty = str_sub(GEOID, 1, 5))
  arterial_nontrans <- st_read(here("outputs", "sub_non_transit_arterial_strata.shp")) %>%
    mutate(stcty = str_sub(GEOID, 1, 5))
  areal <- st_read(here("outputs", "sub_strata.shp"))
  
  # Breakdown by county / strata
  tabular <- sub %>% group_by(stcty) %>% count(stratum)
  
  # Areal strata GEOIDs
  areal_rule_ID <- sub %>% group_by(stcty, stratum) %>%
    sample_n(2, replace = FALSE) %>%
    pull(GEOID)
  # Delaware County (stcty 42045) has 0 stratum 0.1.1.
  # Place remainder into pooled fund
  areal_rule <- areal %>% filter(GEOID %in% areal_rule_ID)
  # Arterial strata
  trans_rule <- arterial %>%
    filter(present == 1) %>%
    group_by(stratum) %>%
    sample_n(7, replace = FALSE) %>%
    mutate(precedence = rep(1:7),
                            type = "By Stratum")
  non_trans_rule <- arterial_nontrans %>%
    filter(present == 0) %>%
    group_by(stratum) %>%
    sample_n(7, replace = FALSE) %>%
    mutate(precedence = rep(1:7),
           type = "By Stratum")
  # School stratum
  sch_rule <- sch %>%
    group_by(stratum) %>%
    sample_n(7, replace = FALSE) %>%
    mutate(precedence = rep(1:7),
           type = "By Stratum")
  # Remove IDs s.t. exclude from pooled fund
  trans_rule_ID <- trans_rule %>%
    st_set_geometry(NULL) %>%
    select(ID) %>% pull(.)
  non_trans_rule_ID <- non_trans_rule %>%
    st_set_geometry(NULL) %>%
    select(ID) %>% pull(.)
  sch_rule_ID <- sch_rule %>%
    st_set_geometry(NULL) %>%
    select(SCHOOLNAME) %>% pull(.)
  arterial <- arterial %>%
    filter(!(ID %in% trans_rule_ID)) %>%
    filter(!(ID %in% non_trans_rule_ID))
  sch <- sch %>%
    filter(!(SCHOOLNAME %in% sch_rule_ID))
  areal <- areal %>%
    filter(!(GEOID %in% areal_rule_ID))
  sub <- sub %>%
    filter(!(GEOID %in% areal_rule_ID))
  
  # Sample for pooled fund
  new_tabular <- sub %>%
    group_by(stcty) %>%
    count(stratum) %>%
    arrange(stcty, -n)
  # Selections from pooled fund based on largest observed n
  # 0.0.0      42017 42029
  # 0.1.0      42029 42045
  # 1.0.1      42017 42091
  # 1.1.1      42045 42091
  # 1.1.0      42017 42045
  # 1.0.0      42017 42045
  # 0.0.1      42017 42091
  # 0.1.1      42029 42091 42091 42091
  # School     42017 42029
  # Nontransit 42029 42045
  # Transit    42029 42045
  areal_exc_ID <- sub %>%
    filter(stratum == "0.0.0" & (stcty == "42017" | stcty == "42029")) %>%
    group_by(stcty) %>%
    sample_n(1, replace = FALSE) %>%
    pull(GEOID) %>%
    as.character(.)
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "0.1.0" & (stcty == "42029" | stcty == "42045")) %>%
                      group_by(stcty) %>%
                      sample_n(1, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "1.0.1" & (stcty == "42017" | stcty == "42091")) %>%
                      group_by(stcty) %>%
                      sample_n(1, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "1.1.1" & (stcty == "42045" | stcty == "42091")) %>%
                      group_by(stcty) %>%
                      sample_n(1, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "1.1.0" & (stcty == "42017" | stcty == "42045")) %>%
                      group_by(stcty) %>%
                      sample_n(1, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "1.0.0" & (stcty == "42017" | stcty == "42045")) %>%
                      group_by(stcty) %>%
                      sample_n(1, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "0.0.1" & (stcty == "42017" | stcty == "42091")) %>%
                      group_by(stcty) %>%
                      sample_n(1, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "0.1.1" & stcty == "42029") %>%
                      group_by(stcty) %>%
                      sample_n(1, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc_ID <- c(areal_exc_ID,
                    sub %>%
                      filter(stratum == "0.1.1" & stcty == "42091") %>%
                      group_by(stcty) %>%
                      sample_n(3, replace = FALSE) %>%
                      pull(GEOID) %>%
                      as.character(.))
  areal_exc <- areal %>%
    filter(GEOID %in% areal_exc_ID)
  areal_sel <- rbind(areal_exc, areal_rule)
  sch_exc <- sch %>%
    filter(stcty %in% c("42017", "42029")) %>%
    group_by(stcty) %>%
    sample_n(7, replace = FALSE) %>%
    mutate(precedence = rep(1:7),
           type = "By County")
  sch_sel <- rbind(sch_exc, sch_rule)
  trans_exc <- arterial %>%
    filter(present == 1) %>%
    filter(stcty %in% c("42029", "42045")) %>%
    group_by(stcty) %>%
    sample_n(7, replace = FALSE) %>%
    mutate(precedence = rep(1:7),
           type = "By County")
  non_trans_exc <- arterial_nontrans %>%
    filter(present == 0) %>%
    filter(stcty %in% c("42029", "42045")) %>%
    group_by(stcty) %>%
    sample_n(7, replace = FALSE) %>%
    mutate(precedence = rep(1:7),
           type = "By County")
  trans_sel <- rbind(trans_exc, trans_rule)
  non_trans_sel <- rbind(non_trans_exc, non_trans_rule)
  
  st_crs(areal_sel) <- 26918
  # MASK ALL FC BUFFERED FEATURES FROM AREAL_SEL
  arterial <- st_read(here("proprietary", "func_class_buff.shp")) %>%
    st_transform(., st_crs(areal_sel)) %>%
    st_union(.)
  areal_sel <- areal_sel %>% st_difference(., arterial)
  
  st_write(areal_sel, here("outputs", "sub_areal_sel.shp"))
  st_write(sch_sel, here("outputs", "sub_sch_sel.shp"), delete_dsn = TRUE)
  st_write(trans_sel, here("outputs", "sub_trans_sel.shp"), delete_dsn = TRUE)
  st_write(non_trans_sel, here("outputs", "sub_non_trans_sel.shp"), delete_dsn = TRUE)
}

# Manual corrections!
# Nothing worked for transit arterials falling in 1.0.1.
trans_sel <- st_read(here("outputs", "sub_trans_sel.shp")) %>%
  filter(stratum == "1.0.1") %>%
  pull(ID)
trans <- st_read(here("outputs", "sub_transit_arterial_strata.shp")) %>%
  filter(!(ID %in% trans_sel)) %>%
  filter(stratum == "1.0.1") %>%
  sample_n(7, replace = FALSE) %>%
  mutate(type = "By Stratum",
         stcty = str_sub(GEOID, 1, 5),
         precedence = seq(8, 14, 1))
trans_stack <- st_read(here("outputs", "sub_trans_sel.shp")) %>%
  rbind(., trans)
st_write(trans_stack, here("outputs", "sub_trans_sel.shp"), delete_dsn = TRUE)

# Nothing worked for nontransit arterials falling in 1.1.0 and 1.0.0.
trans_sel <- st_read(here("outputs", "sub_trans_sel.shp")) %>%
  filter(stratum == "1.1.0" | stratum == "1.0.0") %>%
  pull(ID)
transa <- st_read(here("outputs", "sub_non_transit_arterial_strata.shp")) %>%
  filter(!(ID %in% trans_sel)) %>%
  filter(stratum == "1.1.0") %>%
  sample_n(7, replace = FALSE) %>%
  mutate(type = "By Stratum",
         stcty = str_sub(GEOID, 1, 5),
         precedence = seq(8, 14, 1),
         Comments = NA)
transb <- st_read(here("outputs", "sub_non_transit_arterial_strata.shp")) %>%
  filter(!(ID %in% trans_sel)) %>%
  filter(stratum == "1.0.0") %>%
  sample_n(7, replace = FALSE) %>%
  mutate(type = "By Stratum",
         stcty = str_sub(GEOID, 1, 5),
         precedence = seq(8, 14, 1),
         Comments = NA)
existing_nontrans <- st_read("P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs/sub_non_trans_sel.shp")
existing_nontrans <- rbind(existing_nontrans, transa) %>%
  rbind(., transb)
st_write(existing_nontrans, here("outputs", "sub_non_trans_sel.shp"), delete_dsn = TRUE)

# ChesCo doesn't have a ton of schools -- we're taking all the ones
# in tract stratum 1.1.1 as candidates.
existing_sch <- st_read("P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs/sub_sch_sel.shp")
all_sch <- st_read(here("outputs", "sub_school_stratum.shp")) %>%
  filter(!(SCHOOLNAME %in% existing_sch$SCHOOLNAME)) %>%
  filter(str_sub(GEOID, 3, 5) == "029" & stratum == "1.1.1") %>%
  mutate(precedence = seq(1:nrow(.)),
         Comments = NA,
         stcty = "42029",
         type = "Unknown")
new_sch <- rbind(existing_sch, all_sch)
st_write(new_sch, here("outputs", "sub_sch_sel.shp"), delete_dsn = TRUE)

# One school incorrectly geocoded in ChesCo
existing_sch <- st_read("P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs/sub_sch_sel.shp") %>%
  filter(SCHOOLNAME != "CHESTER CO FAMILY ACADEMY CS") %>%
  st_set_crs(26918)
old_sch <- st_read("P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs/sub_sch_sel.shp") %>%
  filter(SCHOOLNAME == "CHESTER CO FAMILY ACADEMY CS") %>%
  st_set_geometry(NULL)
# This one has wrong address must be manually fixed
sch_manual_corr <- tribble(~SCHOOLNAME, ~X, ~Y,
                           "CHESTER CO FAMILY ACADEMY CS", -75.595449, 39.959660) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>%
  st_transform(26918) %>%
  st_buffer(., 402.336) %>% # 0.25 mi buffer
  left_join(., old_sch)
existing_sch <- rbind(existing_sch, sch_manual_corr)

# Additionally, requests for additional ChesCo schools
# How about not in stratum 1.1.1
all_sch <- st_read(here("outputs", "sub_school_stratum.shp")) %>%
  filter(!(SCHOOLNAME %in% existing_sch$SCHOOLNAME)) %>%
  filter(str_sub(GEOID, 3, 5) == "029" & stratum != "1.1.1") %>%
  sample_n(., 7, replace = FALSE) %>%
  mutate(precedence = 8:14,
         Comments = NA,
         stcty = "42029",
         type = "By County")
new_sch <- rbind(existing_sch, all_sch)
st_write(new_sch, here("outputs", "sub_sch_sel.shp"), delete_dsn = TRUE)

# Still, no ChesCo schools worked
existing_sch <- st_read("P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs/sub_sch_sel.shp") %>%
  st_set_crs(26918)
all_sch <- st_read(here("outputs", "sub_school_stratum.shp")) %>%
  filter(!(SCHOOLNAME %in% existing_sch$SCHOOLNAME)) %>%
  filter(str_sub(GEOID, 3, 5) == "029" & stratum != "1.1.1")
all_sch$stratum <- fct_relevel(all_sch$stratum,
                               "1.1.0", "1.0.1", "1.0.0", "0.1.0", "0.0.0")
all_sch <- all_sch %>% arrange(stratum) %>%
  mutate(precedence = 15:(nrow(all_sch) + 14),
         Comments = NA,
         stcty = "42029",
         type = "By County")
new_sch <- rbind(existing_sch, all_sch)
st_write(new_sch, here("outputs", "sub_sch_sel.shp"), delete_dsn = TRUE)

if(!file.exists(here("outputs", "phila_areal_sel.shp"))){
  areal <- st_read(here("outputs", "phila_strata.shp"))
  phila_areal_sel <- areal %>%
    group_by(stratum) %>%
    sample_n(., 10, replace = FALSE) %>%
    mutate(precedence = 1:10)
  st_write(phila_areal_sel, here("outputs", "phila_areal_sel.shp"))
}

# Keeping for later
# levels_phila <- c(`Low - College / Low - Poverty / Low - Transit Activity` = "0.0.0",
#                   `Low - College / Low - Poverty / High - Transit Activity` = "0.0.1",
#                   `Low - College / High - Poverty / High - Transit Activity` = "0.1.1",
#                   `High - College / High - Poverty / High - Transit Activity` = "1.1.1",
#                   `High - College / Low - Poverty / Low - Transit Activity` = "1.0.0",
#                   `High - College / High - Poverty / Low - Transit Activity` = "1.1.0",
#                   `High - College / Low - Poverty / High - Transit Activity` = "1.0.1",
#                   `Low - College / High - Poverty / Low - Transit Activity` = "0.1.0")
# phila$name <- fct_recode(phila$stratum, !!!levels_phila)

areal <- st_read(here("outputs", "sub_strata.shp"))
# pop_dens, pct_coll, road_dens
levels_sub <- c(`Low - Pop. Dens / Low - College / Low - Road. Dens` = "0.0.0",
                `Low - Pop. Dens / Low - College / High - Road. Dens` = "0.0.1",
                `Low - Pop. Dens / High - College / High - Road. Dens` = "0.1.1",
                `High - Pop. Dens / High - College / High - Road. Dens` = "1.1.1",
                `High - Pop. Dens / Low - College / Low - Road. Dens` = "1.0.0",
                `High - Pop. Dens / High - College / Low - Road. Dens` = "1.1.0",
                `High - Pop. Dens / Low - College / High - Road. Dens` = "1.0.1",
                `Low - Pop. Dens / High - College / Low - Road. Dens` = "0.1.0")
areal$name <- fct_recode(areal$stratum, !!!levels_sub)
st_write(areal, here("outputs", "sub_strata.shp"), delete_dsn = TRUE)

# Problem tracts in Delaware County
# (Must ask why -- no sidewalk?)
s <- st_read("P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs/sub_areal_sel.shp")
s %>% filter(GEOID == "42045407000" | GEOID == "42045405000")
# 42045407000 is 1.0.1 and 42045405000 is 0.1.0
strata <- read.csv(here("outputs", "ind_vars.csv")) %>%
  select(GEOID, pct_coll, pop_dens, bl_pov, ta_dens, road_dens) %>%
  mutate_at(vars(matches("GEOID")), as.factor) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  drop_na()
all_shp <- st_read(here("outputs", "./act_peds.shp")) %>%
  select(GEOID, e_wlk_den) %>%
  mutate(cty = str_sub(GEOID, 3, 5)) %>%
  st_transform(26918)
delco <- strata %>%
  filter(stcty %in% c("42045")) %>%
  mutate_if(is.numeric, funs(ifelse(. > median(.), 1, 0))) %>%
  mutate(stratum = paste(pop_dens, pct_coll, road_dens, sep = ".")) %>%
  select(GEOID, stratum) %>%
  filter(!(GEOID %in% s$GEOID))
delco_shp <- inner_join(all_shp, delco) %>%
  st_transform(., 26918)

r1 <- delco_shp %>% filter(stratum == "1.0.1") %>% sample_n(., 1)
r2 <- delco_shp %>% filter(stratum == "0.1.0") %>% sample_n(., 1)

# Drop 42045407000 and 42045405000
# Replace with r1 and r2

s <- s %>% filter(!(GEOID %in% c("42045407000", "42045405000")))
s <- rbind(s, r1) %>% rbind(., r2)
st_write(s, "P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs/sub_areal_sel.shp", delete_dsn = TRUE)

# PHILADELPHIA
phila_arterial <- st_read(here("outputs", "phila_transit_arterial_strata.shp"))
table(phila_arterial$stratum, phila_arterial$ab_med)
# Allocate more transit arterials to the c.t. stratum
# With the most observations
# Low transit: 2 0.1 and 1.1, 3 0.0 and 1.0
# High transit: 2 0.0 and 1.0, 3 0.1 and 1.1
low_transit <- phila_arterial %>%
  filter(ab_med == 0) %>%
  group_by(stratum) %>%
  sample_n(20, replace = FALSE) %>%
  mutate(obs = c(sapply(1:2, function(i) rep(i, 10))),
         precedence = rep(c(1:10), 2))
exclude_ids <- low_transit %>% pull(ROADID)
low_transit_exc <- phila_arterial %>%
  filter(ab_med == 0) %>%
  filter(!(ROADID %in% exclude_ids)) %>%
  filter(stratum == "0.0" | stratum == "1.0") %>%
  group_by(stratum) %>%
  sample_n(., 10, replace = FALSE) %>%
  mutate(obs = rep(3, 10),
         precedence = 1:10)
low_transit_full <- rbind(low_transit, low_transit_exc) %>%
  arrange(., obs)

high_transit <- phila_arterial %>%
  filter(ab_med == 1) %>%
  group_by(stratum) %>%
  sample_n(20, replace = FALSE) %>%
  mutate(obs = c(sapply(1:2, function(i) rep(i, 10))),
         precedence = rep(c(1:10), 2))
exclude_ids <- high_transit %>% pull(ROADID)
high_transit_exc <- phila_arterial %>%
  filter(ab_med == 1) %>%
  filter(!(ROADID %in% exclude_ids)) %>%
  filter(stratum == "0.1" | stratum == "1.1") %>%
  group_by(stratum) %>%
  sample_n(., 10, replace = FALSE) %>%
  mutate(obs = rep(3, 10),
         precedence = 1:10)
high_transit_full <- rbind(high_transit, high_transit_exc) %>%
  arrange(., obs)

phila_arterial <- rbind(low_transit_full, high_transit_full)

if(!(file.exists(here("outputs", "phila_arterial_sel.shp")))){
  st_write(phila_arterial, here("outputs", "phila_arterial_sel.shp")) 
}
