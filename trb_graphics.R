library(here)
library(sf)
library(tidyverse)
library(tigris)
library(viridis)
options(tigris_class = "sf")

# Map of point locations of ped counts in study area
ped_counts <- st_read(here("proprietary", "ped_counters.shp"))

# 5-county region
se_pa <- counties(state = 42) %>%
  filter(GEOID %in% c("42017", "42029", "42045", "42091", "42101")) %>%
  st_transform(., st_crs(ped_counts))
phila <- se_pa %>% filter(GEOID == "42101")

# water bodies
h2o_se_pa <- st_read(here("proprietary", "h2o.shp")) %>%
  st_transform(., st_crs(ped_counts)) %>%
  st_intersection(., st_union(se_pa))
h2o_phila <- st_read(here("proprietary", "h2o.shp")) %>%
  st_transform(., st_crs(ped_counts)) %>%
  st_intersection(., st_union(phila))

# trails
trails <- st_read(here("proprietary", "circuit_trails_buff.shp"))

# census tracts
all_shp <- tracts(state = 42, county = 101) %>%
  select(GEOID) %>%
  st_transform(26918)

# Remove all counts that intersect with trails buffer
trails_intersect <- st_intersects(trails, ped_counts)
keep_ids <- setdiff(1:nrow(ped_counts), unlist(trails_intersect))
remains <- ped_counts[keep_ids,]
# Remove all counts that have comments
remains <- remains[is.na(remains$COMMENTS),]

ped_counts_phila <- st_intersection(remains, st_union(phila))

# Make a box around Center City
center_city <- tribble(~corner, ~X, ~Y,
                       "southeast", -75.142242, 39.940732, 
                       "northeast", -75.138726, 39.956790,
                       "northwest", -75.180159, 39.961896,
                       "southwest", -75.186344, 39.946505) %>%
  st_as_sf(., coords = c("X", "Y"), crs = 4326) %>%
  st_transform(., st_crs(ped_counts))

# Build linestrings
a <- st_combine(center_city[1:2,]) %>%
  st_cast(., "LINESTRING")
b <- st_combine(center_city[2:3,]) %>%
  st_cast(., "LINESTRING")
c <- st_combine(center_city[3:4,]) %>%
  st_cast(., "LINESTRING")
d <- st_combine(center_city %>% filter(corner %in% c("southeast", "southwest"))) %>%
  st_cast(., "LINESTRING")
box_list <- list(a, b, c, d)
box <- st_multilinestring(do.call("rbind", box_list)) %>%
  st_sfc(., crs = 26918)

ggplot() +
  theme(panel.background = element_blank()) +
  geom_sf(data = phila, fill = "gainsboro", color = NA) +
  geom_sf(data = h2o_phila, fill = "#669999", color = NA) +
  geom_sf(data = ped_counts_phila, color = "#666666", size = 1) +
  geom_sf(data = phila, fill = NA, color = "gray") +
  annotate("text", x = 492500, y = 4422000, label = "Center City") +
  geom_sf(data = box, color = "black") +
  coord_sf(datum = NA) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave(here("figures", "trb_phila-counters.png"), width = 3.5, height = 3.5, units = "in", dpi = "retina")

# Map of regression residuals - assessing R1 fit
r1 <- st_read(here("outputs", "act_peds.shp"))
r1_sub <- r1 %>%
  drop_na(.) %>%
  filter(e_wlk_den < 300000) %>%
  filter(!row_number() %in% which(e_wlk_den > 30000 & substr(GEOID, 3, 5) != "101"))

mod <- lm(a_wlk_den ~ e_wlk_den, data = r1_sub)
r1_sub$resid <- mod$residuals
r1_sub$preliminary_class <- cut_width(r1_sub$resid, center = mean(r1_sub$resid), width = sd(r1_sub$resid))
r1_phila <- r1_sub %>% filter(str_sub(GEOID, 3, 5) == "101") %>%
  mutate(final_class = fct_recode(preliminary_class,
                                  `(-Inf, -1.5SD] (Underestimate)` = "[-2.84e+04,-2.4e+04]",
                                  `(-Inf, -1.5SD] (Underestimate)` = "(-1.97e+04,-1.53e+04]",
                                  `(-Inf, -1.5SD] (Underestimate)` = "(-1.53e+04,-1.09e+04]",
                                  `(-Inf, -1.5SD] (Underestimate)` = "(-1.09e+04,-6.56e+03]",
                                  `(-1.5SD, -0.5SD]` = "(-6.56e+03,-2.19e+03]",
                                  `(-0.5SD, 0.5SD]` = "(-2.19e+03,2.19e+03]",
                                  `(0.5SD, 1.5SD]` = "(2.19e+03,6.56e+03]",
                                  `(1.5SD, Inf) (Overestimate)` = "(6.56e+03,1.09e+04]",
                                  `(1.5SD, Inf) (Overestimate)` = "(1.09e+04,1.53e+04]",
                                  `(1.5SD, Inf) (Overestimate)` = "(1.53e+04,1.97e+04]",
                                  `(1.5SD, Inf) (Overestimate)` = "(4.59e+04,5.03e+04]"
                                  )) %>%
  st_set_geometry(NULL)

r1_phila <- left_join(all_shp, r1_phila)

ggplot() +
  theme(panel.background = element_blank()) +
  geom_sf(data = phila, fill = "gainsboro", color = NA) +
  geom_sf(data = r1_phila, aes(fill = final_class), color = "gray") +
  # scale_fill_grey("Residual", na.value = "white") +
  scale_fill_viridis_d("Residual", option = "cividis",
                       na.value = "white") +
  geom_sf(data = r1_phila, aes(fill = final_class), color = NA, show.legend = FALSE) +
  geom_sf(data = h2o_phila, fill = "#669999", color = NA) +
  geom_sf(data = phila, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figures", "trb_phila_resid.png"), width = 6, height = 4.5, units = "in", dpi = "retina")

# Philadelphia areal strata 
strata <- read_csv(here("outputs", "ind_vars.csv")) %>%
  select(GEOID, pct_coll, pop_dens, bl_pov, ta_dens, road_dens) %>%
  mutate_at(vars(matches("GEOID")), as.factor) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  drop_na()
phila_shp <- strata %>%
  filter(stcty == "42101") %>%
  mutate_if(is.numeric, funs(ifelse(. > median(.), 1, 0))) %>%
  mutate(stratum = paste(pct_coll, ta_dens, sep = ".")) %>%
  select(GEOID, stratum) %>%
  left_join(all_shp, .)
ns <- c(LL = "0.0", LH = "0.1", HL = "1.0", HH = "1.1")
phila_shp <- phila_shp %>%
  mutate(new_stratum = fct_recode(stratum, !!!ns)) %>%
  mutate(new_stratum_fill = ifelse(is.na(new_stratum), "No Data", new_stratum))

ggplot() +
  theme(panel.background = element_blank()) +
  geom_sf(data = phila, fill = "gainsboro", color = NA) +
  geom_sf(data = phila_shp, aes(fill = new_stratum), color = "gray") +
  # scale_fill_grey("Stratum", na.value = "white") +
  scale_fill_viridis_d("Stratum", option = "cividis", na.value = "white") +
  geom_sf(data = phila_shp, aes(fill = new_stratum), color = NA, show.legend = FALSE) +
  geom_sf(data = h2o_phila, fill = "#669999", color = NA) +
  geom_sf(data = phila, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figures", "trb_phila-areal.png"), width = 4.5, height = 4.5, units = "in", dpi = "retina")

# Suburban sampling strata
setwd("P:/TransitBikePed/FY2019/SE PA Ped Cyclical Counting/ped_counts/outputs")

delco <- se_pa %>% filter(GEOID == "42045")
h2o_delco <- h2o_se_pa %>%
  st_intersection(., st_union(delco))
areal <- st_read("sub_areal_sel.shp") %>%
  mutate(Comments = "Selected",
         Type = "Census Tract") %>%
  select(Type, Comments)
trans <- st_read("sub_trans_sel.shp") %>%
  mutate(Type = "Transit Arterial") %>%
  select(Type, Comments)
non_trans <- st_read("sub_non_trans_sel.shp") %>%
  mutate(Type = "Non-Transit Arterial") %>%
  select(Type, Comments)
sch <- st_read("sub_sch_sel.shp") %>%
  mutate(Type = "School") %>%
  select(Type, Comments)
all_strata <- rbind(areal, trans) %>%
  rbind(., non_trans) %>%
  rbind(., sch) %>%
  filter(Comments == "Selected") %>%
  st_transform(., st_crs(delco)) %>%
  st_intersection(., st_union(delco)) %>%
  mutate_at(vars(Type), funs(fct_relevel(., "Census Tract", "School",
                                         "Transit Arterial", "Non-Transit Arterial")))
tailor <- all_strata %>%
  filter(Type == "Transit Arterial" | Type == "Non-Transit Arterial") %>%
  st_buffer(., 201.168)
ggplot() +
  theme(panel.background = element_blank()) +
  geom_sf(data = delco, fill = "white", color = NA) +
  geom_sf(data = all_strata, aes(fill = Type), color = NA) +
  scale_fill_viridis_d(option = "cividis", direction = -1) +
  # scale_fill_grey(start = 0.8, end = 0.2) +
  geom_sf(data = tailor, aes(fill = Type), color = NA, show.legend = FALSE, lwd = 2) +
  scale_color_viridis_d(option = "cividis", direction = -1) +
  # scale_color_grey(start = 0.6, end = 0.4) +
  geom_sf(data = h2o_delco, fill = "#669999", color = NA) +
  geom_sf(data = delco, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figures", "trb_sample-strata.png"), width = 6, height = 4, units = "in", dpi = "retina")
