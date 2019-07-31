library(here)
library(sf)
library(tidyverse)
library(tigris)
library(grid)
library(gridExtra)
library(extrafont)
library(viridis)
options(tigris_class = "sf")
loadfonts(device = "win")

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

ped_counts <- st_intersection(ped_counts, st_union(se_pa))
ped_counts_phila <- st_intersection(ped_counts, st_union(phila))

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("Existing pedestrian counts,\nCity of Philadelphia") +
  geom_sf(data = phila, fill = "gainsboro", color = NA) +
  geom_sf(data = h2o_phila, fill = "#669999", color = NA) +
  geom_sf(data = ped_counts_phila, color = "#666666") +
  geom_sf(data = phila, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figures", "phila-counters.png"), width = 4.5, height = 4.5, units = "in", dpi = 400)

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("Existing pedestrian counts, Southeastern Pennsylvania") +
  geom_sf(data = se_pa, fill = "gainsboro", color = "gray") +
  geom_sf(data = h2o_se_pa, fill = "#669999", color = NA) +
  geom_sf(data = ped_counts, color = "#666666") +
  coord_sf(datum = NA)
ggsave(here("figures", "region-counters.png"), width = 10, height = 7.5, units = "in", dpi = 400)

# Map of raster?
idw <- raster::raster(here("outputs", "act_peds.tif"))
rasterVis::gplot(idw) +
  geom_tile(aes(fill = value)) +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("IDW Pedestrian Count by Grid Cell") +
  scale_fill_viridis_c("Count", option = "cividis") +
  coord_sf(datum = NA) +
  theme(axis.title = element_blank())
ggsave(here("figures", "idw.png"), width = 5, height = 3, units = "in", dpi = 400)

# Map of estimated pedestrian counts?
r1 <- st_read(here("outputs", "act_peds.shp"))
r2 <- r1 %>% filter(str_sub(GEOID, 3, 5) != "101")
r3 <- r1 %>% filter(str_sub(GEOID, 3, 5) == "101")

a <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("Pedestrian densities (AADP per sq. mi.)") +
  geom_sf(data = se_pa, fill = "gainsboro", color = "gray") +
  geom_sf(data = r2, aes(fill = e_wlk_den), color = NA) +
  geom_sf(data = h2o_se_pa, fill = "#669999", color = NA) +
  scale_fill_viridis_c("Density", option = "cividis", na.value = "gainsboro") +
  coord_sf(datum = NA)
b <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = phila, fill = NA, color = "gray") +
  geom_sf(data = r3, aes(fill = e_wlk_den), color = NA) +
  geom_sf(data = h2o_phila, fill = "#669999", color = NA) +
  scale_fill_viridis_c("Density", option = "cividis", na.value = "gainsboro") +
  coord_sf(datum = NA)
png(here("figures", "estdens.png"), width = 7.5, height = 9.5, units = "in", res = 400)
gridExtra::grid.arrange(a, b, nrow = 2, heights = c(0.6, 0.4))
dev.off()

# Map of regression residuals - assessing R1 fit
r1_sub <- r1 %>%
  drop_na(.) %>%
  filter(e_wlk_den < 300000) %>%
  filter(!row_number() %in% which(e_wlk_den > 30000 & substr(GEOID, 3, 5) != "101"))

mod <- lm(a_wlk_den ~ e_wlk_den, data = r1_sub)
stargazer::stargazer(mod, style = "qje", dep.var.labels = "Observed Pedestrian Density",
          covariate.labels = c("Estimated Pedestrian Density"),
          title = "Estimated vs. Observed Pedestrian Density by Tract", out = here("regressions", "r1_fit.txt"))
r1_sub$resid <- mod$residuals

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("Fit of estimated vs. observed pedestrians") +
  geom_sf(data = se_pa, fill = "gainsboro", color = "gray") +
  geom_sf(data = r1_sub, aes(fill = resid), color = NA) +
  scale_fill_viridis_c("Residual", option = "cividis", direction = -1) +
  geom_sf(data = h2o_se_pa, fill = "#669999", color = NA) +
  coord_sf(datum = NA)
ggsave(here("figures", "resid.png"), width = 10, height = 7.5, units = "in", dpi = 400)

r1_phila <- r1_sub %>% filter(str_sub(GEOID, 3, 5) == "101")
ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("Fit of estimated vs. observed pedestrians") +
  geom_sf(data = phila, fill = "gainsboro", color = NA) +
  geom_sf(data = r1_phila, aes(fill = resid), color = NA) +
  scale_fill_viridis_c("Residual", option = "cividis", direction = -1) +
  geom_sf(data = h2o_phila, fill = "#669999", color = NA) +
  geom_sf(data = phila, fill = NA, color = "gray") +
  coord_sf(datum = NA)
ggsave(here("figures", "phila_resid.png"), width = 4.5, height = 4.5, units = "in", dpi = 400)

# Map of sidewalk-to-road ratio
strata <- read_csv(here("outputs", "ind_vars.csv")) %>%
  select(GEOID, s_r_ratio) %>%
  mutate_at(vars(matches("GEOID")), as.factor) %>%
  filter(str_sub(GEOID, 3, 5) != "101") %>%
  drop_na()
all_shp <- st_read(here("outputs", "./act_peds.shp")) %>%
  select(GEOID) %>%
  st_transform(26918)
srr <- inner_join(all_shp, strata)
ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("Sidewalk-to-road ratio") +
  geom_sf(data = se_pa, fill = "gainsboro", color = "gray") +
  geom_sf(data = srr, aes(fill = s_r_ratio), color = NA) +
  scale_fill_viridis_c("Ratio", option = "cividis") +
  geom_sf(data = h2o_se_pa, fill = "#669999", color = NA) +
  coord_sf(datum = NA)
ggsave(here("figures", "srr.png"), width = 10, height = 7.5, units = "in", dpi = 400)

# Sample schematic of suburban transit arterials
fc <- st_read(here("proprietary", "sample_seg.shp"))
st_length(fc)

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

vor <- new_fc_seg %>%
  st_geometry(.) %>%
  st_union(.) %>%
  st_voronoi(.) %>%
  st_collection_extract(type = "POLYGON") %>%
  st_sf(crs = 26918) %>%
  st_crop(., st_bbox(new_fc_seg))

fake_ridership <- c("Yes", "No", "Yes", "No", "No")
vor <- vor %>%
  mutate(Ridership = fake_ridership,
         Record = 1:nrow(.))

fc <- fc %>%
  st_transform(., st_crs(vor))

fc_assign <- st_intersection(fc, vor) %>%
  mutate(Ridership = fake_ridership)

plot(st_geometry(new_segment_points), reset = FALSE)
st_distance(new_segment_points)

fake_station_1 = st_sfc(st_point(x = c(491030.6, 4447937), dim = "XY"))
st_crs(fake_station_1) <- st_crs(vor)
fake_station_2 = st_sfc(st_point(x = c(491209.7, 4448660), dim = "XY"))
st_crs(fake_station_2) <- st_crs(vor)
fs_buff_1 <- st_buffer(fake_station_1, 100)
fs_buff_2 <- st_buffer(fake_station_2, 100)

a <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("A") +
  geom_sf(data = fc, color = "black", lwd = 1.5) +
  coord_sf(datum = NA)
ggsave(here("figures", "segmentize-a.png"), width = 3, height = 4.5, units = "in", dpi = 400)

b <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("B") +
  geom_sf(data = fc_seg, color = "black") +
  coord_sf(datum = NA)
ggsave(here("figures", "segmentize-b.png"), width = 3, height = 4.5, units = "in", dpi = 400)

c <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("C") +
  geom_sf(data = new_fc_seg, color = "black") +
  coord_sf(datum = NA)
ggsave(here("figures", "segmentize-c.png"), width = 3, height = 4.5, units = "in", dpi = 400)

png(here("figures", "segmentize-abc.png"), width = 7, height = 4.5, units = "in", res = 400)
gridExtra::grid.arrange(a, b, c, ncol = 3)
dev.off()

d <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  ggtitle("D") +
  geom_sf(data = fs_buff_1, color = "#669999", fill = "#99CCCC") +
  geom_sf(data = fs_buff_2, color = "#669999", fill = "#99CCCC") +
  geom_sf(data = fake_station_1, color = "#669999") +
  geom_sf(data = fake_station_2, color = "#669999") +
  geom_sf(data = new_fc_seg, color = "black") +
  coord_sf(datum = NA)
ggsave(here("figures", "segmentize-d.png"), width = 3, height = 4.5, units = "in", dpi = 400)

e <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = vor, aes(fill = Ridership), color = NA) +
  geom_sf(data = new_fc_seg, color = "black") +
  ggtitle("E") +
  scale_fill_manual("Transit", values = c("#666666", "#669999")) +
  coord_sf(datum = NA)
ggsave(here("figures", "segmentize-e.png"), width = 3, height = 4.5, units = "in", dpi = 400)

f <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = fc_assign, aes(color = Ridership), lwd = 1.5, ) +
  geom_sf(data = new_fc_seg, color = "black") +
  ggtitle("F") +
  scale_color_manual("Transit", values = c("#666666", "#669999")) +
  coord_sf(datum = NA)
ggsave(here("figures", "segmentize-f.png"), width = 3, height = 4.5, units = "in", dpi = 400)

e2 <- ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(data = vor, aes(fill = Ridership), color = NA) +
  geom_sf(data = new_fc_seg, color = "black") +
  ggtitle("E") +
  scale_fill_manual("Transit", values = c("#666666", "#669999")) +
  coord_sf(datum = NA)

png(here("figures", "segmentize-def.png"), width = 7, height = 4.5, units = "in", res = 400)
gridExtra::grid.arrange(d, e2, f, ncol = 3)
dev.off()

# Suburban areal strata
ns <- c(HHH = "1.1.1", HHL = "1.1.0", HLH = "1.0.1",
        LHH = "0.1.1", LLH = "0.0.1", HLL = "1.0.0",
        LHL = "0.1.0", LLL = "0.0.0")
sub_areal <- st_read(here("outputs", "sub_strata.shp")) %>%
  mutate(new_stratum = fct_recode(stratum, !!!ns))
ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = sub_areal, aes(fill = new_stratum), color = NA) +
  ggtitle("Census tract sampling strata, four suburban counties") +
  scale_fill_viridis_d("Stratum", option = "cividis") +
  coord_sf(datum = NA)
ggsave(here("figures", "suburb-areal.png"), width = 10, height = 7.5, units = "in", dpi = 400)

# Philadelphia areal strata 
strata <- read_csv(here("outputs", "ind_vars.csv")) %>%
  select(GEOID, pct_coll, pop_dens, bl_pov, ta_dens, road_dens) %>%
  mutate_at(vars(matches("GEOID")), as.factor) %>%
  mutate(stcty = substr(GEOID, 1, 5)) %>%
  drop_na()
all_shp <- st_read(here("outputs", "./act_peds.shp")) %>%
  select(GEOID, e_wlk_den) %>%
  mutate(cty = str_sub(GEOID, 3, 5)) %>%
  st_transform(26918)
phila_shp <- strata %>%
  filter(stcty == "42101") %>%
  mutate_if(is.numeric, funs(ifelse(. > median(.), 1, 0))) %>%
  mutate(stratum = paste(pct_coll, ta_dens, sep = ".")) %>%
  select(GEOID, stratum) %>%
  inner_join(all_shp, .)
ns <- c(LL = "0.0", LH = "0.1", HL = "1.0", HH = "1.1")
phila_shp <- phila_shp %>%
  mutate(new_stratum = fct_recode(stratum, !!!ns))

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = phila, fill = "gainsboro", color = NA) +
  geom_sf(data = phila_shp, aes(fill = new_stratum), color = NA) +
  geom_sf(data = h2o_phila, fill = "#669999", color = NA) +
  geom_sf(data = phila, fill = NA, color = "gray") +
  ggtitle("Census tract sampling strata, Philadelphia") +
  scale_fill_viridis_d("Stratum", option = "cividis") +
  coord_sf(datum = NA)
ggsave(here("figures", "phila-areal.png"), width = 4.5, height = 4.5, units = "in", dpi = 400)

# Plot philadelphia transit arterials
phila_arterial <- st_read(here("outputs", "phila_transit_arterial_strata.shp")) %>%
  mutate(stratum = case_when(ab_med == 1 ~ "High Transit",
                             ab_med == 0 ~ "Low Transit"))
crop_box <- st_bbox(c(xmin = 481404, ymin = 4419022.5, xmax = 490035, ymax = 4427653.5))
phila_crop <- st_crop(phila, crop_box)
h2o_phila_crop <- st_crop(h2o_phila, crop_box)
phila_arterial_crop <- st_crop(phila_arterial, crop_box)

ggplot() +
  theme(text = element_text(family = "CMU Serif")) +
  theme(panel.background = element_blank()) +
  geom_sf(data = phila_crop, fill = "gainsboro", color = "gray") +
  geom_sf(data = h2o_phila_crop, fill = "#669999", color = NA) +
  geom_sf(data = phila_arterial_crop, aes(color = stratum)) +
  ggtitle("Transit arterial sampling strata, Philadelphia") +
  scale_color_viridis_d("Stratum", option = "cividis") +
  coord_sf(datum = NA)
ggsave(here("figures", "phila-arterial.png"), width = 5, height = 3, units = "in", dpi = 400)
