library(rgdal); library(tidyverse); library(gstat); library(raster)
library(sp); library(stargazer); library(spatialEco); library(here)
# Note that spsample makes correlations change with every run

aadp <- readOGR(here("proprietary"), "ped_counters", stringsAsFactors = FALSE)
aadpTxt <- as.data.frame(aadp)
uniqueCoords <- aadpTxt[!duplicated(aadpTxt[13:14]),] # 1127 counts, in 549 unique places
aadpTxt$AADP <- as.numeric(as.character(aadpTxt$AADP))
# Collapse multiple observations in a single coordinate point to the mean count
aadpTxt$pasteCoords <- paste0(aadpTxt$LATITUDE, "_", aadpTxt$LONGITUDE)
aadpCollapse <- aggregate(aadpTxt$AADP,
                          by = list(aadpTxt$pasteCoords),
                          FUN = mean, na.rm = TRUE)
# Now that we have an average pedestrian count by location,
# Attach it back to aadp and then cut down the SPDF
colnames(aadpCollapse) <- c("pasteCoords", "newAADP")
aadpTxt <- merge(aadpCollapse, aadpTxt, by = "pasteCoords")
uniqueAADP <- aadpTxt[!duplicated(aadpTxt$pasteCoords),]
uniqueAADP <- uniqueAADP[c("OBJECTID", "newAADP")]
aadp <- merge(aadp, uniqueAADP, by = "OBJECTID")
# Remove all observations with comments, since their ped volumes are outliers.
aadp$COMMENTS[!is.na(aadp$COMMENTS)] <- "REMOVE"
aadp$COMMENTS[is.na(aadp$COMMENTS)] <- "KEEP"
aadp$COMMENTS[aadp$COMMENTS == "REMOVE"] <- NA
aadp <- aadp[c("OBJECTID", "AADP", "newAADP", "LATITUDE", "LONGITUDE")]
aadp <- sp.na.omit(aadp)
# Also remove counters within 100m of a trail
trails <- readOGR(here("proprietary"), "circuit_trails_buff")
aadp$present <- over(aadp, trails)$Id
aadp$present[is.na(aadp$present)] <- 1
aadp[aadp$present == 0] <- NA
aadp <- sp.na.omit(aadp)

grd <- as.data.frame(spsample(aadp, "regular", n = 100000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd) <- TRUE
fullgrid(grd) <- TRUE
proj4string(grd) <- proj4string(aadp)
myIDW <- gstat::idw(newAADP ~ 1, aadp, grd, idp = 1.5)

# Leave-one-out validation routine, see https://mgimond.github.io/Spatial/interpolation-in-r.html
IDW.out <- vector(length = length(aadp))
for (i in 1:length(aadp)) {
  IDW.out[i] <- idw(newAADP ~ 1, aadp[-i,], aadp[i,], idp = 1.5)$var1.pred
}
# Plot the differences
OP <- par(pty="s", mar=c(4,3,0,0))
plot(IDW.out ~ aadp$newAADP, asp=1, xlab="Observed", ylab="Predicted", pch=16,
     col=rgb(0,0,0,0.5))
abline(lm(IDW.out ~ aadp$newAADP), col="red", lw=2,lty=2)
abline(0,1)
par(OP)
# Compute RMSE
sqrt( sum((IDW.out - aadp$newAADP)^2) / length(aadp))
# RMSE of:
# idp = 2,    1712.376
# idp = 1.5,  1601.164 # SELECT THIS ONE
rastIDW <- raster(myIDW)
writeRaster(rastIDW, here("outputs", "act_peds.tif"), overwrite = TRUE)

# Use zonal statistics to get the mean counted pedestrian activity by tract
dvrpc <- readOGR(here("outputs"), "est_peds", stringsAsFactors = FALSE)
dvrpc <- spTransform(dvrpc, proj4string(rastIDW))
dvrpc$GEOID <- as.numeric(dvrpc$GEOID)
dvrpcRast <- rasterize(dvrpc, rastIDW, dvrpc@data$GEOID)
dvrpcByCell <- as.data.frame(dvrpcRast)
dvrpcByCell$id <- seq_len(nrow(dvrpcByCell))
idwByCell <- as.data.frame(rastIDW)
idwByCell$id <- seq_len(nrow(idwByCell))
tractVolume <- merge(dvrpcByCell, idwByCell, by = "id")
tractVolume <- aggregate(tractVolume$var1.pred, list(tractVolume$layer), FUN = mean)
colnames(tractVolume) <- c("GEOID", "wlk_cnt_d")
dvrpc <- merge(dvrpc, tractVolume, by = "GEOID")

summary(dvrpc$wlk_tr_d); summary(dvrpc$wlk_cnt_d)

library(sf); library(tidyverse); library(magrittr)
dvrpc %<>% st_as_sf(.) %>%
  mutate(sqmi = as.numeric(st_area(.) * 0.00000038610),
         e_wlk_den = wlk_tr_d / sqmi,
         a_wlk_den = wlk_cnt_d / sqmi) %>%
  mutate_at(c("GEOID"), as.character)
st_write(dvrpc, here("outputs", "act_peds.shp"), delete_dsn = TRUE)
dvrpc %<>% st_set_geometry(NULL)
write.csv(dvrpc, here("outputs", "act_peds.csv"), row.names = FALSE)

dvrpc <- dvrpc[c(5,6)]
cor(dvrpc, use = "pairwise.complete.obs")
# 0.8174412
