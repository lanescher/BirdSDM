
#############################################################################


#Plots
#AUC boxplots?
boxplot(ci.auc(roc1), ci.auc(roc2), ci.auc(roc3), at=c(1,2,3), names=c("M1","M2","M3"), main="AUC")


#Distribution
library(ggplot2)
library(sf)
library(raster)
library(exactextractr)
library(RColorBrewer)

#Load rasters
elev <- raster("S:/MillerLab/Projects/BirdSDM/GIS/elev_30mDEM/elev2016proj.tif")
can <- raster("S:/MillerLab/Projects/BirdSDM/GIS/nlcd_2016_treecanopy_2019_08_31/can2016proj.tif")
temp <- raster("S:/MillerLab/Projects/BirdSDM/GIS/Climate/temp2016proj.tif")
ppt <- raster("S:/MillerLab/Projects/BirdSDM/GIS/Climate/ppt2016proj.tif")
dev <-  raster("S:/MillerLab/Projects/BirdSDM/GIS/NLCD_2016_Land_Cover_L48_20190424/devlan2016proj.tif")
forest <- raster("S:/MillerLab/Projects/BirdSDM/GIS/NLCD_2016_Land_Cover_L48_20190424/allfor2016proj.tif")
slope <- raster("S:/MillerLab/Projects/BirdSDM/GIS/slope2016proj.tif")
road <- st_read("S:/MillerLab/Projects/BirdSDM/GIS/Roads/PAroads2.shp")
road <- st_transform(road, crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
road <- road[c(2:3,6)]
road <- tibble::rowid_to_column(road, "ID")

#Create grid
pa <- st_read("S:/MillerLab/Projects/BirdSDM/GIS/Pennsylvania_State_Boundary/PaState2020_01.shp")
pa <- st_transform(pa, crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
grid_pa <- pa %>%
  st_make_grid(cellsize=1000) %>%
  st_intersection(pa) %>%
  st_as_sf() %>%
  mutate(id = 1:nrow(.))
grid2 <- grid_pa
grid3 <- grid_pa

#Get centroids for extraction; concert grid to spatial to get point layer
grid_cen <- st_coordinates(st_centroid(grid_pa))
grid_cen2 <- as.data.frame(grid_cen)
grid_pt <- st_as_sf(grid_cen2, coords=c("X","Y"))
grid_pt <- st_set_crs(grid_pt, "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

#Create grid internal buffers
gridbuff <- grid_pt %>% 
  st_buffer(dist=400) %>% 
  st_as_sf() %>%
  mutate(id = 1:nrow(.))

#Road
intersection <- st_intersection(gridbuff, road) %>% 
  mutate(length = st_length(.)) %>% 
  st_drop_geometry()
int_length <- intersection %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(length = sum(length))
grid2 <- left_join(gridbuff, int_length, by = c("id"))
grid2$road <- log((as.numeric(grid2$length) * 0.3048) + 1)
grid3$road <- grid2$road

#Raster extraction at point level
grid3$elev <- raster::extract(elev, grid_cen)
grid3$elev <- scale(grid3$elev, center=352.4586, scale=160.7967)
grid3$temp <- raster::extract(temp, grid_cen)
grid3$temp <- scale(grid3$temp, center=9.408352, scale=1.415427)
grid3$ppt <- raster::extract(ppt, grid_cen)
grid3$ppt <- scale(grid3$ppt, center=1102.585, scale=82.26033)
grid3$slope <- raster::extract(slope, grid_cen)
grid3$slope <- scale(grid3$slope, center=5.130878, scale=4.347548)

#Raster extraction at buffer level
grid3$can <- exactextractr::exact_extract(can, gridbuff, fun = 'mean')
grid3$can <- scale(grid3$can, center=43.68183, scale=25.17216)
grid3$dev <- exactextractr::exact_extract(dev, gridbuff, fun = 'count')
grid3$dev <- (grid3$dev * 900)/502654.825
grid3$dev <- scale(grid3$dev, center=0.2299, scale=0.2136447)
grid3$forest <- exactextractr::exact_extract(forest, gridbuff, fun = 'count')
grid3$forest <- (grid3$forest * 900)/502654.825
grid3$forest <- scale(grid3$forest, center=0.6011573, scale=0.3303459)

#Get lat long
latlong <- st_coordinates(st_transform(st_centroid(grid_pa), 4326))
grid3$latitude <- latlong[,2]
grid3$longitude <- latlong[,1]

#Convert NAs to 0s
grid3[,3:12][is.na(grid3[,3:12])] <- 0


#Save out grids, 1k
save(grid_pa, grid2, grid3, grid_cen, grid_pt, gridbuff, intersection, int_length,  
     file = "FinalResults/Plots/grid_pa_1km.RData")


##########

###Generate predictive map (load jam first)
grid3$pred <- predict(jam1, newdata = grid3, type="response")
grid_sp <- as_Spatial(grid3)

#Change map extent
scale.parameter = 1.1
original.bbox = grid_sp@bbox 
edges = original.bbox
edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1, 
])
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2, 
])

#Plot
pdf("FinalResults/Plots/cawam1map.pdf")
spplot(grid_sp, "pred", col=NA, par.settings = list(axis.line = list(col = 'transparent')),
       xlim = edges[1, ], ylim = edges[2, ], colorkey=list(height=0.6, cex=0.5))
dev.off()




##Adjust new grid for covariate model (old thesis stuff currently)

grid4 <- grid3
load("S:/MillerLab/Projects/BirdSDM/Scripts/datasetup2_3.RData")

gridbuff2 <- grid_pt %>% 
  st_buffer(dist=1000) %>% 
  st_as_sf() %>%
  mutate(id = 1:nrow(.))

lists_ebd <- sf:::aggregate.sf(ebdpoint2["checklist_id"], by=gridbuff2$geometry, FUN = length)
lists_ebd <- st_drop_geometry(lists_ebd)
grid4$lists_ebd <- lists_ebd$checklist_id

lists_bba <- sf:::aggregate.sf(bbapoint["BBA_ID"], by=gridbuff2$geometry, FUN = length)
lists_bba <- st_drop_geometry(lists_bba)
grid4$lists_bba <- lists_bba$BBA_ID

lists_bbs <- sf:::aggregate.sf(bbspoint["X"], by=gridbuff2$geometry, FUN = length)
lists_bbs <- st_drop_geometry(lists_bbs)
grid4$lists_bbs <- lists_bbs$X

gridcawa <- grid4
cawact_ebd <- sf:::aggregate.sf(ebdpoint2["cawacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
cawact_ebd <- st_drop_geometry(cawact_ebd)
gridcawa$cawact_ebd <- cawact_ebd$cawacount
cawact_bba <- sf:::aggregate.sf(bbapoint["cawacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
cawact_bba <- st_drop_geometry(cawact_bba)
gridcawa$cawact_bba <- cawact_bba$cawacount
cawact_bbs <- sf:::aggregate.sf(bbspoint["cawacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
cawact_bbs <- st_drop_geometry(cawact_bbs)
gridcawa$cawact_bbs <- cawact_bbs$cawacount

gridcerw <- grid4
cerwct_ebd <- sf:::aggregate.sf(ebdpoint2["cerwcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
cerwct_ebd <- st_drop_geometry(cerwct_ebd)
gridcerw$cerwct_ebd <- cerwct_ebd$cerwcount
cerwct_bba <- sf:::aggregate.sf(bbapoint["cerwcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
cerwct_bba <- st_drop_geometry(cerwct_bba)
gridcerw$cerwct_bba <- cerwct_bba$cerwcount
cerwct_bbs <- sf:::aggregate.sf(bbspoint["cerwcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
cerwct_bbs <- st_drop_geometry(cerwct_bbs)
gridcerw$cerwct_bbs <- cerwct_bbs$cerwcount

gridgwwa <- grid4
gwwact_ebd <- sf:::aggregate.sf(ebdpoint2["gwwacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
gwwact_ebd <- st_drop_geometry(gwwact_ebd)
gridgwwa$gwwact_ebd <- gwwact_ebd$gwwacount
gwwact_bba <- sf:::aggregate.sf(bbapoint["gwwacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
gwwact_bba <- st_drop_geometry(gwwact_bba)
gridgwwa$gwwact_bba <- gwwact_bba$gwwacount
gwwact_bbs <- sf:::aggregate.sf(bbspoint["gwwacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
gwwact_bbs <- st_drop_geometry(gwwact_bbs)
gridgwwa$gwwact_bbs <- gwwact_bbs$gwwacount

gridwoth <- grid4
wothct_ebd <- sf:::aggregate.sf(ebdpoint2["wothcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
wothct_ebd <- st_drop_geometry(wothct_ebd)
gridwoth$wothct_ebd <- wothct_ebd$wothcount
wothct_bba <- sf:::aggregate.sf(bbapoint["wothcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
wothct_bba <- st_drop_geometry(wothct_bba)
gridwoth$wothct_bba <- wothct_bba$wothcount
wothct_bbs <- sf:::aggregate.sf(bbspoint["wothcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
wothct_bbs <- st_drop_geometry(wothct_bbs)
gridwoth$wothct_bbs <- wothct_bbs$wothcount


gridcawa[,13:18][is.na(gridcawa[,13:18])] <- 0
gridcerw[,13:18][is.na(gridcerw[,13:18])] <- 0
gridgwwa[,13:18][is.na(gridgwwa[,13:18])] <- 0
gridwoth[,13:18][is.na(gridwoth[,13:18])] <- 0


save(gridbuff2, grid4, gridcawa, gridcerw, gridgwwa, gridwoth, 
     file = "FinalResults/Plots/grid_pa_1km_v2.RData")

