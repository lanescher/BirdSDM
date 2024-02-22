
## Objective ---------------------------
## To create maps of predicted occurrence for four species
##
## 
## Input:
##    data/covariates.Rdata
##    data/grid_pa_1km.RData
##    results/jams/cawam1jam.RData
##    results/jams/cerwm1jam.RData
##    results/jams/gwwam1jam.RData
##    results/jams/wothm1jam.RData
##
## Output: 
##    outputs/fig3.jpg
##
## ---------------------------


## load packages ---------------------------
library(tidyverse)
library(sf)
library(raster)
library(exactextractr)
library(RColorBrewer)
library(mgcv)
library(cowplot)


# ## load data ---------------------------
# # Load rasters
# elev <- raster("covariates/elev_30mDEM/elev2016proj.tif")
# can <- raster("covariates/nlcd_2016_treecanopy_2019_08_31/can2016proj.tif")
# temp <- raster("covariates/PRISM_tmean_30yr/temp2016proj.tif")
# ppt <- raster("covariates/PRISM_ppt_30yr/ppt2016proj.tif")
# dev <-  raster("covariates/NLCD_2016_Land_Cover_L48_20190424/devlan2016proj.tif")
# forest <- raster("covariates/NLCD_2016_Land_Cover_L48_20190424/allfor2016proj.tif")
# slope <- raster("covariates/slope2016proj.tif")
# road <- st_read("covariates/Roads/PAroads2.shp") %>%
#           st_transform(road, crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# road <- road[c(2:3,6)]
# road <- tibble::rowid_to_column(road, "ID")
# 
# pa <- st_read("covariates/Pennsylvania_State_Boundary/PaState2020_01.shp") %>%
#   st_transform(crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# 
# save(elev, can, temp, ppt, dev, forest, slope, road, pa,
#      file = 'data/covariates.Rdata')



## Create grid ---------------------------
load('data/covariates.Rdata')

grid_pa <- grid2 <- grid3 <- pa %>%
                              st_make_grid(cellsize=1000) %>%
                              st_intersection(pa) %>%
                              st_as_sf() %>%
                              mutate(id = 1:nrow(.))


## Get centroids for extraction ---------------------------
# concert grid to spatial to get point layer
grid_cen <- st_coordinates(st_centroid(grid_pa))
grid_cen2 <- as.data.frame(grid_cen)
grid_pt <- st_as_sf(grid_cen2, coords=c("X","Y")) %>%
            st_set_crs("+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

## Create grid internal buffers ---------------------------
gridbuff <- grid_pt %>% 
            st_buffer(dist=400) %>% 
            st_as_sf() %>%
            mutate(id = 1:nrow(.))


# Road ---------------------------
intersection <- st_intersection(gridbuff, road) %>%
                  mutate(length = st_length(.)) %>%
                  st_drop_geometry()
int_length <- intersection %>%
              dplyr::group_by(id) %>%
              dplyr::summarise(length = sum(length))
grid2 <- left_join(gridbuff, int_length, by = c("id"))
grid2$road <- log((as.numeric(grid2$length) * 0.3048) + 1)
grid3$road <- grid2$road


## Raster extraction at point level ---------------------------
# Elevation
grid3$elev <- raster::extract(elev, grid_cen)
grid3$elev <- scale(grid3$elev, center=352.4586, scale=160.7967)[,1]
# Temperature
grid3$temp <- raster::extract(temp, grid_cen)
grid3$temp <- scale(grid3$temp, center=9.408352, scale=1.415427)[,1]
# Precipitation
grid3$ppt <- raster::extract(ppt, grid_cen)
grid3$ppt <- scale(grid3$ppt, center=1102.585, scale=82.26033)[,1]
# Slope
grid3$slope <- raster::extract(slope, grid_cen)
grid3$slope <- scale(grid3$slope, center=5.130878, scale=4.347548)[,1]

## Raster extraction at buffer level ---------------------------
# THIS SECTION TAKES AWHILE TO RUN
# Canopy cover
grid3$can <- exactextractr::exact_extract(can, gridbuff, fun = 'mean')
grid3$can <- scale(grid3$can, center=43.68183, scale=25.17216)[,1]
# Developed land
grid3$dev <- exactextractr::exact_extract(dev, gridbuff, fun = 'count')
grid3$dev <- (grid3$dev * 900)/502654.825
grid3$dev <- scale(grid3$dev, center=0.2299, scale=0.2136447)[,1]
# Forested land
grid3$forest <- exactextractr::exact_extract(forest, gridbuff, fun = 'count')
grid3$forest <- (grid3$forest * 900)/502654.825
grid3$forest <- scale(grid3$forest, center=0.6011573, scale=0.3303459)[,1]

## Get lat long ---------------------------
latlong <- st_coordinates(st_transform(st_centroid(grid_pa), 4326))
grid3$latitude <- latlong[,2]
grid3$longitude <- latlong[,1]

## Convert NAs to 0s
# grid3[,3:12][is.na(grid3[,3:12])] <- 0

#Save out grids, 1k
save(grid_pa, grid2, grid3, grid_cen, grid_pt, 
     gridbuff, intersection, int_length,
     file = "data/grid_pa_1km.RData")


## Generate predictive map ---------------------------

load('data/grid_pa_1km.RData')

spp <- c('cawa','cerw','gwwa','woth')

for (i in 1:4) {
  load(paste0('results/jams/',spp[i],'m1jam.RData'))
  
  grid3$pred <- predict(jam, newdata = grid3, type="response")
  
  colnames(grid3)[ncol(grid3)] <- spp[i]
}


# Canadian warbler
ggplot(grid3) +
  geom_sf(aes(fill=cawa, col=cawa)) +
  scale_fill_viridis_c(option = "inferno") +
  scale_color_viridis_c(option = "inferno") +
  theme_void() +
  theme(legend.title = element_blank()) -> cawa

# Cerulean warbler
ggplot(grid3) +
  geom_sf(aes(fill=cerw, col=cerw)) +
  scale_fill_viridis_c(option = "inferno") +
  scale_color_viridis_c(option = "inferno") +
  theme_void() +
  theme(legend.title = element_blank()) -> cerw

# Golden-winged warbler
ggplot(grid3) +
  geom_sf(aes(fill=gwwa, col=gwwa)) +
  scale_fill_viridis_c(option = "inferno") +
  scale_color_viridis_c(option = "inferno") +
  theme_void() +
  theme(legend.title = element_blank()) -> gwwa

# Wood thrush
ggplot(grid3) +
  geom_sf(aes(fill=woth, col=woth)) +
  scale_fill_viridis_c(option = "inferno") +
  scale_color_viridis_c(option = "inferno") +
  theme_void() +
  theme(legend.title = element_blank()) -> woth




plot_grid(cawa, cerw, gwwa, woth, ncol = 2, labels = 'auto')

ggsave(filename = 'outputs/fig3.jpg')


# End script
