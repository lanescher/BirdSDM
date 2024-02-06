





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

