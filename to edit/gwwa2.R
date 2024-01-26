#Golden-winged Warbler models 8-11
setwd("S:/MillerLab/Projects/BirdSDM")

library(plyr)
library(dplyr)
library(tibble)
library(mgcv)
library(jagsUI)
library(ROCR)

#Load data
covdat <- read.csv("FinalData/TestTrain/covdat.csv")
covdat2 <- read.csv("FinalData/TestTrain/covdat2.csv")

#Model 8: gwwa (covariate model)
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836

m8 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + gwwact_ebd + lists + s(longitude, latitude, bs='ds', k=100), 
            data = covdat, family='binomial', file = 'placeholder.txt')

datm8 <- list(y = c(covdat$gwwatot[117937:144939], covdat$gwwadet[144940:177616]), 
              X = m8$jags.data$X, n = m8$jags.data$n, zero = m8$jags.data$zero,
              S1 = m8$jags.data$S1, S2 = m8$jags.data$S2, S3 = m8$jags.data$S3,
              S4 = m8$jags.data$S4, S5 = m8$jags.data$S5, S6 = m8$jags.data$S6,
              S7 = m8$jags.data$S7, S8 = m8$jags.data$S8, S9 = m8$jags.data$S9,
              ehours2 = covdat$duration_minutes[177617:186833], 
              ekm2 = covdat$effort_distance_km[177617:186833], 
              hsm = covdat$hsm[117937:144939],
              hsm2 = covdat$hsm[186834:193563], 
              doy = covdat$doy[117937:144939],
              doy2 = covdat$doy[186834:193563],
              doy3 = covdat$doy[144940:177616],
              doy4 = covdat$doy[193564:201836])

initsm8 <- function(){
  list(b = m8$jags.ini$b, lambda = m8$jags.ini$lambda,  beta = rep(3, 0.001), 
       z = c(rep(NA, 117936), as.numeric(datm8$y>0)))
}

outm8 <- jags(data = datm8, parameters.to.save = c("beta","b","y2"), inits = initsm8, 
              model.file = "FinalModels/gwwam8.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m8_yp <- outm8$mean$y2
m8_yt <- c(covdat$gwwadet[177617:186833], covdat$gwwatot[186834:193563], covdat$gwwadet[193564:201836])
m8_yt <- cbind(m8_yt, c(1 - covdat$gwwadet[177617:186833], 5 - covdat$gwwatot[186834:193563], 1 - covdat$gwwadet[193564:201836]))
m8_yp[9218:15947] <- m8_yp[9218:15947]/5
m8_yp <- 0.0001 + m8_yp*0.9998
m8_dev <- -2*sum(log((m8_yp^m8_yt[,1])*((1-m8_yp)^(m8_yt[,2]))))
#eBird deviance
Em8_yp <- outm8$mean$y2[1:9217]
Em8_yt <- c(covdat$gwwadet[177617:186833])
Em8_yt <- cbind(Em8_yt, c(1 - covdat$gwwadet[177617:186833]))
Em8_yp <- 0.0001 + Em8_yp*0.9998
Em8_dev <- -2*sum(log((Em8_yp^Em8_yt[,1])*((1-Em8_yp)^(Em8_yt[,2]))))
#BBA deviance
Am8_yp <- outm8$mean$y2[9218:15947]
Am8_yt <- c(covdat$gwwatot[186834:193563])
Am8_yt <- cbind(Am8_yt, c(5 - covdat$gwwatot[186834:193563])) 
Am8_yp <- Am8_yp/5
Am8_yp <- 0.0001 + Am8_yp*0.9998
Am8_dev <- -2*sum(log((Am8_yp^Am8_yt[,1])*((1-Am8_yp)^(Am8_yt[,2]))))
#BBS deviance
Sm8_yp <- outm8$mean$y2[15948:24220]
Sm8_yt <- c(covdat$gwwadet[193564:201836])
Sm8_yt <- cbind(Sm8_yt, c(1 - covdat$gwwadet[193564:201836]))
Sm8_yp <- 0.0001 + Sm8_yp*0.9998
Sm8_dev <- -2*sum(log((Sm8_yp^Sm8_yt[,1])*((1-Sm8_yp)^(Sm8_yt[,2]))))

save(outm8, m8, m8_dev, Em8_dev, Am8_dev, Sm8_dev, 
     file = "FinalResults/gwwam8.RData")

brier8 <- mean((outm8$mean$y2[9218:24220] - covdat$gwwadet[186834:201836])^2)

pred8 <- prediction(as.numeric(outm8$mean$y2[9218:24220]), covdat$gwwadet[186834:201836])
auc8 <- performance(pred8, measure = "auc")
auc8 <- auc8@y.values[[1]]



###### Model 9 removed #######




###Model 10: gwwa (no eBird)
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616,
#test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836,

m10 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
             data = covdat, family='binomial', file = 'placeholder.txt')

datm10 <- list(y = c(covdat$gwwatot[117937:144939], covdat$gwwadet[144940:177616]), 
               X = m10$jags.data$X, n = m10$jags.data$n, zero = m10$jags.data$zero,
               S1 = m10$jags.data$S1, S2 = m10$jags.data$S2, S3 = m10$jags.data$S3,
               S4 = m10$jags.data$S4, S5 = m10$jags.data$S5, S6 = m10$jags.data$S6,
               S7 = m10$jags.data$S7, S8 = m10$jags.data$S8, S9 = m10$jags.data$S9,
               ehours2 = covdat$duration_minutes[177617:186833], 
               ekm2 = covdat$effort_distance_km[177617:186833], 
               hsm = covdat$hsm[117937:144939],
               hsm2 = covdat$hsm[186834:193563], 
               doy = covdat$doy[117937:144939],
               doy2 = covdat$doy[186834:193563],
               doy3 = covdat$doy[144940:177616],
               doy4 = covdat$doy[193564:201836])

initsm10 <- function(){
  list(b = m10$jags.ini$b, lambda = m10$jags.ini$lambda,  beta = rep(3, 0.001), 
       z = c(rep(NA, 117936), as.numeric(datm10$y>0)))
}

outm10 <- jags(data = datm10, parameters.to.save = c("beta","b","y2"), inits = initsm10, 
               model.file = "FinalModels/gwwam10.txt",  
               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m10_yp <- outm10$mean$y2
m10_yt <- c(covdat$gwwadet[177617:186833], covdat$gwwatot[186834:193563], covdat$gwwadet[193564:201836])
m10_yt <- cbind(m10_yt, c(1 - covdat$gwwadet[177617:186833], 5 - covdat$gwwatot[186834:193563], 1 - covdat$gwwadet[193564:201836]))
m10_yp[9218:15947] <- m10_yp[9218:15947]/5
m10_yp <- 0.0001 + m10_yp*0.9998
m10_dev <- -2*sum(log((m10_yp^m10_yt[,1])*((1-m10_yp)^(m10_yt[,2]))))
#eBird deviance
Em10_yp <- outm10$mean$y2[1:9217]
Em10_yt <- c(covdat$gwwadet[177617:186833])
Em10_yt <- cbind(Em10_yt, c(1 - covdat$gwwadet[177617:186833]))
Em10_yp <- 0.0001 + Em10_yp*0.9998
Em10_dev <- -2*sum(log((Em10_yp^Em10_yt[,1])*((1-Em10_yp)^(Em10_yt[,2]))))
#BBA deviance
Am10_yp <- outm10$mean$y2[9218:15947]
Am10_yt <- c(covdat$gwwatot[186834:193563])
Am10_yt <- cbind(Am10_yt, c(5 - covdat$gwwatot[186834:193563])) 
Am10_yp <- Am10_yp/5
Am10_yp <- 0.0001 + Am10_yp*0.9998
Am10_dev <- -2*sum(log((Am10_yp^Am10_yt[,1])*((1-Am10_yp)^(Am10_yt[,2]))))
#BBS deviance
Sm10_yp <- outm10$mean$y2[15948:24220]
Sm10_yt <- c(covdat$gwwadet[193564:201836])
Sm10_yt <- cbind(Sm10_yt, c(1 - covdat$gwwadet[193564:201836]))
Sm10_yp <- 0.0001 + Sm10_yp*0.9998
Sm10_dev <- -2*sum(log((Sm10_yp^Sm10_yt[,1])*((1-Sm10_yp)^(Sm10_yt[,2]))))

save(outm10, m10, m10_dev, Em10_dev, Am10_dev, Sm10_dev, 
     file = "FinalResults/gwwam10.RData")

brier10 <- mean((outm10$mean$y2[9218:24220] - covdat$gwwadet[186834:201836])^2)

pred10 <- prediction(as.numeric(outm10$mean$y2[9218:24220]), covdat$gwwadet[186834:201836])
auc10 <- performance(pred10, measure = "auc")
auc10 <- auc10@y.values[[1]]



###Model 11: gwwa (stationary shared, no travelling)
#train: eBird stat- 1:14725, BBA- 14726:41728, BBS- 41729:74405
#test: eBird- 74406:83622, BBA- 83623:90352, BBS- 90353:98625

m11 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
             data = covdat2, family='binomial', file = 'placeholder.txt')

datm11 <- list(y = c(covdat2$gwwadet[1:14725], covdat2$gwwatot[14726:41728], covdat2$gwwadet[41729:74405]), 
               X = m11$jags.data$X, n = m11$jags.data$n, zero = m11$jags.data$zero,
               S1 = m11$jags.data$S1, S2 = m11$jags.data$S2, S3 = m11$jags.data$S3,
               S4 = m11$jags.data$S4, S5 = m11$jags.data$S5, S6 = m11$jags.data$S6,
               S7 = m11$jags.data$S7, S8 = m11$jags.data$S8, S9 = m11$jags.data$S9,
               ehours = covdat2$duration_minutes[1:14725], 
               ehours2 = covdat2$duration_minutes[74406:83622], 
               hsm = covdat2$hsm[14726:41728],
               hsm2 = covdat2$hsm[83623:90352], 
               doy = covdat2$doy[14726:41728],
               doy2 = covdat2$doy[83623:90352],
               doy3 = covdat2$doy[41729:74405],
               doy4 = covdat2$doy[90353:98625])

initsm11 <- function(){
  list(b = m11$jags.ini$b, lambda = m11$jags.ini$lambda,  beta = rep(3, 0.001), 
       z = as.numeric(datm11$y>0))
}

outm11 <- jags(data = datm11, parameters.to.save = c("beta","b","y2"), inits = initsm11, 
               model.file = "FinalModels/gwwam11.txt", 
               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500, parallel=TRUE)

#Full Deviance
m11_yp <- outm11$mean$y2
m11_yt <- c(covdat2$gwwadet[74406:83622], covdat2$gwwatot[83623:90352], covdat2$gwwadet[90353:98625])
m11_yt <- cbind(m11_yt, c(1 - covdat2$gwwadet[74406:83622], 5 - covdat2$gwwatot[83623:90352], 1 - covdat2$gwwadet[90353:98625]))
m11_yp[9218:15947] <- m11_yp[9218:15947]/5
m11_yp <- 0.0001 + m11_yp*0.9998
m11_dev <- -2*sum(log((m11_yp^m11_yt[,1])*((1-m11_yp)^(m11_yt[,2]))))
#eBird deviance
Em11_yp <- outm11$mean$y2[1:9217]
Em11_yt <- c(covdat2$gwwadet[74406:83622])
Em11_yt <- cbind(Em11_yt, c(1 - covdat2$gwwadet[74406:83622]))
Em11_yp <- 0.0001 + Em11_yp*0.9998
Em11_dev <- -2*sum(log((Em11_yp^Em11_yt[,1])*((1-Em11_yp)^(Em11_yt[,2]))))
#BBA deviance
Am11_yp <- outm11$mean$y2[9218:15947]
Am11_yt <- c(covdat2$gwwatot[83623:90352])
Am11_yt <- cbind(Am11_yt, c(5 - covdat2$gwwatot[83623:90352])) 
Am11_yp <- Am11_yp/5
Am11_yp <- 0.0001 + Am11_yp*0.9998
Am11_dev <- -2*sum(log((Am11_yp^Am11_yt[,1])*((1-Am11_yp)^(Am11_yt[,2]))))
#BBS deviance
Sm11_yp <- outm11$mean$y2[15948:24220]
Sm11_yt <- c(covdat2$gwwadet[90353:98625])
Sm11_yt <- cbind(Sm11_yt, c(1 - covdat2$gwwadet[90353:98625]))
Sm11_yp <- 0.0001 + Sm11_yp*0.9998
Sm11_dev <- -2*sum(log((Sm11_yp^Sm11_yt[,1])*((1-Sm11_yp)^(Sm11_yt[,2]))))

save(outm11, m11, m11_dev, Em11_dev, Am11_dev, Sm11_dev, 
     file = "FinalResults/gwwam11_2.RData")

brier11 <- mean((outm11$mean$y2[9218:24220] - covdat2$gwwadet[83623:98625])^2)

pred11 <- prediction(as.numeric(outm11$mean$y2[9218:24220]), covdat2$gwwadet[83623:98625])
auc11 <- performance(pred11, measure = "auc")
auc11 <- auc11@y.values[[1]]



#################################
#Sim2Jam, model 11 (aka 10)
require(rjags)
jm11 <- jags.model("FinalModels/gwwam11.txt", data=datm11, inits=initsm11, n.adapt=500, n.chains=3)
update(jm11, 500)
sam11 <- jags.samples(jm11, c("b","rho"), n.iter=2000, thin=2)
jam11 <- sim2jam(sam11, m11$pregam)

save(m11, jm11, sam11, jam11,
     file = "FinalResults/Plots/gwwam11_jam.RData")

pdf("FinalResults/Plots/gwwa11cov.pdf")
plot(jam11, pages=1)
dev.off()

load("S:/MillerLab/Projects/BirdSDM/FinalResults/Plots/grid_pa_1km.RData")
library(ggplot2)
library(sf)
library(sp)
library(RColorBrewer)

#Generate predictions
grid3$pred <- predict(jam11, newdata = grid3, type="response")
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
pdf("FinalResults/Plots/gwwa11map.pdf")
spplot(grid_sp, "pred", col=NA, par.settings = list(axis.line = list(col = 'transparent')),
       xlim = edges[1, ], ylim = edges[2, ], colorkey=list(height=0.6, cex=0.5))
dev.off()


