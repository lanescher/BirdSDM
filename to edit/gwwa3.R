#Golden-winged Warbler models 12-15
setwd("S:/MillerLab/Projects/BirdSDM")

library(dplyr)
library(tibble)
library(plyr)
library(mgcv)
library(jagsUI)
library(ROCR)

train1 <- read.csv("FinalData/TestTrain/train1.csv")
test2 <- read.csv("FinalData/TestTrain/test2.csv")
hdat <- read.csv("FinalData/TestTrain/hdat.csv")

dat12 <- rbind.fill(train1, test2)

#Model 12: gwwa (full model, validate on current; train1test2)
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509,
#test: eBird- 79510:84969, BBS- 84970:87201

m12 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
             data = dat12, family='binomial', file = 'placeholder.txt')

datm12 <- list(y = c(dat12$gwwadet[1:19829], dat12$gwwatot[19830:46832], dat12$gwwadet[46833:79509]), 
               X = m12$jags.data$X, n = m12$jags.data$n, zero = m12$jags.data$zero,
               S1 = m12$jags.data$S1, S2 = m12$jags.data$S2, S3 = m12$jags.data$S3,
               S4 = m12$jags.data$S4, S5 = m12$jags.data$S5, S6 = m12$jags.data$S6,
               S7 = m12$jags.data$S7, S8 = m12$jags.data$S8, S9 = m12$jags.data$S9, 
               ehours = dat12$duration_minutes[1:19829], 
               ekm = dat12$effort_distance_km[1:19829],
               ehours2 = dat12$duration_minutes[79510:84969], 
               ekm2 = dat12$effort_distance_km[79510:84969], 
               hsm = dat12$hsm[19830:46832],
               doy = dat12$doy[19830:46832],
               doy3 = dat12$doy[46833:79509],
               doy4 = dat12$doy[84970:87201])

initsm12 <- function(){
  list(b = m12$jags.ini$b, lambda = m12$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm12$y>0))
}

outm12 <- jags(data = datm12, parameters.to.save = c("beta","b","y2"), inits = initsm12, 
               model.file = "FinalModels/Q2Models/gwwam12.txt", 
               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500, parallel=TRUE)

#Full Deviance
m12_yp <- outm12$mean$y2
m12_yt <- c(dat12$gwwadet[79510:84969], dat12$gwwadet[84970:87201])
m12_yt <- cbind(m12_yt, c(1 - dat12$gwwadet[79510:84969], 1 - dat12$gwwadet[84970:87201]))
m12_yp <- 0.0001 + m12_yp*0.9998
m12_dev <- -2*sum(log((m12_yp^m12_yt[,1])*((1-m12_yp)^(m12_yt[,2]))))
#eBird deviance
Em12_yp <- outm12$mean$y2[1:5460]
Em12_yt <- c(dat12$gwwadet[79510:84969])
Em12_yt <- cbind(Em12_yt, c(1 - dat12$gwwadet[79510:84969]))
Em12_yp <- 0.0001 + Em12_yp*0.9998
Em12_dev <- -2*sum(log((Em12_yp^Em12_yt[,1])*((1-Em12_yp)^(Em12_yt[,2]))))
#BBS deviance
Sm12_yp <- outm12$mean$y2[5461:7692]
Sm12_yt <- c(dat12$gwwadet[84970:87201])
Sm12_yt <- cbind(Sm12_yt, c(1 - dat12$gwwadet[84970:87201]))
Sm12_yp <- 0.0001 + Sm12_yp*0.9998
Sm12_dev <- -2*sum(log((Sm12_yp^Sm12_yt[,1])*((1-Sm12_yp)^(Sm12_yt[,2]))))

save(outm12, m12, m12_dev, Em12_dev, Sm12_dev, 
     file = "FinalResults/gwwam12.RData")

brier12 <- mean((outm12$mean$y2 - dat12$gwwadet[79510:87201])^2)

pred12 <- prediction(as.numeric(outm12$mean$y2), dat12$gwwadet[79510:87201])
auc12 <- performance(pred12, measure = "auc")
auc12 <- auc12@y.values[[1]]



#Model 13: gwwa (no older data or BBA; hdat)
#train: eBird- 1:10748, BBS- 10749:19556
#test: eBird- 19557:25016, BBS- 25017:27248

m13 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
             data = hdat, family='binomial', file = 'placeholder.txt')

datm13 <- list(y = c(hdat$gwwadet[1:10748], hdat$gwwadet[10749:19556]), 
               X = m13$jags.data$X, n = m13$jags.data$n, zero = m13$jags.data$zero,
               S1 = m13$jags.data$S1, S2 = m13$jags.data$S2, S3 = m13$jags.data$S3,
               S4 = m13$jags.data$S4, S5 = m13$jags.data$S5, S6 = m13$jags.data$S6,
               S7 = m13$jags.data$S7, S8 = m13$jags.data$S8, S9 = m13$jags.data$S9, 
               ehours = hdat$duration_minutes[1:10748], 
               ekm = hdat$effort_distance_km[1:10748],
               ehours2 = hdat$duration_minutes[19557:25016], 
               ekm2 = hdat$effort_distance_km[19557:25016], 
               doy = hdat$doy[10749:19556],
               doy2 = hdat$doy[25017:27248])

initsm13 <- function(){
  list(b = m13$jags.ini$b, lambda = m13$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm13$y>0))
}

outm13 <- jags(data = datm13, parameters.to.save = c("beta","b","y2"), inits = initsm13, 
               model.file = "FinalModels/Q2Models/gwwam13.txt", 
               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500, parallel=TRUE)

#Full Deviance
m13_yp <- outm13$mean$y2
m13_yt <- c(hdat$gwwadet[19557:25016], hdat$gwwadet[25017:27248])
m13_yt <- cbind(m13_yt, c(1 - hdat$gwwadet[19557:25016], 1 - hdat$gwwadet[25017:27248]))
m13_yp <- 0.0001 + m13_yp*0.9998
m13_dev <- -2*sum(log((m13_yp^m13_yt[,1])*((1-m13_yp)^(m13_yt[,2]))))
#eBird deviance
Em13_yp <- outm13$mean$y2[1:5460]
Em13_yt <- c(hdat$gwwadet[19557:25016])
Em13_yt <- cbind(Em13_yt, c(1 - hdat$gwwadet[19557:25016]))
Em13_yp <- 0.0001 + Em13_yp*0.9998
Em13_dev <- -2*sum(log((Em13_yp^Em13_yt[,1])*((1-Em13_yp)^(Em13_yt[,2]))))
#BBS deviance
Sm13_yp <- outm13$mean$y2[5461:7692]
Sm13_yt <- c(hdat$gwwadet[25017:27248])
Sm13_yt <- cbind(Sm13_yt, c(1 - hdat$gwwadet[25017:27248]))
Sm13_yp <- 0.0001 + Sm13_yp*0.9998
Sm13_dev <- -2*sum(log((Sm13_yp^Sm13_yt[,1])*((1-Sm13_yp)^(Sm13_yt[,2]))))

save(outm13, m13, m13_dev, Em13_dev, Sm13_dev, 
     file = "FinalResults/gwwam13.RData")

brier13 <- mean((outm13$mean$y2 - hdat$gwwadet[19557:27248])^2)

pred13 <- prediction(as.numeric(outm13$mean$y2), hdat$gwwadet[19557:27248])
auc13 <- performance(pred13, measure = "auc")
auc13 <- auc13@y.values[[1]]



#Model 14: gwwa (old data and BBA as covariate; hdat)
#train: eBird- 1:10748, BBS- 10749:19556
#test: eBird- 19557:25016, BBS- 25017:27248

m14 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100) +
               lists_ebd + lists_bba + lists_bbs + gwwact_ebd + gwwact_bba + gwwact_bbs, 
             data = hdat, family='binomial', file = 'placeholder.txt')

datm14 <- list(y = c(hdat$gwwadet[1:10748], hdat$gwwadet[10749:19556]), 
               X = m14$jags.data$X, n = m14$jags.data$n, zero = m14$jags.data$zero,
               S1 = m14$jags.data$S1, S2 = m14$jags.data$S2, S3 = m14$jags.data$S3,
               S4 = m14$jags.data$S4, S5 = m14$jags.data$S5, S6 = m14$jags.data$S6,
               S7 = m14$jags.data$S7, S8 = m14$jags.data$S8, S9 = m14$jags.data$S9, 
               ehours = hdat$duration_minutes[1:10748], 
               ekm = hdat$effort_distance_km[1:10748],
               ehours2 = hdat$duration_minutes[19557:25016], 
               ekm2 = hdat$effort_distance_km[19557:25016], 
               doy = hdat$doy[10749:19556],
               doy2 = hdat$doy[25017:27248])

m14$jags.ini$b[2:4] <- -0.05
m14$jags.ini$b[5:7] <- 0.05

initsm14 <- function(){
  list(b = m14$jags.ini$b, lambda = m14$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm14$y>0))
}

outm14 <- jags(data = datm14, parameters.to.save = c("beta","b","y2"), inits = initsm14, 
               model.file = "FinalModels/Q2Models/gwwam14.txt", 
               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500, parallel=TRUE)

#Full Deviance
m14_yp <- outm14$mean$y2
m14_yt <- c(hdat$gwwadet[19557:25016], hdat$gwwadet[25017:27248])
m14_yt <- cbind(m14_yt, c(1 - hdat$gwwadet[19557:25016], 1 - hdat$gwwadet[25017:27248]))
m14_yp <- 0.0001 + m14_yp*0.9998
m14_dev <- -2*sum(log((m14_yp^m14_yt[,1])*((1-m14_yp)^(m14_yt[,2]))))
#eBird deviance
Em14_yp <- outm14$mean$y2[1:5460]
Em14_yt <- c(hdat$gwwadet[19557:25016])
Em14_yt <- cbind(Em14_yt, c(1 - hdat$gwwadet[19557:25016]))
Em14_yp <- 0.0001 + Em14_yp*0.9998
Em14_dev <- -2*sum(log((Em14_yp^Em14_yt[,1])*((1-Em14_yp)^(Em14_yt[,2]))))
#BBS deviance
Sm14_yp <- outm14$mean$y2[5461:7692]
Sm14_yt <- c(hdat$gwwadet[25017:27248])
Sm14_yt <- cbind(Sm14_yt, c(1 - hdat$gwwadet[25017:27248]))
Sm14_yp <- 0.0001 + Sm14_yp*0.9998
Sm14_dev <- -2*sum(log((Sm14_yp^Sm14_yt[,1])*((1-Sm14_yp)^(Sm14_yt[,2]))))

save(outm14, m14, m14_dev, Em14_dev, Sm14_dev, 
     file = "FinalResults/gwwam14.RData")

brier14 <- mean((outm14$mean$y2 - hdat$gwwadet[19557:27248])^2)

pred14 <- prediction(as.numeric(outm14$mean$y2), hdat$gwwadet[19557:27248])
auc14 <- performance(pred14, measure = "auc")
auc14 <- auc14@y.values[[1]]


#Model 15: gwwa (BBA false pos; dat12)
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
#test: eBird- 79510:84969, BBS- 84970:87201

m15 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
             data = dat12, family='binomial', file = 'placeholder.txt')

datm15 <- list(y = c(dat12$gwwadet[1:19829], dat12$gwwatot[19830:46832], dat12$gwwadet[46833:79509]), 
               X = m15$jags.data$X, n = m15$jags.data$n, zero = m15$jags.data$zero,
               S1 = m15$jags.data$S1, S2 = m15$jags.data$S2, S3 = m15$jags.data$S3,
               S4 = m15$jags.data$S4, S5 = m15$jags.data$S5, S6 = m15$jags.data$S6,
               S7 = m15$jags.data$S7, S8 = m15$jags.data$S8, S9 = m15$jags.data$S9, 
               ehours = dat12$duration_minutes[1:19829], 
               ekm = dat12$effort_distance_km[1:19829],
               ehours2 = dat12$duration_minutes[79510:84969], 
               ekm2 = dat12$effort_distance_km[79510:84969], 
               hsm = dat12$hsm[19830:46832],
               doy = dat12$doy[19830:46832],
               doy3 = dat12$doy[46833:79509],
               doy4 = dat12$doy[84970:87201])

initsm15 <- function(){
  list(b = m15$jags.ini$b, lambda = m15$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm15$y>0))
}

outm15 <- jags(data = datm15, parameters.to.save = c("beta","alpha1","alpha3","b","y2","f1"), inits = initsm15, 
               model.file = "FinalModels/Q2Models/gwwam15.txt", 
               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m15_yp <- outm15$mean$y2
m15_yt <- c(dat12$gwwadet[79510:84969], dat12$gwwadet[84970:87201])
m15_yt <- cbind(m15_yt, c(1 - dat12$gwwadet[79510:84969], 1 - dat12$gwwadet[84970:87201]))
m15_yp <- 0.0001 + m15_yp*0.9998
m15_dev <- -2*sum(log((m15_yp^m15_yt[,1])*((1-m15_yp)^(m15_yt[,2]))))
#eBird deviance
Em15_yp <- outm15$mean$y2[1:5460]
Em15_yt <- c(dat12$gwwadet[79510:84969])
Em15_yt <- cbind(Em15_yt, c(1 - dat12$gwwadet[79510:84969]))
Em15_yp <- 0.0001 + Em15_yp*0.9998
Em15_dev <- -2*sum(log((Em15_yp^Em15_yt[,1])*((1-Em15_yp)^(Em15_yt[,2]))))
#BBS deviance
Sm15_yp <- outm15$mean$y2[5461:7692]
Sm15_yt <- c(dat12$gwwadet[84970:87201])
Sm15_yt <- cbind(Sm15_yt, c(1 - dat12$gwwadet[84970:87201]))
Sm15_yp <- 0.0001 + Sm15_yp*0.9998
Sm15_dev <- -2*sum(log((Sm15_yp^Sm15_yt[,1])*((1-Sm15_yp)^(Sm15_yt[,2]))))

save(outm15, m15, m15_dev, Em15_dev, Sm15_dev, 
     file = "FinalResults/gwwam15.RData")

brier15 <- mean((outm15$mean$y2 - dat12$gwwadet[79510:87201])^2)

pred15 <- prediction(as.numeric(outm15$mean$y2), dat12$gwwadet[79510:87201])
auc15 <- performance(pred15, measure = "auc")
auc15 <- auc15@y.values[[1]]





########################

#Sim2Jam, model 14 (aka 13, covariate)
require(rjags)
jm14 <- jags.model("FinalModels/Q2Models/gwwam14.txt", data=datm14, inits=initsm14, n.adapt=500, n.chains=3)
update(jm14, 500)
sam14 <- jags.samples(jm14, c("b","rho"), n.iter=2000, thin=2)
jam14 <- sim2jam(sam14, m14$pregam)

save(m14, jm14, sam14, jam14,
     file = "FinalResults/Plots/gwwam14_jam.RData")

pdf("FinalResults/Plots/gwwa14cov.pdf")
plot(jam14, pages=1)
dev.off()

load("S:/MillerLab/Projects/BirdSDM/FinalResults/Plots/grid_pa_1km.RData")
library(ggplot2)
library(sf)
library(sp)
library(RColorBrewer)

#Generate predictions
grid3$pred <- predict(jam14, newdata = grid3, type="response")
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
pdf("FinalResults/Plots/gwwa14map.pdf")
spplot(grid_sp, "pred", col=NA, par.settings = list(axis.line = list(col = 'transparent')),
       xlim = edges[1, ], ylim = edges[2, ], colorkey=list(height=0.6, cex=0.5))
dev.off()



