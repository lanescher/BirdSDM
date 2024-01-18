#Newer Golden-winged Warbler models
setwd("S:/MillerLab/Projects/BirdSDM")

library(dplyr)
library(tibble)
library(plyr)
library(mgcv)
library(jagsUI)
library(ROCR)

#Load data
test1 <- read.csv("FinalData/TestTrain/test1.csv")
train1 <- read.csv("FinalData/TestTrain/train1.csv")

#Bind data
dat1 <- rbind.fill(train1, test1)

#Create intercept column (n=non-standardized, s=standardized)
dat1$int <- "x"
dat1$int[1:19829] <- "N"
dat1$int[19830:79509] <- "S"
dat1$int[79510:88726] <- "N"
dat1$int[88727:103729] <- "S"
dat1$int <- as.factor(dat1$int)

#Model 16: same as full model #1 but with different intercept
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509,
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729

m16 <- jagam(gwwadet ~ int + s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
             data = dat1, family='binomial', file = 'placeholder.txt')

datm16 <- list(y = c(dat1$gwwadet[1:19829], dat1$gwwatot[19830:46832], dat1$gwwadet[46833:79509]), 
               X = m16$jags.data$X, n = m16$jags.data$n, zero = m16$jags.data$zero,
               S1 = m16$jags.data$S1, S2 = m16$jags.data$S2, S3 = m16$jags.data$S3,
               S4 = m16$jags.data$S4, S5 = m16$jags.data$S5, S6 = m16$jags.data$S6,
               S7 = m16$jags.data$S7, S8 = m16$jags.data$S8, S9 = m16$jags.data$S9, 
               ehours = dat1$duration_minutes[1:19829], 
               ekm = dat1$effort_distance_km[1:19829],
               ehours2 = dat1$duration_minutes[79510:88726], 
               ekm2 = dat1$effort_distance_km[79510:88726], 
               hsm = dat1$hsm[19830:46832],
               hsm2 = dat1$hsm[88727:95456], 
               doy = dat1$doy[19830:46832],
               doy2 = dat1$doy[88727:95456],
               doy3 = dat1$doy[46833:79509],
               doy4 = dat1$doy[95457:103729])

initsm16 <- function(){
  list(b = m16$jags.ini$b, lambda = m16$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm16$y>0))
}

outm16 <- jags(data = datm16, parameters.to.save = c("beta","b","y2"), inits = initsm16, 
               model.file = "FinalModels/gwwam16.txt", 
               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m16_yp <- outm16$mean$y2
m16_yt <- c(dat1$gwwadet[79510:88726], dat1$gwwatot[88727:95456], dat1$gwwadet[95457:103729])
m16_yt <- cbind(m16_yt, c(1 - dat1$gwwadet[79510:88726], 5 - dat1$gwwatot[88727:95456], 1 - dat1$gwwadet[95457:103729]))
m16_yp[9218:15947] <- m16_yp[9218:15947]/5
m16_yp <- 0.0001 + m16_yp*0.9998
m16_dev <- -2*sum(log((m16_yp^m16_yt[,1])*((1-m16_yp)^(m16_yt[,2]))))
#eBird deviance
Em16_yp <- outm16$mean$y2[1:9217]
Em16_yt <- c(dat1$gwwadet[79510:88726])
Em16_yt <- cbind(Em16_yt, c(1 - dat1$gwwadet[79510:88726]))
Em16_yp <- 0.0001 + Em16_yp*0.9998
Em16_dev <- -2*sum(log((Em16_yp^Em16_yt[,1])*((1-Em16_yp)^(Em16_yt[,2]))))
#BBA deviance
Am16_yp <- outm16$mean$y2[9218:15947]
Am16_yt <- c(dat1$gwwatot[88727:95456])
Am16_yt <- cbind(Am16_yt, c(5 - dat1$gwwatot[88727:95456]))
Am16_yp <- Am16_yp/5
Am16_yp <- 0.0001 + Am16_yp*0.9998
Am16_dev <- -2*sum(log((Am16_yp^Am16_yt[,1])*((1-Am16_yp)^(Am16_yt[,2]))))
#BBS deviance
Sm16_yp <- outm16$mean$y2[15948:24220]
Sm16_yt <- c(dat1$gwwadet[95457:103729])
Sm16_yt <- cbind(Sm16_yt, c(1 - dat1$gwwadet[95457:103729]))
Sm16_yp <- 0.0001 + Sm16_yp*0.9998
Sm16_dev <- -2*sum(log((Sm16_yp^Sm16_yt[,1])*((1-Sm16_yp)^(Sm16_yt[,2]))))

save(outm16, m16, m16_dev, Em16_dev, Am16_dev, Sm16_dev, 
     file = "FinalResults/gwwam16.RData")

brier16 <- mean((outm16$mean$y2[9218:24220] - dat1$gwwadet[88727:103729])^2)

pred16 <- prediction(as.numeric(outm16$mean$y2[9218:24220]), dat1$gwwadet[88727:103729])
auc16 <- performance(pred16, measure = "auc")
auc16 <- auc16@y.values[[1]]









