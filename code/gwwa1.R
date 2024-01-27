
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 1: R1 (reference) 
## Golden-winged warbler (GWWA)
##
## Input:
##   test1.csv
##   train1.csv
##
## Output: 
##   gwwam1.RData
##

## load packages ---------------------------
library(plyr)
library(dplyr)
library(tibble)
library(mgcv)
library(jagsUI)
library(ROCR)

## load functions ---------------------------
source('functions.R')

## load data ---------------------------
test1 <- read.csv("data/test1.csv")
train1 <- read.csv("data/train1.csv")

dat1 <- rbind.fill(train1, test1)

# Model 1: R1 - GWWA ------------------------------------------------------
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509,
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729

m1 <- generate.code(dat1)

datm1 <- list(y = c(dat1$gwwadet[1:19829], dat1$gwwatot[19830:46832], dat1$gwwadet[46833:79509]), 
              X = m1$jags.data$X, n = m1$jags.data$n, zero = m1$jags.data$zero,
              S1 = m1$jags.data$S1, S2 = m1$jags.data$S2, S3 = m1$jags.data$S3,
              S4 = m1$jags.data$S4, S5 = m1$jags.data$S5, S6 = m1$jags.data$S6,
              S7 = m1$jags.data$S7, S8 = m1$jags.data$S8, S9 = m1$jags.data$S9, 
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

initsm1 <- function(){
  list(b = m1$jags.ini$b, 
       lambda = m1$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm1$y>0))
}

outm1 <- jags(data = datm1, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm1, 
              model.file = "models/gwwam1.txt", 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500, 
              parallel=TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m1_yp <- outm1$mean$y2
m1_yt <- c(dat1$gwwadet[79510:88726], dat1$gwwatot[88727:95456], dat1$gwwadet[95457:103729])
m1_yt <- cbind(m1_yt, c(1 - dat1$gwwadet[79510:88726], 5 - dat1$gwwatot[88727:95456], 1 - dat1$gwwadet[95457:103729]))
m1_yp[9218:15947] <- m1_yp[9218:15947]/5
m1_yp <- 0.0001 + m1_yp*0.9998
m1_dev <- -2*sum(log((m1_yp^m1_yt[,1])*((1-m1_yp)^(m1_yt[,2]))))
#eBird deviance
Em1_yp <- outm1$mean$y2[1:9217]
Em1_yt <- c(dat1$gwwadet[79510:88726])
Em1_yt <- cbind(Em1_yt, c(1 - dat1$gwwadet[79510:88726]))
Em1_yp <- 0.0001 + Em1_yp*0.9998
Em1_dev <- -2*sum(log((Em1_yp^Em1_yt[,1])*((1-Em1_yp)^(Em1_yt[,2]))))
#BBA deviance
Am1_yp <- outm1$mean$y2[9218:15947]
Am1_yt <- c(dat1$gwwatot[88727:95456])
Am1_yt <- cbind(Am1_yt, c(5 - dat1$gwwatot[88727:95456]))
Am1_yp <- Am1_yp/5
Am1_yp <- 0.0001 + Am1_yp*0.9998
Am1_dev <- -2*sum(log((Am1_yp^Am1_yt[,1])*((1-Am1_yp)^(Am1_yt[,2]))))
#BBS deviance
Sm1_yp <- outm1$mean$y2[15948:24220]
Sm1_yt <- c(dat1$gwwadet[95457:103729])
Sm1_yt <- cbind(Sm1_yt, c(1 - dat1$gwwadet[95457:103729]))
Sm1_yp <- 0.0001 + Sm1_yp*0.9998
Sm1_dev <- -2*sum(log((Sm1_yp^Sm1_yt[,1])*((1-Sm1_yp)^(Sm1_yt[,2]))))


# Brier score
brier1 <- mean((outm1$mean$y2[9218:24220] - dat1$gwwadet[88727:103729])^2)

# AUC
pred1 <- prediction(as.numeric(outm1$mean$y2[9218:24220]), dat1$gwwadet[88727:103729])
auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]


save(outm1, m1, datm1, m1_dev, Em1_dev, Am1_dev, Sm1_dev,
     brier1, pred1, auc1,
     file = "results/out/gwwam1.RData")

# End script

