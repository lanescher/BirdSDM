
## This code was written by: fiona lunt

## Objective ---------------------------
##  Model 13: O3 (no older data or BBA; hdat)
##  Canada warbler (CAWA)
## 
## Input:
##    train1.csv
##    test1.csv
##    hdat.csv
##
## Output: 
##    cawam13.RData
##
## ---------------------------

## load packages ---------------------------
library(plyr)
library(dplyr)
library(tibble)
library(mgcv)
library(jagsUI)
library(ROCR)


## load functions ---------------------------
source('functions.R')


## load data --------------------------------
hdat <- read.csv("data/hdat.csv")


# Model 13: O3 - CAWA -----------------------------------------------------
#train: eBird- 1:10748, BBS- 10749:19556
#test: eBird- 19557:25016, BBS- 25017:27248

m13 <- generate.code(hdat)

datm13 <- list(y = c(hdat$cawadet[1:10748], hdat$cawadet[10749:19556]), 
               X = m13$jags.data$X, n = m13$jags.data$n, zero = m13$jags.data$zero,
               S1 = m13$jags.data$S1, S2 = m13$jags.data$S2, S3 = m13$jags.data$S3,
               S4 = m13$jags.data$S4, S5 = m13$jags.data$S5, S6 = m13$jags.data$S6,
               S7 = m13$jags.data$S7, 
               ehours = hdat$duration_minutes[1:10748], 
               ekm = hdat$effort_distance_km[1:10748],
               ehours2 = hdat$duration_minutes[19557:25016], 
               ekm2 = hdat$effort_distance_km[19557:25016], 
               doy = hdat$doy[10749:19556],
               doy2 = hdat$doy[25017:27248])

initsm13 <- function(){
  list(b = m13$jags.ini$b, 
       lambda = m13$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm13$y>0))
}

outm13 <- jags(data = datm13, 
               parameters.to.save = c("beta","b","y2"), 
               inits = initsm13, 
               model.file = "models/cawam13.txt", 
               n.chains = 3, 
               n.thin = 2, 
               n.adapt = 500, 
               n.burnin = 500, 
               n.iter = 2500, 
               parallel=TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m13_yp <- outm13$mean$y2
m13_yt <- c(hdat$cawadet[19557:25016], hdat$cawadet[25017:27248])
m13_yt <- cbind(m13_yt, c(1 - hdat$cawadet[19557:25016], 1 - hdat$cawadet[25017:27248]))
m13_yp <- 0.0001 + m13_yp*0.9998
m13_dev <- -2*sum(log((m13_yp^m13_yt[,1])*((1-m13_yp)^(m13_yt[,2]))))
#eBird deviance
Em13_yp <- outm13$mean$y2[1:5460]
Em13_yt <- c(hdat$cawadet[19557:25016])
Em13_yt <- cbind(Em13_yt, c(1 - hdat$cawadet[19557:25016]))
Em13_yp <- 0.0001 + Em13_yp*0.9998
Em13_dev <- -2*sum(log((Em13_yp^Em13_yt[,1])*((1-Em13_yp)^(Em13_yt[,2]))))
#BBS deviance
Sm13_yp <- outm13$mean$y2[5461:7692]
Sm13_yt <- c(hdat$cawadet[25017:27248])
Sm13_yt <- cbind(Sm13_yt, c(1 - hdat$cawadet[25017:27248]))
Sm13_yp <- 0.0001 + Sm13_yp*0.9998
Sm13_dev <- -2*sum(log((Sm13_yp^Sm13_yt[,1])*((1-Sm13_yp)^(Sm13_yt[,2]))))


# Brier score
brier13 <- mean((outm13$mean$y2 - hdat$cawadet[19557:27248])^2)


# AUC
pred13 <- prediction(as.numeric(outm13$mean$y2), hdat$cawadet[19557:27248])
auc13 <- performance(pred13, measure = "auc")
auc13 <- auc13@y.values[[1]]

save(outm13, m13, m13_dev, Em13_dev, Sm13_dev, 
     brier13, pred13, auc13, initsm13,
     file = "results/out/cawam13.RData")


# End script


