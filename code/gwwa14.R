
## This code was written by: fiona lunt

## Objective ---------------------------
##  Model 14: O2 (treat BBA and old data as a covariate)
##  Golden-winged warbler (GWWA)
## 
## Input:
##    train1.csv
##    test1.csv
##    hdat.csv
##
## Output: 
##    gwwam14.RData
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


# Model 14: O2 - GWWA -----------------------------------------------------
#train: eBird- 1:10748, BBS- 10749:19556
#test: eBird- 19557:25016, BBS- 25017:27248

m14 <- generate.code(hdat)

datm14 <- list(y = c(hdat$gwwadet[1:10748], hdat$gwwadet[10749:19556]), 
               X = m14$jags.data$X, n = m14$jags.data$n, zero = m14$jags.data$zero,
               S1 = m14$jags.data$S1, S2 = m14$jags.data$S2, S3 = m14$jags.data$S3,
               S4 = m14$jags.data$S4, S5 = m14$jags.data$S5, S6 = m14$jags.data$S6,
               S7 = m14$jags.data$S7,  
               ehours = hdat$duration_minutes[1:10748], 
               ekm = hdat$effort_distance_km[1:10748],
               ehours2 = hdat$duration_minutes[19557:25016], 
               ekm2 = hdat$effort_distance_km[19557:25016], 
               doy = hdat$doy[10749:19556],
               doy2 = hdat$doy[25017:27248])

m14$jags.ini$b[2:4] <- -0.05
m14$jags.ini$b[5:7] <- 0.05

initsm14 <- function(){
  list(b = m14$jags.ini$b, 
       lambda = m14$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm14$y>0))
}

outm14 <- jags(data = datm14, 
               parameters.to.save = c("beta","b","y2"), 
               inits = initsm14, 
               model.file = "models/gwwam14.txt", 
               n.chains = 3, 
               n.thin = 2, 
               n.adapt = 500, 
               n.burnin = 500, 
               n.iter = 2500, 
               parallel=TRUE)


### Performance metrics -----------------------------------------------------

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


# Brier score
brier14 <- mean((outm14$mean$y2 - hdat$gwwadet[19557:27248])^2)

# AUC
pred14 <- prediction(as.numeric(outm14$mean$y2), hdat$gwwadet[19557:27248])
auc14 <- performance(pred14, measure = "auc")
auc14 <- auc14@y.values[[1]]


save(outm14, m14, datm14, m14_dev, Em14_dev, Sm14_dev, 
     brier14, pred14, auc14, initsm14,
     file = "results/out/gwwam14.RData")


# End script
