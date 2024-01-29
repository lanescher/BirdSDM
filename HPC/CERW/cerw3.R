
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 3: F2 (no eBird spatial balancing)
## Cerulean warbler (CERW)
## 
## Input:
##   test1.csv
##   train3.csv
##
## Output: 
##   cerwm3.RData
##


## load packages ---------------------------
library(plyr)
library(dplyr)
library(tibble)
library(mgcv)
library(jagsUI)
library(ROCR)

path = '/caldera/hovenweep/projects/usgs/ecosystems/eesc/rmummah/proj05-fiona/'

## load functions ---------------------------
source(paste0(path,'functions.R'))

## load data ---------------------------
test1 <- read.csv(paste0(path,"data/test1.csv"))
train3 <- read.csv(paste0(path,"data/train3.csv"))

dat3 <- rbind.fill(train3, test1)


# Model 3: F2 - CAWA ------------------------------------------------------
#train: eBird- 1:111728, BBA- 111729:138731, BBS- 138732:171408
#test: eBird- 171409:180625, BBA- 180626:187355, BBS- 187356:195628

m3 <- generate.code(dat3)

datm3 <- list(y = c(dat3$cerwdet[1:111728], dat3$cerwtot[111729:138731], dat3$cerwdet[138732:171408]), 
              X = m3$jags.data$X, n = m3$jags.data$n, zero = m3$jags.data$zero,
              S1 = m3$jags.data$S1, S2 = m3$jags.data$S2, S3 = m3$jags.data$S3,
              S4 = m3$jags.data$S4, S5 = m3$jags.data$S5, S6 = m3$jags.data$S6,
              S7 = m3$jags.data$S7, 
              ehours = dat3$duration_minutes[1:111728], 
              ekm = dat3$effort_distance_km[1:111728], 
              ehours2 = dat3$duration_minutes[171409:180625], 
              ekm2 = dat3$effort_distance_km[171409:180625], 
              hsm = dat3$hsm[111729:138731],
              hsm2 = dat3$hsm[180626:187355], 
              doy = dat3$doy[111729:138731],
              doy2 = dat3$doy[180626:187355],
              doy3 = dat3$doy[138732:171408],
              doy4 = dat3$doy[187356:195628])

initsm3 <- function(){
  list(b = m3$jags.ini$b, 
       lambda = m3$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm3$y>0))
}

outm3 <- jags(data = datm3, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm3, 
              model.file = paste0(path,"models/cerwm3.txt"), 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500)


# Performance metrics -----------------------------------------------------

#Full Deviance
m3_yp <- outm3$mean$y2
m3_yt <- c(dat3$cerwdet[171409:180625], dat3$cerwtot[180626:187355], dat3$cerwdet[187356:195628])
m3_yt <- cbind(m3_yt, c(1 - dat3$cerwdet[171409:180625], 5 - dat3$cerwtot[180626:187355], 1 - dat3$cerwdet[187356:195628]))
m3_yp[9218:15947] <- m3_yp[9218:15947]/5
m3_yp <- 0.0001 + m3_yp*0.9998
m3_dev <- -2*sum(log((m3_yp^m3_yt[,1])*((1-m3_yp)^(m3_yt[,2]))))
#eBird deviance
Em3_yp <- outm3$mean$y2[1:9217]
Em3_yt <- c(dat3$cerwdet[171409:180625])
Em3_yt <- cbind(Em3_yt, c(1 - dat3$cerwdet[171409:180625]))
Em3_yp <- 0.0001 + Em3_yp*0.9998
Em3_dev <- -2*sum(log((Em3_yp^Em3_yt[,1])*((1-Em3_yp)^(Em3_yt[,2]))))
#BBA deviance
Am3_yp <- outm3$mean$y2[9218:15947]
Am3_yt <- c(dat3$cerwtot[180626:187355])
Am3_yt <- cbind(Am3_yt, c(5 - dat3$cerwtot[180626:187355])) 
Am3_yp <- Am3_yp/5
Am3_yp <- 0.0001 + Am3_yp*0.9998
Am3_dev <- -2*sum(log((Am3_yp^Am3_yt[,1])*((1-Am3_yp)^(Am3_yt[,2]))))
#BBS deviance
Sm3_yp <- outm3$mean$y2[15948:24220]
Sm3_yt <- c(dat3$cerwdet[187356:195628])
Sm3_yt <- cbind(Sm3_yt, c(1 - dat3$cerwdet[187356:195628]))
Sm3_yp <- 0.0001 + Sm3_yp*0.9998
Sm3_dev <- -2*sum(log((Sm3_yp^Sm3_yt[,1])*((1-Sm3_yp)^(Sm3_yt[,2]))))


# Brier score
brier3 <- mean((outm3$mean$y2[9218:24220] - dat3$cerwdet[180626:195628])^2)


# AUC
pred3 <- prediction(as.numeric(outm3$mean$y2[9218:24220]), dat3$cerwdet[180626:195628])
auc3 <- performance(pred3, measure = "auc")
auc3 <- auc3@y.values[[1]]


save(outm3, m3, datm3, m3_dev, Em3_dev, Am3_dev, Sm3_dev, 
     brier3, pred3, auc3, initsm3,
     file = paste0(path,"results/out/cerwm3.RData"))


# End script

