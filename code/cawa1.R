
## This code was written by: fiona lunt

## Objective ---------------------------
## 
## Canada Warbler models 1-7
##
## Input:
##   test1.csv
##   train1.csv
##
## Output: 
##   cawam1.RData
##

## load packages ---------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(tibble)
library(plyr)
library(mgcv)
library(jagsUI)
library(ROCR)

## load functions ---------------------------
source('functions.R')

## load data ---------------------------
test1 <- read.csv("data/test1.csv")
train1 <- read.csv("data/train1.csv")

dat1 <- rbind.fill(train1, test1)


# Model 1: R1 - CAWA ------------------------------------------------------
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
train.eBird = 1:19829
train.BBA = 19830:46832
train.BBS = 46833:79509
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729
test.eBird = 79510:88726
test.BBA = 88727:95456
test.BBS = 95457:103729

m1 <- generate.code(dat1)


# Use the output found in placeholder.txt in the JAGS model file: cawam1.txt

datm1 <- list(y = c(dat1$cawadet[train.eBird],
                    dat1$cawatot[train.BBA],
                    dat1$cawadet[train.BBS]),
              X = m1$jags.data$X, n = m1$jags.data$n, zero = m1$jags.data$zero,
              S1 = m1$jags.data$S1, S2 = m1$jags.data$S2, S3 = m1$jags.data$S3,
              S4 = m1$jags.data$S4, S5 = m1$jags.data$S5, S6 = m1$jags.data$S6,
              S7 = m1$jags.data$S7, #S8 = m1$jags.data$S8, S9 = m1$jags.data$S9,
              ehours = dat1$duration_minutes[train.eBird],
              ekm = dat1$effort_distance_km[train.eBird],
              ehours2 = dat1$duration_minutes[test.eBird],
              ekm2 = dat1$effort_distance_km[test.eBird],
              hsm = dat1$hsm[train.BBA],
              hsm2 = dat1$hsm[test.BBA],
              doy = dat1$doy[train.BBA],
              doy2 = dat1$doy[test.BBA],
              doy3 = dat1$doy[train.BBS],
              doy4 = dat1$doy[test.BBS])

initsm1 <- function(){
  list(b = m1$jags.ini$b,
       lambda = m1$jags.ini$lambda,
       beta = rep(3, 0.001),
       z = as.numeric(datm1$y>0))
}

outm1 <- jags(data = datm1,
              parameters.to.save = c("beta","b","y2"),
              inits = initsm1,
              model.file = "models/cawam1.txt",
              n.chains = 3,
              n.thin = 2,
              n.adapt = 500,
              n.burnin = 500,
              n.iter = 2500,
              parallel = TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m1_yp <- outm1$mean$y2
m1_yt <- c(dat1$cawadet[test.eBird], dat1$cawatot[test.BBA], dat1$cawadet[test.BBS])
m1_yt <- cbind(m1_yt, c(1 - dat1$cawadet[test.eBird], 5 - dat1$cawatot[test.BBA], 1 - dat1$cawadet[test.BBS]))
m1_yp[9218:15947] <- m1_yp[9218:15947]/5
m1_yp <- 0.0001 + m1_yp*0.9998
m1_dev <- -2*sum(log((m1_yp^m1_yt[,1])*((1-m1_yp)^(m1_yt[,2]))))
#eBird deviance
Em1_yp <- outm1$mean$y2[1:9217]
Em1_yt <- c(dat1$cawadet[test.eBird])
Em1_yt <- cbind(Em1_yt, c(1 - dat1$cawadet[test.eBird]))
Em1_yp <- 0.0001 + Em1_yp*0.9998
Em1_dev <- -2*sum(log((Em1_yp^Em1_yt[,1])*((1-Em1_yp)^(Em1_yt[,2]))))
#BBA deviance
Am1_yp <- outm1$mean$y2[9218:15947]
Am1_yt <- c(dat1$cawatot[test.BBA])
Am1_yt <- cbind(Am1_yt, c(5 - dat1$cawatot[test.BBA]))
Am1_yp <- Am1_yp/5
Am1_yp <- 0.0001 + Am1_yp*0.9998
Am1_dev <- -2*sum(log((Am1_yp^Am1_yt[,1])*((1-Am1_yp)^(Am1_yt[,2]))))
#BBS deviance
Sm1_yp <- outm1$mean$y2[15948:24220]
Sm1_yt <- c(dat1$cawadet[test.BBS])
Sm1_yt <- cbind(Sm1_yt, c(1 - dat1$cawadet[test.BBS]))
Sm1_yp <- 0.0001 + Sm1_yp*0.9998
Sm1_dev <- -2*sum(log((Sm1_yp^Sm1_yt[,1])*((1-Sm1_yp)^(Sm1_yt[,2]))))


# Brier score
brier1 <- mean((outm1$mean$y2[9218:24220] - dat1$cawadet[88727:103729])^2)


# AUC
pred1 <- prediction(as.numeric(outm1$mean$y2[9218:24220]), dat1$cawadet[88727:103729])
auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]

# roc1 <- roc(dat1$cawadet[88642:103748], as.numeric(outm1$mean$y2[9218:24220]),
# ci.auc=TRUE, auc=TRUE, parallel=TRUE)


# Save CAWA M1 file
save(outm1, m1, datm1, m1_dev, Em1_dev, Am1_dev, Sm1_dev,
     brier1, pred1, auc1, initsm1,
     file = "results/out/cawam1.RData")


# End script

