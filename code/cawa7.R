
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 7: I1 (full model + false pos for eBird)
## Canada warbler (CAWA) 
##
## Input:
##   test1.csv
##   train1.csv
##
## Output: 
##
##

## load packages ---------------------------
library(plyr)
library(magrittr)
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


# Model 7: I1 - CAWA ------------------------------------------------------
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729

# Run and incorporate code into JAGS model
m7 <- generate.code(dat1)


datm7 <- list(y = c(dat1$cawadet[1:19829], dat1$cawatot[19830:46832], dat1$cawadet[46833:79509]),
              X = m7$jags.data$X, n = m7$jags.data$n, zero = m7$jags.data$zero,
              S1 = m7$jags.data$S1, S2 = m7$jags.data$S2, S3 = m7$jags.data$S3,
              S4 = m7$jags.data$S4, S5 = m7$jags.data$S5, S6 = m7$jags.data$S6,
              S7 = m7$jags.data$S7,
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

initsm7 <- function(){
  list(b = m7$jags.ini$b, 
       lambda = m7$jags.ini$lambda, 
       beta = rep(3, 0.001), 
       z = as.numeric(datm7$y>0), f=0.05)
}

outm7 <- jags(data = datm7, 
              parameters.to.save = c("beta","b","y2", "f"), 
              inits = initsm7,
              model.file = "models/cawam7.txt",
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)


### Performance metrics -----------------------------------------------------

#Full Deviance
m7_yp <- outm7$mean$y2
m7_yt <- c(dat1$cawadet[79510:88726], dat1$cawatot[88727:95456], 
           dat1$cawadet[95457:103729])
m7_yt <- cbind(m7_yt, c(1 - dat1$cawadet[79510:88726], 
                        5 - dat1$cawatot[88727:95456], 
                        1 - dat1$cawadet[95457:103729]))
m7_yp[9218:15947] <- m7_yp[9287:16204]/5
m7_yp <- 0.0001 + m7_yp*0.9998
m7_dev <- -2*sum(log((m7_yp^m7_yt[,1])*((1-m7_yp)^(m7_yt[,2]))))
#eBird deviance
Em7_yp <- outm7$mean$y2[1:9217]
Em7_yt <- c(dat1$cawadet[79510:88726])
Em7_yt <- cbind(Em7_yt, c(1 - dat1$cawadet[79510:88726]))
Em7_yp <- 0.0001 + Em7_yp*0.9998
Em7_dev <- -2*sum(log((Em7_yp^Em7_yt[,1])*((1-Em7_yp)^(Em7_yt[,2]))))
#BBA deviance
Am7_yp <- outm7$mean$y2[9218:15947]
Am7_yt <- c(dat1$cawatot[88727:95456])
Am7_yt <- cbind(Am7_yt, c(5 - dat1$cawatot[88727:95456]))
Am7_yp <- Am7_yp/5
Am7_yp <- 0.0001 + Am7_yp*0.9998
Am7_dev <- -2*sum(log((Am7_yp^Am7_yt[,1])*((1-Am7_yp)^(Am7_yt[,2]))))
#BBS deviance
Sm7_yp <- outm7$mean$y2[15948:24220]
Sm7_yt <- c(dat1$cawadet[95457:103729])
Sm7_yt <- cbind(Sm7_yt, c(1 - dat1$cawadet[95457:103729]))
Sm7_yp <- 0.0001 + Sm7_yp*0.9998
Sm7_dev <- -2*sum(log((Sm7_yp^Sm7_yt[,1])*((1-Sm7_yp)^(Sm7_yt[,2]))))


# Brier score
brier7 <- mean((outm7$mean$y2[9218:24220] - dat1$cawadet[88727:103729])^2)


# AUC
pred7 <- prediction(as.numeric(outm7$mean$y2[9218:24220]), dat1$cawadet[88727:103729])
auc7 <- performance(pred7, measure = "auc")
auc7 <- auc7@y.values[[1]]


save(outm7, m7, datm7, m7_dev, Em7_dev, Am7_dev, Sm7_dev,
     brier7, pred7, auc7, initsm7,
     file = "results/out/cawam7.RData")


# End script
