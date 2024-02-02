
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 11: F4 (stationary shared, no travelling)
## Wood thrush (WOTH) 
##
## Input:
##    covdat2.csv
##
## Output: 
##    wothm11.RData
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
covdat2 <- read.csv("data/covdat2.csv")

covdat2$duration_minutes[12729] <- mean(covdat2$duration_minutes[1:14725])

# Model 11: F4 - CAWA -----------------------------------------------------
#train: eBird stat- 1:14725, BBA- 14726:41728, BBS- 41729:74405
#test: eBird- 74406:83622, BBA- 83623:90352, BBS- 90353:98625

m11 <- generate.code(covdat2)

datm11 <- list(y = c(covdat2$wothdet[1:14725], covdat2$wothtot[14726:41728], covdat2$wothdet[41729:74405]), 
               X = m11$jags.data$X, n = m11$jags.data$n, zero = m11$jags.data$zero,
               S1 = m11$jags.data$S1, S2 = m11$jags.data$S2, S3 = m11$jags.data$S3,
               S4 = m11$jags.data$S4, S5 = m11$jags.data$S5, S6 = m11$jags.data$S6,
               S7 = m11$jags.data$S7, 
               ehours = covdat2$duration_minutes[1:14725], 
               ehours2 = covdat2$duration_minutes[74406:83622], 
               hsm = covdat2$hsm[14726:41728],
               hsm2 = covdat2$hsm[83623:90352], 
               doy = covdat2$doy[14726:41728],
               doy2 = covdat2$doy[83623:90352],
               doy3 = covdat2$doy[41729:74405],
               doy4 = covdat2$doy[90353:98625])

initsm11 <- function(){
  list(b = m11$jags.ini$b, 
       lambda = m11$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm11$y>0))
}

outm11 <- jags(data = datm11, 
               parameters.to.save = c("beta","b","y2"), 
               inits = initsm11, 
               model.file = "models/wothm11.txt", 
               n.chains = 3, 
               n.thin = 2, 
               n.adapt = 500, 
               n.burnin = 500, 
               n.iter = 2500,
               parallel = TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m11_yp <- outm11$mean$y2
m11_yt <- c(covdat2$wothdet[74406:83622], covdat2$wothtot[83623:90352], covdat2$wothdet[90353:98625])
m11_yt <- cbind(m11_yt, c(1 - covdat2$wothdet[74406:83622], 5 - covdat2$wothtot[83623:90352], 1 - covdat2$wothdet[90353:98625]))
m11_yp[9218:15947] <- m11_yp[9218:15947]/5
m11_yp <- 0.0001 + m11_yp*0.9998
m11_dev <- -2*sum(log((m11_yp^m11_yt[,1])*((1-m11_yp)^(m11_yt[,2]))))
#eBird deviance
Em11_yp <- outm11$mean$y2[1:9217]
Em11_yt <- c(covdat2$wothdet[74406:83622])
Em11_yt <- cbind(Em11_yt, c(1 - covdat2$wothdet[74406:83622]))
Em11_yp <- 0.0001 + Em11_yp*0.9998
Em11_dev <- -2*sum(log((Em11_yp^Em11_yt[,1])*((1-Em11_yp)^(Em11_yt[,2]))))
#BBA deviance
Am11_yp <- outm11$mean$y2[9218:15947]
Am11_yt <- c(covdat2$wothtot[83623:90352])
Am11_yt <- cbind(Am11_yt, c(5 - covdat2$wothtot[83623:90352])) 
Am11_yp <- Am11_yp/5
Am11_yp <- 0.0001 + Am11_yp*0.9998
Am11_dev <- -2*sum(log((Am11_yp^Am11_yt[,1])*((1-Am11_yp)^(Am11_yt[,2]))))
#BBS deviance
Sm11_yp <- outm11$mean$y2[15948:24220]
Sm11_yt <- c(covdat2$wothdet[90353:98625])
Sm11_yt <- cbind(Sm11_yt, c(1 - covdat2$wothdet[90353:98625]))
Sm11_yp <- 0.0001 + Sm11_yp*0.9998
Sm11_dev <- -2*sum(log((Sm11_yp^Sm11_yt[,1])*((1-Sm11_yp)^(Sm11_yt[,2]))))


# Brier score
brier11 <- mean((outm11$mean$y2[9218:24220] - covdat2$wothdet[83623:98625])^2)

# AUC
pred11 <- prediction(as.numeric(outm11$mean$y2[9218:24220]), covdat2$wothdet[83623:98625])
auc11 <- performance(pred11, measure = "auc")
auc11 <- auc11@y.values[[1]]

save(outm11, m11, datm11, m11_dev, Em11_dev, Am11_dev, Sm11_dev, 
     brier11, pred11, auc11, initsm11,
     file = "results/out/wothm11.RData")

# End script
