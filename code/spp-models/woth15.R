
## This code was written by: fiona lunt

## Objective ---------------------------
##  Model 15: O1 (false positive for BBA, validate on new data)
##  Wood thrush (WOTH)
## 
## Input:
##    train1.csv
##    test1.csv
##
## Output: 
##    wothm15.RData
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
train1 <- read.csv("data/train1.csv")
test2 <- read.csv("data/test2.csv")

dat12 <- rbind.fill(train1, test2)


# Model 15: O1 - CAWA -----------------------------------------------------
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
#test: eBird- 79510:84969, BBS- 84970:87201

m15 <- generate.code(dat12)

datm15 <- list(y = c(dat12$wothdet[1:19829], dat12$wothtot[19830:46832], dat12$wothdet[46833:79509]), 
               X = m15$jags.data$X, n = m15$jags.data$n, zero = m15$jags.data$zero,
               S1 = m15$jags.data$S1, S2 = m15$jags.data$S2, S3 = m15$jags.data$S3,
               S4 = m15$jags.data$S4, S5 = m15$jags.data$S5, S6 = m15$jags.data$S6,
               S7 = m15$jags.data$S7, 
               ehours = dat12$duration_minutes[1:19829], 
               ekm = dat12$effort_distance_km[1:19829],
               ehours2 = dat12$duration_minutes[79510:84969], 
               ekm2 = dat12$effort_distance_km[79510:84969], 
               hsm = dat12$hsm[19830:46832],
               doy = dat12$doy[19830:46832],
               doy3 = dat12$doy[46833:79509],
               doy4 = dat12$doy[84970:87201])

initsm15 <- function(){
  list(b = m15$jags.ini$b, 
       lambda = m15$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm15$y>0))
}

outm15 <- jags(data = datm15, 
               parameters.to.save = c("beta","alpha1","alpha3","b","y2","f1"), 
               inits = initsm15, 
               model.file = "models/wothm15.txt", 
               n.chains = 3, 
               n.thin = 2, 
               n.adapt = 500, 
               n.burnin = 500, 
               n.iter = 2500,
               parallel = TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m15_yp <- outm15$mean$y2
m15_yt <- c(dat12$wothdet[79510:84969], dat12$wothdet[84970:87201])
m15_yt <- cbind(m15_yt, c(1 - dat12$wothdet[79510:84969], 1 - dat12$wothdet[84970:87201]))
m15_yp <- 0.0001 + m15_yp*0.9998
m15_dev <- -2*sum(log((m15_yp^m15_yt[,1])*((1-m15_yp)^(m15_yt[,2]))))
#eBird deviance
Em15_yp <- outm15$mean$y2[1:5460]
Em15_yt <- c(dat12$wothdet[79510:84969])
Em15_yt <- cbind(Em15_yt, c(1 - dat12$wothdet[79510:84969]))
Em15_yp <- 0.0001 + Em15_yp*0.9998
Em15_dev <- -2*sum(log((Em15_yp^Em15_yt[,1])*((1-Em15_yp)^(Em15_yt[,2]))))
#BBS deviance
Sm15_yp <- outm15$mean$y2[5461:7692]
Sm15_yt <- c(dat12$wothdet[84970:87201])
Sm15_yt <- cbind(Sm15_yt, c(1 - dat12$wothdet[84970:87201]))
Sm15_yp <- 0.0001 + Sm15_yp*0.9998
Sm15_dev <- -2*sum(log((Sm15_yp^Sm15_yt[,1])*((1-Sm15_yp)^(Sm15_yt[,2]))))


# Brier score
brier15 <- mean((outm15$mean$y2 - dat12$wothdet[79510:87201])^2)

# AUC
pred15 <- prediction(as.numeric(outm15$mean$y2), dat12$wothdet[79510:87201])
auc15 <- performance(pred15, measure = "auc")
auc15 <- auc15@y.values[[1]]

save(outm15, m15, datm15, m15_dev, Em15_dev, Sm15_dev, 
     brier15, pred15, auc15, initsm15,
     file = "results/out/wothm15.RData")

# End script
