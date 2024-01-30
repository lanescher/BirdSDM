
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 5: F5 (no eBird effort)
## Wood thrush (WOTH)
## 
## Input:
##   test1.csv
##   train1.csv
##
## Output: 
##   wothm5.RData
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



# Model 5: F5 - CAWA ------------------------------------------------------
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729

m5 <- generate.code(dat1)

datm5 <- list(y = c(dat1$wothdet[1:19829], dat1$wothtot[19830:46832], dat1$wothdet[46833:79509]), 
              X = m5$jags.data$X, n = m5$jags.data$n, zero = m5$jags.data$zero,
              S1 = m5$jags.data$S1, S2 = m5$jags.data$S2, S3 = m5$jags.data$S3,
              S4 = m5$jags.data$S4, S5 = m5$jags.data$S5, S6 = m5$jags.data$S6,
              S7 = m5$jags.data$S7, 
              hsm = dat1$hsm[19830:46832],
              hsm2 = dat1$hsm[88727:95456], 
              doy = dat1$doy[19830:46832],
              doy2 = dat1$doy[88727:95456],
              doy3 = dat1$doy[46833:79509],
              doy4 = dat1$doy[95457:103729])

initsm5 <- function(){
  list(b = m5$jags.ini$b, 
       lambda = m5$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm5$y>0))
}

outm5 <- jags(data = datm5, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm5, 
              model.file = "models/wothm5.txt", 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500,
              parallel = TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m5_yp <- outm5$mean$y2
m5_yt <- c(dat1$wothdet[79510:88726], dat1$wothtot[88727:95456], dat1$wothdet[95457:103729])
m5_yt <- cbind(m5_yt, c(1 - dat1$wothdet[79510:88726], 5 - dat1$wothtot[88727:95456], 1 - dat1$wothdet[95457:103729]))
m5_yp[9218:15947] <- m5_yp[9287:16204]/5
m5_yp <- 0.0001 + m5_yp*0.9998
m5_dev <- -2*sum(log((m5_yp^m5_yt[,1])*((1-m5_yp)^(m5_yt[,2]))))
#eBird deviance
Em5_yp <- outm5$mean$y2[1:9217]
Em5_yt <- c(dat1$wothdet[79510:88726])
Em5_yt <- cbind(Em5_yt, c(1 - dat1$wothdet[79510:88726]))
Em5_yp <- 0.0001 + Em5_yp*0.9998
Em5_dev <- -2*sum(log((Em5_yp^Em5_yt[,1])*((1-Em5_yp)^(Em5_yt[,2]))))
#BBA deviance
Am5_yp <- outm5$mean$y2[9218:15947]
Am5_yt <- c(dat1$wothtot[88727:95456])
Am5_yt <- cbind(Am5_yt, c(5 - dat1$wothtot[88727:95456]))
Am5_yp <- Am5_yp/5
Am5_yp <- 0.0001 + Am5_yp*0.9998
Am5_dev <- -2*sum(log((Am5_yp^Am5_yt[,1])*((1-Am5_yp)^(Am5_yt[,2]))))
#BBS deviance
Sm5_yp <- outm5$mean$y2[15948:24220]
Sm5_yt <- c(dat1$wothdet[95457:103729])
Sm5_yt <- cbind(Sm5_yt, c(1 - dat1$wothdet[95457:103729]))
Sm5_yp <- 0.0001 + Sm5_yp*0.9998
Sm5_dev <- -2*sum(log((Sm5_yp^Sm5_yt[,1])*((1-Sm5_yp)^(Sm5_yt[,2]))))


# Brier score
brier5 <- mean((outm5$mean$y2[9218:24220] - dat1$wothdet[88727:103729])^2)

# AUC
pred5 <- prediction(as.numeric(outm5$mean$y2[9218:24220]), dat1$wothdet[88727:103729])
auc5 <- performance(pred5, measure = "auc")
auc5 <- auc5@y.values[[1]]

save(outm5, m5, datm5, m5_dev, Em5_dev, Am5_dev, Sm5_dev, 
     brier5, pred5, auc5, initsm5,
     file = "results/out/wothm5.RData")


# End script
