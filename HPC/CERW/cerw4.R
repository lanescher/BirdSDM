
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 4: F3 (no eBird filtering or spatial balancing)
## Cerulean warbler (CERW)
## 
## Input:
##   test1.csv
##   train4.csv
##
## Output: 
##   cerwm4.RData
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
train4 <- read.csv(paste0(path,"data/train4.csv"))

dat4 <- rbind.fill(train4, test1)


# Model 4: F3 - CERW ------------------------------------------------------
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS-193564:201836

m4 <- generate.code(dat4)

datm4 <- list(y = c(dat4$cerwdet[1:117936], dat4$cerwtot[117937:144939], dat4$cerwdet[144940:177616]), 
              X = m4$jags.data$X, n = m4$jags.data$n, zero = m4$jags.data$zero,
              S1 = m4$jags.data$S1, S2 = m4$jags.data$S2, S3 = m4$jags.data$S3,
              S4 = m4$jags.data$S4, S5 = m4$jags.data$S5, S6 = m4$jags.data$S6,
              S7 = m4$jags.data$S7, 
              ehours = dat4$duration_minutes[1:117936], 
              ekm = dat4$effort_distance_km[1:117936], 
              ehours2 = dat4$duration_minutes[177617:186833], 
              ekm2 = dat4$effort_distance_km[177617:186833], 
              hsm = dat4$hsm[117937:144939],
              hsm2 = dat4$hsm[186834:193563], 
              doy = dat4$doy[117937:144939],
              doy2 = dat4$doy[186834:193563],
              doy3 = dat4$doy[144940:177616],
              doy4 = dat4$doy[193564:201836])

initsm4 <- function(){
  list(b = m4$jags.ini$b, 
       lambda = m4$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm4$y>0))
}

outm4 <- jags(data = datm4, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm4, 
              model.file = paste0(path,"models/cerwm4.txt"), 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500)


# Performance metrics -----------------------------------------------------

#Full Deviance
m4_yp <- outm4$mean$y2
m4_yt <- c(dat4$cerwdet[177617:186833], dat4$cerwtot[186834:193563], dat4$cerwdet[193564:201836])
m4_yt <- cbind(m4_yt, c(1 - dat4$cerwdet[177617:186833], 5 - dat4$cerwtot[186834:193563], 1 - dat4$cerwdet[193564:201836]))
m4_yp[9218:15947] <- m4_yp[9218:15947]/5
m4_yp <- 0.0001 + m4_yp*0.9998
m4_dev <- -2*sum(log((m4_yp^m4_yt[,1])*((1-m4_yp)^(m4_yt[,2]))))
#eBird deviance
Em4_yp <- outm4$mean$y2[1:9217]
Em4_yt <- c(dat4$cerwdet[177617:186833])
Em4_yt <- cbind(Em4_yt, c(1 - dat4$cerwdet[177617:186833]))
Em4_yp <- 0.0001 + Em4_yp*0.9998
Em4_dev <- -2*sum(log((Em4_yp^Em4_yt[,1])*((1-Em4_yp)^(Em4_yt[,2]))))
#BBA deviance
Am4_yp <- outm4$mean$y2[9218:15947]
Am4_yt <- c(dat4$cerwtot[186834:193563])
Am4_yt <- cbind(Am4_yt, c(5 - dat4$cerwtot[186834:193563])) 
Am4_yp <- Am4_yp/5
Am4_yp <- 0.0001 + Am4_yp*0.9998
Am4_dev <- -2*sum(log((Am4_yp^Am4_yt[,1])*((1-Am4_yp)^(Am4_yt[,2]))))
#BBS deviance
Sm4_yp <- outm4$mean$y2[15948:24220]
Sm4_yt <- c(dat4$cerwdet[193564:201836])
Sm4_yt <- cbind(Sm4_yt, c(1 - dat4$cerwdet[193564:201836]))
Sm4_yp <- 0.0001 + Sm4_yp*0.9998
Sm4_dev <- -2*sum(log((Sm4_yp^Sm4_yt[,1])*((1-Sm4_yp)^(Sm4_yt[,2]))))


# Brier score
brier4 <- mean((outm4$mean$y2[9218:24220] - dat4$cerwdet[186834:201836])^2)


# AUC
pred4 <- prediction(as.numeric(outm4$mean$y2[9218:24220]), dat4$cerwdet[186834:201836])
auc4 <- performance(pred4, measure = "auc")
auc4 <- auc4@y.values[[1]]


save(outm4, m4, datm4, m4_dev, Em4_dev, Am4_dev, Sm4_dev, 
     brier4, pred4, auc4, initsm4,
     file = paste0(path,"results/out/cerwm4.RData"))


# End script


