
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 2: F1 (unfiltered eBird) 
## Canada warbler (CAWA)
## 
## Input:
##   test1.csv
##   train2.csv
##
## Output: 
##   cawam2.RData
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
# Generate code for GAMs
generate.code <- function(dat) {
  jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) +
          s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + #s(forest, k=10) +
          s(road, k=10) + s(longitude, latitude, bs='ds', k=100),
        data = dat, family='binomial', file = paste0(path,'placeholder.txt'))
}

## load data ---------------------------
test1 <- read.csv(paste0(path,"data/test1.csv"))
train2 <- read.csv(paste0(path,"data/train2.csv"))

dat2 <- rbind.fill(train2, test1)


# Model 2: F1 - CAWA ------------------------------------------------------
#train: eBird- 1:20361, BBA- 20362:47364, BBS- 47365:80041
#test: eBird- 80042:89258, BBA- 89259:95988, BBS- 95989:104261

m2 <- generate.code(dat2)

datm2 <- list(y = c(dat2$cawadet[1:20361], dat2$cawatot[20362:47364], dat2$cawadet[47365:80041]), 
              X = m2$jags.data$X, n = m2$jags.data$n, zero = m2$jags.data$zero,
              S1 = m2$jags.data$S1, S2 = m2$jags.data$S2, S3 = m2$jags.data$S3,
              S4 = m2$jags.data$S4, S5 = m2$jags.data$S5, S6 = m2$jags.data$S6,
              S7 = m2$jags.data$S7, S8 = m2$jags.data$S8, S9 = m2$jags.data$S9,
              ehours = dat2$duration_minutes[1:20361], 
              ekm = dat2$effort_distance_km[1:20361], 
              ehours2 = dat2$duration_minutes[80042:89258], 
              ekm2 = dat2$effort_distance_km[80042:89258], 
              hsm = dat2$hsm[20362:47364],
              hsm2 = dat2$hsm[89259:95988], 
              doy = dat2$doy[20362:47364],
              doy2 = dat2$doy[89259:95988],
              doy3 = dat2$doy[47365:80041],
              doy4 = dat2$doy[95989:104261])

initsm2 <- function(){
  list(b = m2$jags.ini$b, 
       lambda = m2$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm2$y>0))
}

outm2 <- jags(data = datm2, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm2, 
              model.file = paste0(path,"models/cawam2.txt"), 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500)


# Performance metrics -----------------------------------------------------

#Full Deviance
m2_yp <- outm2$mean$y2
m2_yt <- c(dat2$cawadet[80042:89258], dat2$cawatot[89259:95988], dat2$cawadet[95989:104261])
m2_yt <- cbind(m2_yt, c(1 - dat2$cawadet[80042:89258], 5 - dat2$cawatot[89259:95988], 1 - dat2$cawadet[95989:104261]))
m2_yp[9218:15947] <- m2_yp[9218:15947]/5
m2_yp <- 0.0001 + m2_yp*0.9998
m2_dev <- -2*sum(log((m2_yp^m2_yt[,1])*((1-m2_yp)^(m2_yt[,2]))))
#eBird deviance
Em2_yp <- outm2$mean$y2[1:9217]
Em2_yt <- c(dat2$cawadet[80042:89258])
Em2_yt <- cbind(Em2_yt, c(1 - dat2$cawadet[80042:89258]))
Em2_yp <- 0.0001 + Em2_yp*0.9998
Em2_dev <- -2*sum(log((Em2_yp^Em2_yt[,1])*((1-Em2_yp)^(Em2_yt[,2]))))
#BBA deviance
Am2_yp <- outm2$mean$y2[9218:15947]
Am2_yt <- c(dat2$cawatot[89259:95988])
Am2_yt <- cbind(Am2_yt, c(5 - dat2$cawatot[89259:95988]))
Am2_yp <- Am2_yp/5
Am2_yp <- 0.0001 + Am2_yp*0.9998
Am2_dev <- -2*sum(log((Am2_yp^Am2_yt[,1])*((1-Am2_yp)^(Am2_yt[,2]))))
#BBS deviance
Sm2_yp <- outm2$mean$y2[15948:24220]
Sm2_yt <- c(dat2$cawadet[95989:104261])
Sm2_yt <- cbind(Sm2_yt, c(1 - dat2$cawadet[95989:104261]))
Sm2_yp <- 0.0001 + Sm2_yp*0.9998
Sm2_dev <- -2*sum(log((Sm2_yp^Sm2_yt[,1])*((1-Sm2_yp)^(Sm2_yt[,2]))))


# Brier score
brier2 <- mean((outm2$mean$y2[9218:24220] - dat2$cawadet[89259:104261])^2)

# AUC
pred2 <- prediction(as.numeric(outm2$mean$y2[9218:24220]), dat2$cawadet[89259:104261])
auc2 <- performance(pred2, measure = "auc")
auc2 <- auc2@y.values[[1]]


save(outm2, m2, datm2, m2_dev, Em2_dev, Am2_dev, Sm2_dev, 
     brier2, pred2, auc2, initsm2,
     file = paste0(path,"results/out/cawam2.RData"))

# End script
