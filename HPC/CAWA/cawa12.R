
## This code was written by: fiona lunt

## ---------------------------
## Objective: 
##  Model 12: R2 (full model, validate on current; train1test2)
##  Canada warbler (CAWA)
## 
## Input:
##  train1.csv
##  test2.csv
##
## Output: 
##  cawam12.RData
##
## ---------------------------

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


## load data --------------------------------
train1 <- read.csv(paste0(path,"data/train1.csv"))
test2 <- read.csv(paste0(path,"data/test2.csv"))

dat12 <- rbind.fill(train1, test2)


# Model 12: R2 - (CAWA) ---------------------------------------------------
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509,
#test: eBird- 79510:84969, BBS- 84970:87201

m12 <- generate.code(dat12)

datm12 <- list(y = c(dat12$cawadet[1:19829], dat12$cawatot[19830:46832], dat12$cawadet[46833:79509]), 
               X = m12$jags.data$X, n = m12$jags.data$n, zero = m12$jags.data$zero,
               S1 = m12$jags.data$S1, S2 = m12$jags.data$S2, S3 = m12$jags.data$S3,
               S4 = m12$jags.data$S4, S5 = m12$jags.data$S5, S6 = m12$jags.data$S6,
               S7 = m12$jags.data$S7, S8 = m12$jags.data$S8, S9 = m12$jags.data$S9, 
               ehours = dat12$duration_minutes[1:19829], 
               ekm = dat12$effort_distance_km[1:19829],
               ehours2 = dat12$duration_minutes[79510:84969], 
               ekm2 = dat12$effort_distance_km[79510:84969], 
               hsm = dat12$hsm[19830:46832],
               doy = dat12$doy[19830:46832],
               doy3 = dat12$doy[46833:79509],
               doy4 = dat12$doy[84970:87201])

initsm12 <- function(){
  list(b = m12$jags.ini$b, 
       lambda = m12$jags.ini$lambda, 
       beta = rep(3, 0.001), 
       z = as.numeric(datm12$y>0))
}

outm12 <- jags(data = datm12, 
               parameters.to.save = c("beta","b","y2"), 
               inits = initsm12, 
               model.file = paste0(path,"models/cawam12.txt"), 
               n.chains = 3, 
               n.thin = 2, 
               n.adapt = 500, 
               n.burnin = 500, 
               n.iter = 2500, 
               parallel=TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m12_yp <- outm12$mean$y2
m12_yt <- c(dat12$cawadet[79510:84969], dat12$cawadet[84970:87201])
m12_yt <- cbind(m12_yt, c(1 - dat12$cawadet[79510:84969], 1 - dat12$cawadet[84970:87201]))
m12_yp <- 0.0001 + m12_yp*0.9998
m12_dev <- -2*sum(log((m12_yp^m12_yt[,1])*((1-m12_yp)^(m12_yt[,2]))))
#eBird deviance
Em12_yp <- outm12$mean$y2[1:5460]
Em12_yt <- c(dat12$cawadet[79510:84969])
Em12_yt <- cbind(Em12_yt, c(1 - dat12$cawadet[79510:84969]))
Em12_yp <- 0.0001 + Em12_yp*0.9998
Em12_dev <- -2*sum(log((Em12_yp^Em12_yt[,1])*((1-Em12_yp)^(Em12_yt[,2]))))
#BBS deviance
Sm12_yp <- outm12$mean$y2[5461:7692]
Sm12_yt <- c(dat12$cawadet[84970:87201])
Sm12_yt <- cbind(Sm12_yt, c(1 - dat12$cawadet[84970:87201]))
Sm12_yp <- 0.0001 + Sm12_yp*0.9998
Sm12_dev <- -2*sum(log((Sm12_yp^Sm12_yt[,1])*((1-Sm12_yp)^(Sm12_yt[,2]))))


# Brier score
brier12 <- mean((outm12$mean$y2 - dat12$cawadet[79510:87201])^2)


# AUC
pred12 <- prediction(as.numeric(outm12$mean$y2), dat12$cawadet[79510:87201])
auc12 <- performance(pred12, measure = "auc")
auc12 <- auc12@y.values[[1]]


# Save output
save(outm12, m12, datm12, m12_dev, Em12_dev, Sm12_dev, 
     brier12, pred12, auc12, initsm12,
     file = paste0(path,"results/out/cawam12.RData"))


# End script
