
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 8: E2 (eBird covariate model)
## Cerulean warbler (CERW) 
##
## Input:
##    covdat.csv
##
## Output: 
##    cerwm8.RData
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
covdat <- read.csv("data/covdat.csv")


# Model 8: E2 - CERW ------------------------------------------------------
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836

m8 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) +
              s(ppt, k=10) + s(dev, k=10) + s(forest, k=10) + #s(can, k=10) +
              s(road, k=10) + cerwct_ebd + lists + s(longitude, latitude, bs='ds', k=100),
            data = covdat, family='binomial', file = 'placeholder.txt')

datm8 <- list(y = c(covdat$cerwtot[117937:144939], covdat$cerwdet[144940:177616]), 
              X = m8$jags.data$X, n = m8$jags.data$n, zero = m8$jags.data$zero,
              S1 = m8$jags.data$S1, S2 = m8$jags.data$S2, S3 = m8$jags.data$S3,
              S4 = m8$jags.data$S4, S5 = m8$jags.data$S5, S6 = m8$jags.data$S6,
              S7 = m8$jags.data$S7, 
              ehours2 = covdat$duration_minutes[177617:186833], 
              ekm2 = covdat$effort_distance_km[177617:186833], 
              hsm = covdat$hsm[117937:144939],
              hsm2 = covdat$hsm[186834:193563], 
              doy = covdat$doy[117937:144939],
              doy2 = covdat$doy[186834:193563],
              doy3 = covdat$doy[144940:177616],
              doy4 = covdat$doy[193564:201836])

initsm8 <- function(){
  b = m8$jags.ini$b
  b[3] = -0.001
  list(b = b,
       lambda = m8$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = c(rep(NA, 117936), as.numeric(datm8$y>0)))
}

outm8 <- jags(data = datm8, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm8, 
              model.file = "models/cerwm8.txt", 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500)


### Performance metrics -----------------------------------------------------

#Full Deviance
m8_yp <- outm8$mean$y2
m8_yt <- c(covdat$cerwdet[177617:186833], covdat$cerwtot[186834:193563], covdat$cerwdet[193564:201836])
m8_yt <- cbind(m8_yt, c(1 - covdat$cerwdet[177617:186833], 5 - covdat$cerwtot[186834:193563], 1 - covdat$cerwdet[193564:201836]))
m8_yp[9218:15947] <- m8_yp[9218:15947]/5
m8_yp <- 0.0001 + m8_yp*0.9998
m8_dev <- -2*sum(log((m8_yp^m8_yt[,1])*((1-m8_yp)^(m8_yt[,2]))))
#eBird deviance
Em8_yp <- outm8$mean$y2[1:9217]
Em8_yt <- c(covdat$cerwdet[177617:186833])
Em8_yt <- cbind(Em8_yt, c(1 - covdat$cerwdet[177617:186833]))
Em8_yp <- 0.0001 + Em8_yp*0.9998
Em8_dev <- -2*sum(log((Em8_yp^Em8_yt[,1])*((1-Em8_yp)^(Em8_yt[,2]))))
#BBA deviance
Am8_yp <- outm8$mean$y2[9218:15947]
Am8_yt <- c(covdat$cerwtot[186834:193563])
Am8_yt <- cbind(Am8_yt, c(5 - covdat$cerwtot[186834:193563])) 
Am8_yp <- Am8_yp/5
Am8_yp <- 0.0001 + Am8_yp*0.9998
Am8_dev <- -2*sum(log((Am8_yp^Am8_yt[,1])*((1-Am8_yp)^(Am8_yt[,2]))))
#BBS deviance
Sm8_yp <- outm8$mean$y2[15948:24220]
Sm8_yt <- c(covdat$cerwdet[193564:201836])
Sm8_yt <- cbind(Sm8_yt, c(1 - covdat$cerwdet[193564:201836]))
Sm8_yp <- 0.0001 + Sm8_yp*0.9998
Sm8_dev <- -2*sum(log((Sm8_yp^Sm8_yt[,1])*((1-Sm8_yp)^(Sm8_yt[,2]))))


# Brier score
brier8 <- mean((outm8$mean$y2[9218:24220] - covdat$cerwdet[186834:201836])^2)


# AUC
pred8 <- prediction(as.numeric(outm8$mean$y2[9218:24220]), covdat$cerwdet[186834:201836])
auc8 <- performance(pred8, measure = "auc")
auc8 <- auc8@y.values[[1]]


save(outm8, m8, datm8, m8_dev, Em8_dev, Am8_dev, Sm8_dev, 
     brier8, pred8, auc8, initsm8,
     file = "results/out/cerwm8.RData")


# End script
