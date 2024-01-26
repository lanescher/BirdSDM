
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 8: E1 (false-positive model for eBird, joint-likelihood)
## Golden-winged warbler (GWWA) 
##
## Input:
##    covdat.csv
##
## Output: 
##    gwwam8.RData
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
covdat <- read.csv("FinalData/TestTrain/covdat.csv")


# Model 8: E1 - GWWA ------------------------------------------------------
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836

m8 <- generate.code(covdat)

datm8 <- list(y = c(covdat$gwwatot[117937:144939], covdat$gwwadet[144940:177616]), 
              X = m8$jags.data$X, n = m8$jags.data$n, zero = m8$jags.data$zero,
              S1 = m8$jags.data$S1, S2 = m8$jags.data$S2, S3 = m8$jags.data$S3,
              S4 = m8$jags.data$S4, S5 = m8$jags.data$S5, S6 = m8$jags.data$S6,
              S7 = m8$jags.data$S7, S8 = m8$jags.data$S8, S9 = m8$jags.data$S9,
              ehours2 = covdat$duration_minutes[177617:186833], 
              ekm2 = covdat$effort_distance_km[177617:186833], 
              hsm = covdat$hsm[117937:144939],
              hsm2 = covdat$hsm[186834:193563], 
              doy = covdat$doy[117937:144939],
              doy2 = covdat$doy[186834:193563],
              doy3 = covdat$doy[144940:177616],
              doy4 = covdat$doy[193564:201836])

initsm8 <- function(){
  list(b = m8$jags.ini$b, 
       lambda = m8$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = c(rep(NA, 117936), as.numeric(datm8$y>0)))
}

outm8 <- jags(data = datm8, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm8, 
              model.file = "models/gwwam8.txt", 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500,
              parallel = TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m8_yp <- outm8$mean$y2
m8_yt <- c(covdat$gwwadet[177617:186833], covdat$gwwatot[186834:193563], covdat$gwwadet[193564:201836])
m8_yt <- cbind(m8_yt, c(1 - covdat$gwwadet[177617:186833], 5 - covdat$gwwatot[186834:193563], 1 - covdat$gwwadet[193564:201836]))
m8_yp[9218:15947] <- m8_yp[9218:15947]/5
m8_yp <- 0.0001 + m8_yp*0.9998
m8_dev <- -2*sum(log((m8_yp^m8_yt[,1])*((1-m8_yp)^(m8_yt[,2]))))
#eBird deviance
Em8_yp <- outm8$mean$y2[1:9217]
Em8_yt <- c(covdat$gwwadet[177617:186833])
Em8_yt <- cbind(Em8_yt, c(1 - covdat$gwwadet[177617:186833]))
Em8_yp <- 0.0001 + Em8_yp*0.9998
Em8_dev <- -2*sum(log((Em8_yp^Em8_yt[,1])*((1-Em8_yp)^(Em8_yt[,2]))))
#BBA deviance
Am8_yp <- outm8$mean$y2[9218:15947]
Am8_yt <- c(covdat$gwwatot[186834:193563])
Am8_yt <- cbind(Am8_yt, c(5 - covdat$gwwatot[186834:193563])) 
Am8_yp <- Am8_yp/5
Am8_yp <- 0.0001 + Am8_yp*0.9998
Am8_dev <- -2*sum(log((Am8_yp^Am8_yt[,1])*((1-Am8_yp)^(Am8_yt[,2]))))
#BBS deviance
Sm8_yp <- outm8$mean$y2[15948:24220]
Sm8_yt <- c(covdat$gwwadet[193564:201836])
Sm8_yt <- cbind(Sm8_yt, c(1 - covdat$gwwadet[193564:201836]))
Sm8_yp <- 0.0001 + Sm8_yp*0.9998
Sm8_dev <- -2*sum(log((Sm8_yp^Sm8_yt[,1])*((1-Sm8_yp)^(Sm8_yt[,2]))))


# Brier score
brier8 <- mean((outm8$mean$y2[9218:24220] - covdat$gwwadet[186834:201836])^2)

# AUC
pred8 <- prediction(as.numeric(outm8$mean$y2[9218:24220]), covdat$gwwadet[186834:201836])
auc8 <- performance(pred8, measure = "auc")
auc8 <- auc8@y.values[[1]]

save(outm8, m8, datm8, m8_dev, Em8_dev, Am8_dev, Sm8_dev,
     brier8, pred8, auc8, initsm8,
     file = "results/out/gwwam8.RData")


# End script
