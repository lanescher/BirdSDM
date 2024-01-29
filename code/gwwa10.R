
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 10: E3 (eBird)
## Golden-winged warbler (GWWA) 
##
## Input:
##    covdat.csv
##
## Output: 
##    gwwam10.RData
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
covdat <- read.csv("data/covdat.csv")


# Model 10: E3 - GWWA -----------------------------------------------------
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616,
#test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836,

m10 <- generate.code(covdat)

datm10 <- list(y = c(covdat$gwwatot[117937:144939], covdat$gwwadet[144940:177616]), 
               X = m10$jags.data$X, n = m10$jags.data$n, zero = m10$jags.data$zero,
               S1 = m10$jags.data$S1, S2 = m10$jags.data$S2, S3 = m10$jags.data$S3,
               S4 = m10$jags.data$S4, S5 = m10$jags.data$S5, S6 = m10$jags.data$S6,
               S7 = m10$jags.data$S7, 
               ehours2 = covdat$duration_minutes[177617:186833], 
               ekm2 = covdat$effort_distance_km[177617:186833], 
               hsm = covdat$hsm[117937:144939],
               hsm2 = covdat$hsm[186834:193563], 
               doy = covdat$doy[117937:144939],
               doy2 = covdat$doy[186834:193563],
               doy3 = covdat$doy[144940:177616],
               doy4 = covdat$doy[193564:201836])

initsm10 <- function(){
  list(b = m10$jags.ini$b, 
       lambda = m10$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = c(rep(NA, 117936), as.numeric(datm10$y>0)))
}

outm10 <- jags(data = datm10, 
               parameters.to.save = c("beta","b","y2"), 
               inits = initsm10, 
               model.file = "models/gwwam10.txt",  
               n.chains = 3, 
               n.thin = 2, 
               n.adapt = 500, 
               n.burnin = 500, 
               n.iter = 2500,
               parallel = TRUE)


### Performance metrics -----------------------------------------------------

#Full Deviance
m10_yp <- outm10$mean$y2
m10_yt <- c(covdat$gwwadet[177617:186833], covdat$gwwatot[186834:193563], covdat$gwwadet[193564:201836])
m10_yt <- cbind(m10_yt, c(1 - covdat$gwwadet[177617:186833], 5 - covdat$gwwatot[186834:193563], 1 - covdat$gwwadet[193564:201836]))
m10_yp[9218:15947] <- m10_yp[9218:15947]/5
m10_yp <- 0.0001 + m10_yp*0.9998
m10_dev <- -2*sum(log((m10_yp^m10_yt[,1])*((1-m10_yp)^(m10_yt[,2]))))
#eBird deviance
Em10_yp <- outm10$mean$y2[1:9217]
Em10_yt <- c(covdat$gwwadet[177617:186833])
Em10_yt <- cbind(Em10_yt, c(1 - covdat$gwwadet[177617:186833]))
Em10_yp <- 0.0001 + Em10_yp*0.9998
Em10_dev <- -2*sum(log((Em10_yp^Em10_yt[,1])*((1-Em10_yp)^(Em10_yt[,2]))))
#BBA deviance
Am10_yp <- outm10$mean$y2[9218:15947]
Am10_yt <- c(covdat$gwwatot[186834:193563])
Am10_yt <- cbind(Am10_yt, c(5 - covdat$gwwatot[186834:193563])) 
Am10_yp <- Am10_yp/5
Am10_yp <- 0.0001 + Am10_yp*0.9998
Am10_dev <- -2*sum(log((Am10_yp^Am10_yt[,1])*((1-Am10_yp)^(Am10_yt[,2]))))
#BBS deviance
Sm10_yp <- outm10$mean$y2[15948:24220]
Sm10_yt <- c(covdat$gwwadet[193564:201836])
Sm10_yt <- cbind(Sm10_yt, c(1 - covdat$gwwadet[193564:201836]))
Sm10_yp <- 0.0001 + Sm10_yp*0.9998
Sm10_dev <- -2*sum(log((Sm10_yp^Sm10_yt[,1])*((1-Sm10_yp)^(Sm10_yt[,2]))))


# Brier score
brier10 <- mean((outm10$mean$y2[9218:24220] - covdat$gwwadet[186834:201836])^2)

# AUC
pred10 <- prediction(as.numeric(outm10$mean$y2[9218:24220]), covdat$gwwadet[186834:201836])
auc10 <- performance(pred10, measure = "auc")
auc10 <- auc10@y.values[[1]]


save(outm10, m10, datm10, m10_dev, Em10_dev, Am10_dev, Sm10_dev, 
     brier10, pred10, auc10, initsm10,
     file = "results/out/gwwam10.RData")

# End scripts
