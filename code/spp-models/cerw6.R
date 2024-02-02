
## This code was written by: fiona lunt

## Objective ---------------------------
## Model 6: F (no filtering, no spatial balancing, & no effort for eBird) 
## Cerulean warbler (CERW)
## 
## Input:
##   test1.csv
##   train4.csv
##
## Output: 
##   cerwm6.RData
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
train4 <- read.csv("data/train4.csv")

dat4 <- rbind.fill(train4, test1)


# Model 6: E2 - CAWA ------------------------------------------------------
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836

m6 <- generate.code(dat4)

datm6 <- list(y = c(dat4$cerwdet[1:117936], dat4$cerwtot[117937:144939], dat4$cerwdet[144940:177616]), 
              X = m6$jags.data$X, n = m6$jags.data$n, zero = m6$jags.data$zero,
              S1 = m6$jags.data$S1, S2 = m6$jags.data$S2, S3 = m6$jags.data$S3,
              S4 = m6$jags.data$S4, S5 = m6$jags.data$S5, S6 = m6$jags.data$S6,
              S7 = m6$jags.data$S7,
              hsm = dat4$hsm[117937:144939],
              hsm2 = dat4$hsm[186834:193563], 
              doy = dat4$doy[117937:144939],
              doy2 = dat4$doy[186834:193563],
              doy3 = dat4$doy[144940:177616],
              doy4 = dat4$doy[193564:201836])

initsm6 <- function(){
  list(b = m6$jags.ini$b, 
       lambda = m6$jags.ini$lambda,  
       beta = rep(3, 0.001), 
       z = as.numeric(datm6$y>0))
}

outm6 <- jags(data = datm6, 
              parameters.to.save = c("beta","b","y2"), 
              inits = initsm6, 
              model.file = "models/cerwm6.txt", 
              n.chains = 3, 
              n.thin = 2, 
              n.adapt = 500, 
              n.burnin = 500, 
              n.iter = 2500)


# Performance metrics -----------------------------------------------------

#Full Deviance
m6_yp <- outm6$mean$y2
m6_yt <- c(dat4$cerwdet[177617:186833], dat4$cerwtot[186834:193563], dat4$cerwdet[193564:201836])
m6_yt <- cbind(m6_yt, c(1 - dat4$cerwdet[177617:186833], 5 - dat4$cerwtot[186834:193563], 1 - dat4$cerwdet[193564:201836]))
m6_yp[9218:15947] <- m6_yp[9218:15947]/5
m6_yp <- 0.0001 + m6_yp*0.9998
m6_dev <- -2*sum(log((m6_yp^m6_yt[,1])*((1-m6_yp)^(m6_yt[,2]))))
#eBird deviance
Em6_yp <- outm6$mean$y2[1:9217]
Em6_yt <- c(dat4$cerwdet[177617:186833])
Em6_yt <- cbind(Em6_yt, c(1 - dat4$cerwdet[177617:186833]))
Em6_yp <- 0.0001 + Em6_yp*0.9998
Em6_dev <- -2*sum(log((Em6_yp^Em6_yt[,1])*((1-Em6_yp)^(Em6_yt[,2]))))
#BBA deviance
Am6_yp <- outm6$mean$y2[9218:15947]
Am6_yt <- c(dat4$cerwtot[186834:193563])
Am6_yt <- cbind(Am6_yt, c(5 - dat4$cerwtot[186834:193563])) 
Am6_yp <- Am6_yp/5
Am6_yp <- 0.0001 + Am6_yp*0.9998
Am6_dev <- -2*sum(log((Am6_yp^Am6_yt[,1])*((1-Am6_yp)^(Am6_yt[,2]))))
#BBS deviance
Sm6_yp <- outm6$mean$y2[15948:24220]
Sm6_yt <- c(dat4$cerwdet[193564:201836])
Sm6_yt <- cbind(Sm6_yt, c(1 - dat4$cerwdet[193564:201836]))
Sm6_yp <- 0.0001 + Sm6_yp*0.9998
Sm6_dev <- -2*sum(log((Sm6_yp^Sm6_yt[,1])*((1-Sm6_yp)^(Sm6_yt[,2]))))


# Brier score
brier6 <- mean((outm6$mean$y2[9218:24220] - dat4$cerwdet[186834:201836])^2)

# AUC
pred6 <- prediction(as.numeric(outm6$mean$y2[9218:24220]), dat4$cerwdet[186834:201836])
auc6 <- performance(pred6, measure = "auc")
auc6 <- auc6@y.values[[1]]


save(outm6, m6, datm6, m6_dev, Em6_dev, Am6_dev, Sm6_dev, 
     brier6, pred6, auc6, initsm6,
     file = "results/out/cerwm6.RData")

# End script
