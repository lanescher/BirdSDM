
## This code was written by: fiona lunt

## Objective ---------------------------
## 
## Canada Warbler models 1-7
##
## Input:
##   
##
## Output: 
##
##

## load packages ---------------------------
library(tidyverse)
library(magrittr)
library(dplyr)
library(tibble)
library(plyr)
library(mgcv)
library(jagsUI)
library(ROCR)

## load functions ---------------------------


## load data ---------------------------
test1 <- read.csv("data/test1.csv")
train1 <- read.csv("data/train1.csv")
train2 <- read.csv("data/train2.csv")
train3 <- read.csv("data/train3.csv")
train4 <- read.csv("data/train4.csv")

dat1 <- rbind.fill(train1, test1)
dat2 <- rbind.fill(train2, test1)
dat3 <- rbind.fill(train3, test1)
dat4 <- rbind.fill(train4, test1)


# Model 1: R1 - CAWA ------------------------------------------------------
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
train.eBird = 1:19829
train.BBA = 19830:46832
train.BBS = 46833:79509
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729
test.eBird = 79510:88726
test.BBA = 88727:95456
test.BBS = 95457:103729

# m1 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) + 
#               s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + #s(forest, k=10) + 
#               s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
#             data = dat1, family='binomial', file = 'models/0_placeholder.txt')
# 
# # Use the output found in placeholder.txt in the JAGS model file: cawam1.txt
# 
# datm1 <- list(y = c(dat1$cawadet[train.eBird], 
#                     dat1$cawatot[train.BBA], 
#                     dat1$cawadet[train.BBS]), 
#               X = m1$jags.data$X, n = m1$jags.data$n, zero = m1$jags.data$zero,
#               S1 = m1$jags.data$S1, S2 = m1$jags.data$S2, S3 = m1$jags.data$S3,
#               S4 = m1$jags.data$S4, S5 = m1$jags.data$S5, S6 = m1$jags.data$S6,
#               S7 = m1$jags.data$S7, #S8 = m1$jags.data$S8, S9 = m1$jags.data$S9, 
#               ehours = dat1$duration_minutes[train.eBird], 
#               ekm = dat1$effort_distance_km[train.eBird],
#               ehours2 = dat1$duration_minutes[test.eBird], 
#               ekm2 = dat1$effort_distance_km[test.eBird], 
#               hsm = dat1$hsm[train.BBA],
#               hsm2 = dat1$hsm[test.BBA], 
#               doy = dat1$doy[train.BBA],
#               doy2 = dat1$doy[test.BBA],
#               doy3 = dat1$doy[train.BBS],
#               doy4 = dat1$doy[test.BBS])
# 
initsm1 <- function(){
  list(b = m1$jags.ini$b,
       lambda = m1$jags.ini$lambda,
       beta = rep(3, 0.001),
       z = as.numeric(datm1$y>0))
}
# 
# outm1 <- jags(data = datm1, 
#               parameters.to.save = c("beta","b","y2"), 
#               inits = initsm1, 
#               model.file = "models/0_cawam1.txt", 
#               n.chains = 3, 
#               n.thin = 2, 
#               n.adapt = 500, 
#               n.burnin = 500, 
#               n.iter = 2500)

### Performance metrics -----------------------------------------------------

#Full Deviance
# m1_yp <- outm1$mean$y2
# m1_yt <- c(dat1$cawadet[test.eBird], dat1$cawatot[test.BBA], dat1$cawadet[test.BBS])
# m1_yt <- cbind(m1_yt, c(1 - dat1$cawadet[test.eBird], 5 - dat1$cawatot[test.BBA], 1 - dat1$cawadet[test.BBS]))
# m1_yp[9218:15947] <- m1_yp[9218:15947]/5
# m1_yp <- 0.0001 + m1_yp*0.9998
# m1_dev <- -2*sum(log((m1_yp^m1_yt[,1])*((1-m1_yp)^(m1_yt[,2]))))
# #eBird deviance
# Em1_yp <- outm1$mean$y2[1:9217]
# Em1_yt <- c(dat1$cawadet[test.eBird])
# Em1_yt <- cbind(Em1_yt, c(1 - dat1$cawadet[test.eBird]))
# Em1_yp <- 0.0001 + Em1_yp*0.9998
# Em1_dev <- -2*sum(log((Em1_yp^Em1_yt[,1])*((1-Em1_yp)^(Em1_yt[,2]))))
# #BBA deviance
# Am1_yp <- outm1$mean$y2[9218:15947]
# Am1_yt <- c(dat1$cawatot[test.BBA])
# Am1_yt <- cbind(Am1_yt, c(5 - dat1$cawatot[test.BBA]))
# Am1_yp <- Am1_yp/5
# Am1_yp <- 0.0001 + Am1_yp*0.9998
# Am1_dev <- -2*sum(log((Am1_yp^Am1_yt[,1])*((1-Am1_yp)^(Am1_yt[,2]))))
# #BBS deviance
# Sm1_yp <- outm1$mean$y2[15948:24220]
# Sm1_yt <- c(dat1$cawadet[test.BBS])
# Sm1_yt <- cbind(Sm1_yt, c(1 - dat1$cawadet[test.BBS]))
# Sm1_yp <- 0.0001 + Sm1_yp*0.9998
# Sm1_dev <- -2*sum(log((Sm1_yp^Sm1_yt[,1])*((1-Sm1_yp)^(Sm1_yt[,2]))))
# 
# 
# # Brier score
# brier1 <- mean((outm1$mean$y2[9218:24220] - dat1$cawadet[88727:103729])^2)
# 
# 
# # AUC
# pred1 <- prediction(as.numeric(outm1$mean$y2[9218:24220]), dat1$cawadet[88727:103729])
# auc1 <- performance(pred1, measure = "auc")
# auc1 <- auc1@y.values[[1]]
# 
# # roc1 <- roc(dat1$cawadet[88642:103748], as.numeric(outm1$mean$y2[9218:24220]),
# # ci.auc=TRUE, auc=TRUE, parallel=TRUE)
# 
# 
# # Save CAWA M1 file
# save(outm1, m1, datm1, m1_dev, Em1_dev, Am1_dev, Sm1_dev, 
#      brier1, pred1, auc1,
#      file = "results/out/cawam1.RData")


### GAMs --------------------------------------------------------------------

#Sim2Jam
require(rjags)
load("results/out/cawam1.RData")

jm1 <- jags.model("models/0_cawam1.txt", 
                  data=datm1, 
                  inits=initsm1, 
                  n.adapt=500, 
                  n.chains=3)
update(jm1, 500)

sam1 <- jags.samples(jm1, c("b","rho"), n.iter=2000, thin=2)
jam1 <- sim2jam(sam1, m1$pregam)

save(m1, datm1, jm1, sam1, jam1,
     file = "results/jams/cawam1jam.RData")

# Base plotting of GAMs
# pdf("FinalResults/Plots/cawam1cov.pdf")
# plot(jam1, pages=1)
# dev.off()


# Clear workspace for next run
rm(list=setdiff(ls(), c('test1','train1','train2','train3','train4',
                        'dat1','dat2','dat3','dat4')))



# Model 2: F1 - CAWA (no eBird filtering) ---------------------------------
#train: eBird- 1:20361, BBA- 20362:47364, BBS- 47365:80041
# train.eBird = 1:20361
# train.BBA = 20362:47364
# train.BBS = 47365:80041
# #test: eBird- 80042:89258, BBA- 89259:95988, BBS- 95989:104261
# test.eBird = 80042:89258
# test.BBA = 89259:95988
# test.BBS = 95989:104261
# 
# m2 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) + 
#               s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + #s(forest, k=10) + 
#               s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
#             data = dat2, family='binomial', file = 'placeholder.txt')
# 
# datm2 <- list(y = c(dat2$cawadet[train.eBird], dat2$cawatot[train.BBA], 
#                     dat2$cawadet[train.BBS]), 
#               X = m2$jags.data$X, n = m2$jags.data$n, zero = m2$jags.data$zero,
#               S1 = m2$jags.data$S1, S2 = m2$jags.data$S2, S3 = m2$jags.data$S3,
#               S4 = m2$jags.data$S4, S5 = m2$jags.data$S5, S6 = m2$jags.data$S6,
#               S7 = m2$jags.data$S7, #S8 = m2$jags.data$S8, S9 = m2$jags.data$S9,
#               ehours = dat2$duration_minutes[train.eBird], 
#               ekm = dat2$effort_distance_km[train.eBird], 
#               ehours2 = dat2$duration_minutes[test.eBird], 
#               ekm2 = dat2$effort_distance_km[test.eBird], 
#               hsm = dat2$hsm[train.BBA],
#               hsm2 = dat2$hsm[test.BBA], 
#               doy = dat2$doy[train.BBA],
#               doy2 = dat2$doy[test.BBA],
#               doy3 = dat2$doy[train.BBS],
#               doy4 = dat2$doy[test.BBS])
# 
# 
# initsm2 <- function(){
#   list(b = m2$jags.ini$b, 
#        lambda = m2$jags.ini$lambda,  
#        beta = rep(3, 0.001), 
#        z = as.numeric(datm2$y>0))
# }
# 
# outm2 <- jags(data = datm2, 
#               parameters.to.save = c("beta","b","y2"), 
#               inits = initsm2, 
#               model.file = "models/cawam2.txt", 
#               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)
# 
# 
# ### Performance metrics -----------------------------------------------------
# 
# #Full Deviance
# m2_yp <- outm2$mean$y2
# m2_yt <- c(dat2$cawadet[test.eBird], dat2$cawatot[test.BBA], dat2$cawadet[test.BBS])
# m2_yt <- cbind(m2_yt, c(1 - dat2$cawadet[test.eBird], 5 - dat2$cawatot[test.BBA], 1 - dat2$cawadet[test.BBS]))
# m2_yp[9218:15947] <- m2_yp[9218:15947]/5
# m2_yp <- 0.0001 + m2_yp*0.9998
# m2_dev <- -2*sum(log((m2_yp^m2_yt[,1])*((1-m2_yp)^(m2_yt[,2]))))
# #eBird deviance
# Em2_yp <- outm2$mean$y2[1:9217]
# Em2_yt <- c(dat2$cawadet[test.eBird])
# Em2_yt <- cbind(Em2_yt, c(1 - dat2$cawadet[test.eBird]))
# Em2_yp <- 0.0001 + Em2_yp*0.9998
# Em2_dev <- -2*sum(log((Em2_yp^Em2_yt[,1])*((1-Em2_yp)^(Em2_yt[,2]))))
# #BBA deviance
# Am2_yp <- outm2$mean$y2[9218:15947]
# Am2_yt <- c(dat2$cawatot[test.BBA])
# Am2_yt <- cbind(Am2_yt, c(5 - dat2$cawatot[test.BBA]))
# Am2_yp <- Am2_yp/5
# Am2_yp <- 0.0001 + Am2_yp*0.9998
# Am2_dev <- -2*sum(log((Am2_yp^Am2_yt[,1])*((1-Am2_yp)^(Am2_yt[,2]))))
# #BBS deviance
# Sm2_yp <- outm2$mean$y2[15948:24220]
# Sm2_yt <- c(dat2$cawadet[test.BBS])
# Sm2_yt <- cbind(Sm2_yt, c(1 - dat2$cawadet[test.BBS]))
# Sm2_yp <- 0.0001 + Sm2_yp*0.9998
# Sm2_dev <- -2*sum(log((Sm2_yp^Sm2_yt[,1])*((1-Sm2_yp)^(Sm2_yt[,2]))))
# 
# 
# # Brier score
# brier2 <- mean((outm2$mean$y2[9218:24220] - dat2$cawadet[89259:104261])^2)
# 
# 
# # AUC
# pred2 <- prediction(as.numeric(outm2$mean$y2[9218:24220]), dat2$cawadet[89259:104261])
# auc2 <- performance(pred2, measure = "auc")
# auc2 <- auc2@y.values[[1]]
# 
# #roc2 <- roc(dat2$cawadet[89168:104274], as.numeric(outm2$mean$y2[9218:24220]), ci.auc=TRUE, auc=TRUE, parallel=TRUE)
# 
# save(outm2, m2, datm2, m2_dev, Em2_dev, Am2_dev, Sm2_dev,
#      brier2, pred2, auc2,
#      file = "results/out/cawam2.RData")
# 
# 
# ### GAMs --------------------------------------------------------------------
# 
# #Sim2Jam
# require(rjags)
# # load("results/out/cawam2.RData")
# 
# jm2 <- jags.model("models/cawam2.txt", 
#                   data=datm2, 
#                   inits=initsm2, 
#                   n.adapt=500, 
#                   n.chains=3)
# update(jm2, 500)
# sam2 <- jags.samples(jm2, c("b","rho"), n.iter=2000, thin=2)
# jam2 <- sim2jam(sam2, m2$pregam)
# 
# save(m2, jm2, sam2, jam2,
#      file = "results/jams/cawam2jam.RData")
# 
# 
# # Clear workspace for next run
# clear.workspace()
# 
# 
# 
# 
# # Model 3: F2 - CAWA (no eBird spatial balancing) -------------------------
# #train: eBird- 1:111728, BBA- 111729:138731, BBS- 138732:171408
# train.eBird = 1:111728
# train.BBA = 111729:138731
# train.BBS = 138732:171408
# #test: eBird- 171409:180625, BBA- 180626:187355, BBS- 187356:195628
# test.eBird = 171409:180625
# test.BBA = 180626:187355
# test.BBS = 187356:195628
# 
# m3 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) + 
#               s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + #s(forest, k=10) + 
#               s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
#             data = dat3, family='binomial', file = 'placeholder.txt')
# 
# datm3 <- list(y = c(dat3$cawadet[train.eBird], dat3$cawatot[train.BBA], dat3$cawadet[train.BBS]), 
#               X = m3$jags.data$X, n = m3$jags.data$n, zero = m3$jags.data$zero,
#               S1 = m3$jags.data$S1, S2 = m3$jags.data$S2, S3 = m3$jags.data$S3,
#               S4 = m3$jags.data$S4, S5 = m3$jags.data$S5, S6 = m3$jags.data$S6,
#               S7 = m3$jags.data$S7, S8 = m3$jags.data$S8, S9 = m3$jags.data$S9,
#               ehours = dat3$duration_minutes[train.eBird], 
#               ekm = dat3$effort_distance_km[train.eBird], 
#               ehours2 = dat3$duration_minutes[test.eBird], 
#               ekm2 = dat3$effort_distance_km[test.eBird], 
#               hsm = dat3$hsm[train.BBA],
#               hsm2 = dat3$hsm[test.BBA], 
#               doy = dat3$doy[train.BBA],
#               doy2 = dat3$doy[test.BBA],
#               doy3 = dat3$doy[train.BBS],
#               doy4 = dat3$doy[test.BBS])
# 
# initsm3 <- function(){
#   list(b = m3$jags.ini$b, 
#        lambda = m3$jags.ini$lambda,  
#        beta = rep(3, 0.001), 
#        z = as.numeric(datm3$y>0))
# }
# 
# outm3 <- jags(data = datm3, 
#               parameters.to.save = c("beta","b","y2"), 
#               inits = initsm3, 
#               model.file = "models/cawam3.txt", 
#               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)
# 
# 
# ### Performance metrics -----------------------------------------------------
# 
# #Full Deviance
# m3_yp <- outm3$mean$y2
# m3_yt <- c(dat3$cawadet[test.eBird], dat3$cawatot[test.BBA], dat3$cawadet[test.BBS])
# m3_yt <- cbind(m3_yt, c(1 - dat3$cawadet[test.eBird], 5 - dat3$cawatot[test.BBA], 1 - dat3$cawadet[test.BBS]))
# m3_yp[9218:15947] <- m3_yp[9218:15947]/5
# m3_yp <- 0.0001 + m3_yp*0.9998
# m3_dev <- -2*sum(log((m3_yp^m3_yt[,1])*((1-m3_yp)^(m3_yt[,2]))))
# #eBird deviance
# Em3_yp <- outm3$mean$y2[1:9217]
# Em3_yt <- c(dat3$cawadet[test.eBird])
# Em3_yt <- cbind(Em3_yt, c(1 - dat3$cawadet[test.eBird]))
# Em3_yp <- 0.0001 + Em3_yp*0.9998
# Em3_dev <- -2*sum(log((Em3_yp^Em3_yt[,1])*((1-Em3_yp)^(Em3_yt[,2]))))
# #BBA deviance
# Am3_yp <- outm3$mean$y2[9218:15947]
# Am3_yt <- c(dat3$cawatot[test.BBA])
# Am3_yt <- cbind(Am3_yt, c(5 - dat3$cawatot[test.BBA])) 
# Am3_yp <- Am3_yp/5
# Am3_yp <- 0.0001 + Am3_yp*0.9998
# Am3_dev <- -2*sum(log((Am3_yp^Am3_yt[,1])*((1-Am3_yp)^(Am3_yt[,2]))))
# #BBS deviance
# Sm3_yp <- outm3$mean$y2[15948:24220]
# Sm3_yt <- c(dat3$cawadet[test.BBS])
# Sm3_yt <- cbind(Sm3_yt, c(1 - dat3$cawadet[test.BBS]))
# Sm3_yp <- 0.0001 + Sm3_yp*0.9998
# Sm3_dev <- -2*sum(log((Sm3_yp^Sm3_yt[,1])*((1-Sm3_yp)^(Sm3_yt[,2]))))
# 
# 
# # Brier score
# brier3 <- mean((outm3$mean$y2[9218:24220] - dat3$cawadet[180626:195628])^2)
# 
# # AUC
# pred3 <- prediction(as.numeric(outm3$mean$y2[9218:24220]), dat3$cawadet[180626:195628])
# auc3 <- performance(pred3, measure = "auc")
# auc3 <- auc3@y.values[[1]]
# 
# #roc3 <- roc(dat3$cawadet[180668:195774], as.numeric(outm3$mean$y2[9218:24220]), ci.auc=TRUE, auc=TRUE, parallel=TRUE)
# 
# save(outm3, m3, datm3, m3_dev, Em3_dev, Am3_dev, Sm3_dev,
#      brier3, pred3, auc3,
#      file = "results/out/cawam3.RData")
# 
# 
# ### GAMs --------------------------------------------------------------------
# 
# #Sim2Jam
# require(rjags)
# # load("results/out/cawam3.RData")
# 
# jm3 <- jags.model("models/cawam3.txt", 
#                   data=datm3, 
#                   inits=initsm3, 
#                   n.adapt=500, 
#                   n.chains=3)
# update(jm3, 500)
# sam3 <- jags.samples(jm3, c("b","rho"), n.iter=2000, thin=2)
# jam3 <- sim3jam(sam3, m3$pregam)
# 
# save(m3, jm3, sam3, jam3,
#      file = "results/jams/cawam3jam.RData")
# 
# 
# # Clear workspace for next run
# clear.workspace()
# 
# 
# 
# 
# # Model 4: F3 - CAWA (no eBird filtering or spatial balancing) ------------
# #train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
# train.eBird = 1:117936
# train.BBA = 117937:144939
# train.BBS = 144940:177616
# #test: eBird- 177617:186833, BBA- 186834:193563, BBS-193564:201836
# test.eBird = 177617:186833
# test.BBA = 186834:193563
# test.BBS = 193564:201836
# 
# m4 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) + 
#               s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + #s(forest, k=10) + 
#               s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
#             data = dat4, family='binomial', file = 'placeholder.txt')
# 
# datm4 <- list(y = c(dat4$cawadet[1:117936], dat4$cawatot[117937:144939], 
#                     dat4$cawadet[144940:177616]), 
#               X = m4$jags.data$X, n = m4$jags.data$n, zero = m4$jags.data$zero,
#               S1 = m4$jags.data$S1, S2 = m4$jags.data$S2, S3 = m4$jags.data$S3,
#               S4 = m4$jags.data$S4, S5 = m4$jags.data$S5, S6 = m4$jags.data$S6,
#               S7 = m4$jags.data$S7, S8 = m4$jags.data$S8, S9 = m4$jags.data$S9,
#               ehours = dat4$duration_minutes[1:117936], 
#               ekm = dat4$effort_distance_km[1:117936], 
#               ehours2 = dat4$duration_minutes[177617:186833], 
#               ekm2 = dat4$effort_distance_km[177617:186833], 
#               hsm = dat4$hsm[117937:144939],
#               hsm2 = dat4$hsm[186834:193563], 
#               doy = dat4$doy[117937:144939],
#               doy2 = dat4$doy[186834:193563],
#               doy3 = dat4$doy[144940:177616],
#               doy4 = dat4$doy[193564:201836])
# 
# initsm4 <- function(){
#   list(b = m4$jags.ini$b, 
#        lambda = m4$jags.ini$lambda,  
#        beta = rep(3, 0.001), 
#        z = as.numeric(datm4$y>0))
# }
# 
# outm4 <- jags(data = datm4, 
#               parameters.to.save = c("beta","b","y2"), 
#               inits = initsm4, 
#               model.file = "FinalModels/cawam4.txt", 
#               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)
# 
# #Full Deviance
# m4_yp <- outm4$mean$y2
# m4_yt <- c(dat4$cawadet[177617:186833], dat4$cawatot[186834:193563], dat4$cawadet[193564:201836])
# m4_yt <- cbind(m4_yt, c(1 - dat4$cawadet[177617:186833], 5 - dat4$cawatot[186834:193563], 1 - dat4$cawadet[193564:201836]))
# m4_yp[9218:15947] <- m4_yp[9218:15947]/5
# m4_yp <- 0.0001 + m4_yp*0.9998
# m4_dev <- -2*sum(log((m4_yp^m4_yt[,1])*((1-m4_yp)^(m4_yt[,2]))))
# #eBird deviance
# Em4_yp <- outm4$mean$y2[1:9217]
# Em4_yt <- c(dat4$cawadet[177617:186833])
# Em4_yt <- cbind(Em4_yt, c(1 - dat4$cawadet[177617:186833]))
# Em4_yp <- 0.0001 + Em4_yp*0.9998
# Em4_dev <- -2*sum(log((Em4_yp^Em4_yt[,1])*((1-Em4_yp)^(Em4_yt[,2]))))
# #BBA deviance
# Am4_yp <- outm4$mean$y2[9218:15947]
# Am4_yt <- c(dat4$cawatot[186834:193563])
# Am4_yt <- cbind(Am4_yt, c(5 - dat4$cawatot[186834:193563])) 
# Am4_yp <- Am4_yp/5
# Am4_yp <- 0.0001 + Am4_yp*0.9998
# Am4_dev <- -2*sum(log((Am4_yp^Am4_yt[,1])*((1-Am4_yp)^(Am4_yt[,2]))))
# #BBS deviance
# Sm4_yp <- outm4$mean$y2[15948:24220]
# Sm4_yt <- c(dat4$cawadet[193564:201836])
# Sm4_yt <- cbind(Sm4_yt, c(1 - dat4$cawadet[193564:201836]))
# Sm4_yp <- 0.0001 + Sm4_yp*0.9998
# Sm4_dev <- -2*sum(log((Sm4_yp^Sm4_yt[,1])*((1-Sm4_yp)^(Sm4_yt[,2]))))
# 
# save(outm4, m4, m4_dev, Em4_dev, Am4_dev, Sm4_dev, 
#      file = "FinalResults/cawam4.RData")
# 
# brier4 <- mean((outm4$mean$y2[9218:24220] - dat4$cawadet[186834:201836])^2)
# 
# pred4 <- prediction(as.numeric(outm4$mean$y2[9218:24220]), dat4$cawadet[186834:201836])
# auc4 <- performance(pred4, measure = "auc")
# auc4 <- auc4@y.values[[1]]
# 
# 
# 
# ##Model 5: CAWA (no eBird effort)
# #train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
# #test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729
# 
# m5 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
#               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
#             data = dat1, family='binomial', file = 'placeholder.txt')
# 
# datm5 <- list(y = c(dat1$cawadet[1:19829], dat1$cawatot[19830:46832], dat1$cawadet[46833:79509]), 
#               X = m5$jags.data$X, n = m5$jags.data$n, zero = m5$jags.data$zero,
#               S1 = m5$jags.data$S1, S2 = m5$jags.data$S2, S3 = m5$jags.data$S3,
#               S4 = m5$jags.data$S4, S5 = m5$jags.data$S5, S6 = m5$jags.data$S6,
#               S7 = m5$jags.data$S7, S8 = m5$jags.data$S8, S9 = m5$jags.data$S9,
#               hsm = dat1$hsm[19830:46832],
#               hsm2 = dat1$hsm[88727:95456], 
#               doy = dat1$doy[19830:46832],
#               doy2 = dat1$doy[88727:95456],
#               doy3 = dat1$doy[46833:79509],
#               doy4 = dat1$doy[95457:103729])
# 
# initsm5 <- function(){
#   list(b = m5$jags.ini$b, lambda = m5$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm5$y>0))
# }
# 
# outm5 <- jags(data = datm5, parameters.to.save = c("beta","b","y2"), inits = initsm5, 
#               model.file = "FinalModels/cawam5.txt", 
#               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)
# 
# #Full Deviance
# m5_yp <- outm5$mean$y2
# m5_yt <- c(dat1$cawadet[79510:88726], dat1$cawatot[88727:95456], dat1$cawadet[95457:103729])
# m5_yt <- cbind(m5_yt, c(1 - dat1$cawadet[79510:88726], 5 - dat1$cawatot[88727:95456], 1 - dat1$cawadet[95457:103729]))
# m5_yp[9218:15947] <- m5_yp[9287:16204]/5
# m5_yp <- 0.0001 + m5_yp*0.9998
# m5_dev <- -2*sum(log((m5_yp^m5_yt[,1])*((1-m5_yp)^(m5_yt[,2]))))
# #eBird deviance
# Em5_yp <- outm5$mean$y2[1:9217]
# Em5_yt <- c(dat1$cawadet[79510:88726])
# Em5_yt <- cbind(Em5_yt, c(1 - dat1$cawadet[79510:88726]))
# Em5_yp <- 0.0001 + Em5_yp*0.9998
# Em5_dev <- -2*sum(log((Em5_yp^Em5_yt[,1])*((1-Em5_yp)^(Em5_yt[,2]))))
# #BBA deviance
# Am5_yp <- outm5$mean$y2[9218:15947]
# Am5_yt <- c(dat1$cawatot[88727:95456])
# Am5_yt <- cbind(Am5_yt, c(5 - dat1$cawatot[88727:95456]))
# Am5_yp <- Am5_yp/5
# Am5_yp <- 0.0001 + Am5_yp*0.9998
# Am5_dev <- -2*sum(log((Am5_yp^Am5_yt[,1])*((1-Am5_yp)^(Am5_yt[,2]))))
# #BBS deviance
# Sm5_yp <- outm5$mean$y2[15948:24220]
# Sm5_yt <- c(dat1$cawadet[95457:103729])
# Sm5_yt <- cbind(Sm5_yt, c(1 - dat1$cawadet[95457:103729]))
# Sm5_yp <- 0.0001 + Sm5_yp*0.9998
# Sm5_dev <- -2*sum(log((Sm5_yp^Sm5_yt[,1])*((1-Sm5_yp)^(Sm5_yt[,2]))))
# 
# save(outm5, m5, m5_dev, Em5_dev, Am5_dev, Sm5_dev, 
#      file = "FinalResults/cawam5.RData")
# 
# brier5 <- mean((outm5$mean$y2[9218:24220] - dat1$cawadet[88727:103729])^2)
# 
# pred5 <- prediction(as.numeric(outm5$mean$y2[9218:24220]), dat1$cawadet[88727:103729])
# auc5 <- performance(pred5, measure = "auc")
# auc5 <- auc5@y.values[[1]]
# 
# 
# 
# ##Model 6: CAWA (no filtering, no spatial balancing, & no effort for eBird)
# #train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
# #test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836
# 
# m6 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
#               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
#             data = dat4, family='binomial', file = 'placeholder.txt')
# 
# datm6 <- list(y = c(dat4$cawadet[1:117936], dat4$cawatot[117937:144939], dat4$cawadet[144940:177616]), 
#               X = m6$jags.data$X, n = m6$jags.data$n, zero = m6$jags.data$zero,
#               S1 = m6$jags.data$S1, S2 = m6$jags.data$S2, S3 = m6$jags.data$S3,
#               S4 = m6$jags.data$S4, S5 = m6$jags.data$S5, S6 = m6$jags.data$S6,
#               S7 = m6$jags.data$S7, S8 = m6$jags.data$S8, S9 = m6$jags.data$S9,
#               hsm = dat4$hsm[117937:144939],
#               hsm2 = dat4$hsm[186834:193563], 
#               doy = dat4$doy[117937:144939],
#               doy2 = dat4$doy[186834:193563],
#               doy3 = dat4$doy[144940:177616],
#               doy4 = dat4$doy[193564:201836])
# 
# initsm6 <- function(){
#   list(b = m6$jags.ini$b, lambda = m6$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm6$y>0))
# }
# 
# outm6 <- jags(data = datm6, parameters.to.save = c("beta","b","y2"), inits = initsm6, 
#               model.file = "FinalModels/cawam6.txt", 
#               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)
# 
# #Full Deviance
# m6_yp <- outm6$mean$y2
# m6_yt <- c(dat4$cawadet[177617:186833], dat4$cawatot[186834:193563], dat4$cawadet[193564:201836])
# m6_yt <- cbind(m6_yt, c(1 - dat4$cawadet[177617:186833], 5 - dat4$cawatot[186834:193563], 1 - dat4$cawadet[193564:201836]))
# m6_yp[9218:15947] <- m6_yp[9218:15947]/5
# m6_yp <- 0.0001 + m6_yp*0.9998
# m6_dev <- -2*sum(log((m6_yp^m6_yt[,1])*((1-m6_yp)^(m6_yt[,2]))))
# #eBird deviance
# Em6_yp <- outm6$mean$y2[1:9217]
# Em6_yt <- c(dat4$cawadet[177617:186833])
# Em6_yt <- cbind(Em6_yt, c(1 - dat4$cawadet[177617:186833]))
# Em6_yp <- 0.0001 + Em6_yp*0.9998
# Em6_dev <- -2*sum(log((Em6_yp^Em6_yt[,1])*((1-Em6_yp)^(Em6_yt[,2]))))
# #BBA deviance
# Am6_yp <- outm6$mean$y2[9218:15947]
# Am6_yt <- c(dat4$cawatot[186834:193563])
# Am6_yt <- cbind(Am6_yt, c(5 - dat4$cawatot[186834:193563])) 
# Am6_yp <- Am6_yp/5
# Am6_yp <- 0.0001 + Am6_yp*0.9998
# Am6_dev <- -2*sum(log((Am6_yp^Am6_yt[,1])*((1-Am6_yp)^(Am6_yt[,2]))))
# #BBS deviance
# Sm6_yp <- outm6$mean$y2[15948:24220]
# Sm6_yt <- c(dat4$cawadet[193564:201836])
# Sm6_yt <- cbind(Sm6_yt, c(1 - dat4$cawadet[193564:201836]))
# Sm6_yp <- 0.0001 + Sm6_yp*0.9998
# Sm6_dev <- -2*sum(log((Sm6_yp^Sm6_yt[,1])*((1-Sm6_yp)^(Sm6_yt[,2]))))
# 
# save(outm6, m6, m6_dev, Em6_dev, Am6_dev, Sm6_dev, 
#      file = "FinalResults/cawam6.RData")
# 
# brier6 <- mean((outm6$mean$y2[9218:24220] - dat4$cawadet[186834:201836])^2)
# 
# pred6 <- prediction(as.numeric(outm6$mean$y2[9218:24220]), dat4$cawadet[186834:201836])
# auc6 <- performance(pred6, measure = "auc")
# auc6 <- auc6@y.values[[1]]
# 
# 
# #Model 7: CAWA (full model + false pos for eBird)
# #train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
# #test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729
# 
# m7 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
#               s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
#             data = dat1, family='binomial', file = 'placeholder.txt')
# 
# datm7 <- list(y = c(dat1$cawadet[1:19829], dat1$cawatot[19830:46832], dat1$cawadet[46833:79509]), 
#               X = m7$jags.data$X, n = m7$jags.data$n, zero = m7$jags.data$zero,
#               S1 = m7$jags.data$S1, S2 = m7$jags.data$S2, S3 = m7$jags.data$S3,
#               S4 = m7$jags.data$S4, S5 = m7$jags.data$S5, S6 = m7$jags.data$S6,
#               S7 = m7$jags.data$S7, S8 = m7$jags.data$S8, S9 = m7$jags.data$S9,
#               ehours = dat1$duration_minutes[1:19829], 
#               ekm = dat1$effort_distance_km[1:19829], 
#               ehours2 = dat1$duration_minutes[79510:88726], 
#               ekm2 = dat1$effort_distance_km[79510:88726], 
#               hsm = dat1$hsm[19830:46832],
#               hsm2 = dat1$hsm[88727:95456], 
#               doy = dat1$doy[19830:46832],
#               doy2 = dat1$doy[88727:95456],
#               doy3 = dat1$doy[46833:79509],
#               doy4 = dat1$doy[95457:103729])
# 
# initsm7 <- function(){
#   list(b = m7$jags.ini$b, lambda = m7$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm7$y>0), f=0.05)
# }
# 
# outm7 <- jags(data = datm7, parameters.to.save = c("beta","b","y2", "f"), inits = initsm7, 
#               model.file = "FinalModels/cawam7.txt", 
#               n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)
# 
# #Full Deviance
# m7_yp <- outm7$mean$y2
# m7_yt <- c(dat1$cawadet[79510:88726], dat1$cawatot[88727:95456], dat1$cawadet[95457:103729])
# m7_yt <- cbind(m7_yt, c(1 - dat1$cawadet[79510:88726], 5 - dat1$cawatot[88727:95456], 1 - dat1$cawadet[95457:103729]))
# m7_yp[9218:15947] <- m7_yp[9287:16204]/5
# m7_yp <- 0.0001 + m7_yp*0.9998
# m7_dev <- -2*sum(log((m7_yp^m7_yt[,1])*((1-m7_yp)^(m7_yt[,2]))))
# #eBird deviance
# Em7_yp <- outm7$mean$y2[1:9217]
# Em7_yt <- c(dat1$cawadet[79510:88726])
# Em7_yt <- cbind(Em7_yt, c(1 - dat1$cawadet[79510:88726]))
# Em7_yp <- 0.0001 + Em7_yp*0.9998
# Em7_dev <- -2*sum(log((Em7_yp^Em7_yt[,1])*((1-Em7_yp)^(Em7_yt[,2]))))
# #BBA deviance
# Am7_yp <- outm7$mean$y2[9218:15947]
# Am7_yt <- c(dat1$cawatot[88727:95456])
# Am7_yt <- cbind(Am7_yt, c(5 - dat1$cawatot[88727:95456]))
# Am7_yp <- Am7_yp/5
# Am7_yp <- 0.0001 + Am7_yp*0.9998
# Am7_dev <- -2*sum(log((Am7_yp^Am7_yt[,1])*((1-Am7_yp)^(Am7_yt[,2]))))
# #BBS deviance
# Sm7_yp <- outm7$mean$y2[15948:24220]
# Sm7_yt <- c(dat1$cawadet[95457:103729])
# Sm7_yt <- cbind(Sm7_yt, c(1 - dat1$cawadet[95457:103729]))
# Sm7_yp <- 0.0001 + Sm7_yp*0.9998
# Sm7_dev <- -2*sum(log((Sm7_yp^Sm7_yt[,1])*((1-Sm7_yp)^(Sm7_yt[,2]))))
# 
# 
# save(outm7, m7, m7_dev, Em7_dev, Am7_dev, Sm7_dev, 
#      file = "FinalResults/cawam7.RData")
# 
# brier7 <- mean((outm7$mean$y2[9218:24220] - dat1$cawadet[88727:103729])^2)
# 
# pred7 <- prediction(as.numeric(outm7$mean$y2[9218:24220]), dat1$cawadet[88727:103729])
# auc7 <- performance(pred7, measure = "auc")
# auc7 <- auc7@y.values[[1]]
# 
# 
# #############################################################################
# 
# 
# #Plots
# #AUC boxplots?
# boxplot(ci.auc(roc1), ci.auc(roc2), ci.auc(roc3), at=c(1,2,3), names=c("M1","M2","M3"), main="AUC")
# 
# 
# #Distribution
# library(ggplot2)
# library(sf)
# library(raster)
# library(exactextractr)
# library(RColorBrewer)
# 
# #Load rasters
# elev <- raster("S:/MillerLab/Projects/BirdSDM/GIS/elev_30mDEM/elev2016proj.tif")
# can <- raster("S:/MillerLab/Projects/BirdSDM/GIS/nlcd_2016_treecanopy_2019_08_31/can2016proj.tif")
# temp <- raster("S:/MillerLab/Projects/BirdSDM/GIS/Climate/temp2016proj.tif")
# ppt <- raster("S:/MillerLab/Projects/BirdSDM/GIS/Climate/ppt2016proj.tif")
# dev <-  raster("S:/MillerLab/Projects/BirdSDM/GIS/NLCD_2016_Land_Cover_L48_20190424/devlan2016proj.tif")
# forest <- raster("S:/MillerLab/Projects/BirdSDM/GIS/NLCD_2016_Land_Cover_L48_20190424/allfor2016proj.tif")
# slope <- raster("S:/MillerLab/Projects/BirdSDM/GIS/slope2016proj.tif")
# road <- st_read("S:/MillerLab/Projects/BirdSDM/GIS/Roads/PAroads2.shp")
# road <- st_transform(road, crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# road <- road[c(2:3,6)]
# road <- tibble::rowid_to_column(road, "ID")
# 
# #Create grid
# pa <- st_read("S:/MillerLab/Projects/BirdSDM/GIS/Pennsylvania_State_Boundary/PaState2020_01.shp")
# pa <- st_transform(pa, crs = "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# grid_pa <- pa %>%
#   st_make_grid(cellsize=1000) %>%
#   st_intersection(pa) %>%
#   st_as_sf() %>%
#   mutate(id = 1:nrow(.))
# grid2 <- grid_pa
# grid3 <- grid_pa
# 
# #Get centroids for extraction; concert grid to spatial to get point layer
# grid_cen <- st_coordinates(st_centroid(grid_pa))
# grid_cen2 <- as.data.frame(grid_cen)
# grid_pt <- st_as_sf(grid_cen2, coords=c("X","Y"))
# grid_pt <- st_set_crs(grid_pt, "+proj=utm +zone=18 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# 
# #Create grid internal buffers
# gridbuff <- grid_pt %>% 
#   st_buffer(dist=400) %>% 
#   st_as_sf() %>%
#   mutate(id = 1:nrow(.))
# 
# #Road
# intersection <- st_intersection(gridbuff, road) %>% 
#   mutate(length = st_length(.)) %>% 
#   st_drop_geometry()
# int_length <- intersection %>%
#   dplyr::group_by(id) %>%
#   dplyr::summarise(length = sum(length))
# grid2 <- left_join(gridbuff, int_length, by = c("id"))
# grid2$road <- log((as.numeric(grid2$length) * 0.3048) + 1)
# grid3$road <- grid2$road
# 
# #Raster extraction at point level
# grid3$elev <- raster::extract(elev, grid_cen)
# grid3$elev <- scale(grid3$elev, center=352.4586, scale=160.7967)
# grid3$temp <- raster::extract(temp, grid_cen)
# grid3$temp <- scale(grid3$temp, center=9.408352, scale=1.415427)
# grid3$ppt <- raster::extract(ppt, grid_cen)
# grid3$ppt <- scale(grid3$ppt, center=1102.585, scale=82.26033)
# grid3$slope <- raster::extract(slope, grid_cen)
# grid3$slope <- scale(grid3$slope, center=5.130878, scale=4.347548)
# 
# #Raster extraction at buffer level
# grid3$can <- exactextractr::exact_extract(can, gridbuff, fun = 'mean')
# grid3$can <- scale(grid3$can, center=43.68183, scale=25.17216)
# grid3$dev <- exactextractr::exact_extract(dev, gridbuff, fun = 'count')
# grid3$dev <- (grid3$dev * 900)/502654.825
# grid3$dev <- scale(grid3$dev, center=0.2299, scale=0.2136447)
# grid3$forest <- exactextractr::exact_extract(forest, gridbuff, fun = 'count')
# grid3$forest <- (grid3$forest * 900)/502654.825
# grid3$forest <- scale(grid3$forest, center=0.6011573, scale=0.3303459)
# 
# #Get lat long
# latlong <- st_coordinates(st_transform(st_centroid(grid_pa), 4326))
# grid3$latitude <- latlong[,2]
# grid3$longitude <- latlong[,1]
# 
# #Convert NAs to 0s
# grid3[,3:12][is.na(grid3[,3:12])] <- 0
# 
# 
# #Save out grids, 1k
# save(grid_pa, grid2, grid3, grid_cen, grid_pt, gridbuff, intersection, int_length,  
#      file = "FinalResults/Plots/grid_pa_1km.RData")
# 
# 
# ##########
# 
# ###Generate predictive map (load jam first)
# grid3$pred <- predict(jam1, newdata = grid3, type="response")
# grid_sp <- as_Spatial(grid3)
# 
# #Change map extent
# scale.parameter = 1.1
# original.bbox = grid_sp@bbox 
# edges = original.bbox
# edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1, 
# ])
# edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2, 
# ])
# 
# #Plot
# pdf("FinalResults/Plots/cawam1map.pdf")
# spplot(grid_sp, "pred", col=NA, par.settings = list(axis.line = list(col = 'transparent')),
#        xlim = edges[1, ], ylim = edges[2, ], colorkey=list(height=0.6, cex=0.5))
# dev.off()
# 
# 
# 
# 
# ##Adjust new grid for covariate model (old thesis stuff currently)
# 
# grid4 <- grid3
# load("S:/MillerLab/Projects/BirdSDM/Scripts/datasetup2_3.RData")
# 
# gridbuff2 <- grid_pt %>% 
#   st_buffer(dist=1000) %>% 
#   st_as_sf() %>%
#   mutate(id = 1:nrow(.))
# 
# lists_ebd <- sf:::aggregate.sf(ebdpoint2["checklist_id"], by=gridbuff2$geometry, FUN = length)
# lists_ebd <- st_drop_geometry(lists_ebd)
# grid4$lists_ebd <- lists_ebd$checklist_id
# 
# lists_bba <- sf:::aggregate.sf(bbapoint["BBA_ID"], by=gridbuff2$geometry, FUN = length)
# lists_bba <- st_drop_geometry(lists_bba)
# grid4$lists_bba <- lists_bba$BBA_ID
# 
# lists_bbs <- sf:::aggregate.sf(bbspoint["X"], by=gridbuff2$geometry, FUN = length)
# lists_bbs <- st_drop_geometry(lists_bbs)
# grid4$lists_bbs <- lists_bbs$X
# 
# gridcawa <- grid4
# cawact_ebd <- sf:::aggregate.sf(ebdpoint2["cawacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# cawact_ebd <- st_drop_geometry(cawact_ebd)
# gridcawa$cawact_ebd <- cawact_ebd$cawacount
# cawact_bba <- sf:::aggregate.sf(bbapoint["cawacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# cawact_bba <- st_drop_geometry(cawact_bba)
# gridcawa$cawact_bba <- cawact_bba$cawacount
# cawact_bbs <- sf:::aggregate.sf(bbspoint["cawacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# cawact_bbs <- st_drop_geometry(cawact_bbs)
# gridcawa$cawact_bbs <- cawact_bbs$cawacount
# 
# gridcerw <- grid4
# cerwct_ebd <- sf:::aggregate.sf(ebdpoint2["cerwcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# cerwct_ebd <- st_drop_geometry(cerwct_ebd)
# gridcerw$cerwct_ebd <- cerwct_ebd$cerwcount
# cerwct_bba <- sf:::aggregate.sf(bbapoint["cerwcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# cerwct_bba <- st_drop_geometry(cerwct_bba)
# gridcerw$cerwct_bba <- cerwct_bba$cerwcount
# cerwct_bbs <- sf:::aggregate.sf(bbspoint["cerwcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# cerwct_bbs <- st_drop_geometry(cerwct_bbs)
# gridcerw$cerwct_bbs <- cerwct_bbs$cerwcount
# 
# gridgwwa <- grid4
# gwwact_ebd <- sf:::aggregate.sf(ebdpoint2["gwwacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# gwwact_ebd <- st_drop_geometry(gwwact_ebd)
# gridgwwa$gwwact_ebd <- gwwact_ebd$gwwacount
# gwwact_bba <- sf:::aggregate.sf(bbapoint["gwwacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# gwwact_bba <- st_drop_geometry(gwwact_bba)
# gridgwwa$gwwact_bba <- gwwact_bba$gwwacount
# gwwact_bbs <- sf:::aggregate.sf(bbspoint["gwwacount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# gwwact_bbs <- st_drop_geometry(gwwact_bbs)
# gridgwwa$gwwact_bbs <- gwwact_bbs$gwwacount
# 
# gridwoth <- grid4
# wothct_ebd <- sf:::aggregate.sf(ebdpoint2["wothcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# wothct_ebd <- st_drop_geometry(wothct_ebd)
# gridwoth$wothct_ebd <- wothct_ebd$wothcount
# wothct_bba <- sf:::aggregate.sf(bbapoint["wothcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# wothct_bba <- st_drop_geometry(wothct_bba)
# gridwoth$wothct_bba <- wothct_bba$wothcount
# wothct_bbs <- sf:::aggregate.sf(bbspoint["wothcount"], by=gridbuff2$geometry, FUN = sum, na.rm=TRUE)
# wothct_bbs <- st_drop_geometry(wothct_bbs)
# gridwoth$wothct_bbs <- wothct_bbs$wothcount
# 
# 
# gridcawa[,13:18][is.na(gridcawa[,13:18])] <- 0
# gridcerw[,13:18][is.na(gridcerw[,13:18])] <- 0
# gridgwwa[,13:18][is.na(gridgwwa[,13:18])] <- 0
# gridwoth[,13:18][is.na(gridwoth[,13:18])] <- 0
# 
# 
# save(gridbuff2, grid4, gridcawa, gridcerw, gridgwwa, gridwoth, 
#      file = "FinalResults/Plots/grid_pa_1km_v2.RData")
# 
