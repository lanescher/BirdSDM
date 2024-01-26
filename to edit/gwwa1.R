#Golden-winged Warbler models 1-7
setwd("S:/MillerLab/Projects/BirdSDM")

library(dplyr)
library(tibble)
library(plyr)
library(mgcv)
library(jagsUI)
library(ROCR)

#Load data
test1 <- read.csv("FinalData/TestTrain/test1.csv")
train1 <- read.csv("FinalData/TestTrain/train1.csv")
train2 <- read.csv("FinalData/TestTrain/train2.csv")
train3 <- read.csv("FinalData/TestTrain/train3.csv")
train4 <- read.csv("FinalData/TestTrain/train4.csv")

dat1 <- rbind.fill(train1, test1)
dat2 <- rbind.fill(train2, test1)
dat3 <- rbind.fill(train3, test1)
dat4 <- rbind.fill(train4, test1)

#Model 1: gwwa (full model)
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509,
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729

m1 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
            data = dat1, family='binomial', file = 'placeholder.txt')

datm1 <- list(y = c(dat1$gwwadet[1:19829], dat1$gwwatot[19830:46832], dat1$gwwadet[46833:79509]), 
              X = m1$jags.data$X, n = m1$jags.data$n, zero = m1$jags.data$zero,
              S1 = m1$jags.data$S1, S2 = m1$jags.data$S2, S3 = m1$jags.data$S3,
              S4 = m1$jags.data$S4, S5 = m1$jags.data$S5, S6 = m1$jags.data$S6,
              S7 = m1$jags.data$S7, S8 = m1$jags.data$S8, S9 = m1$jags.data$S9, 
              ehours = dat1$duration_minutes[1:19829], 
              ekm = dat1$effort_distance_km[1:19829],
              ehours2 = dat1$duration_minutes[79510:88726], 
              ekm2 = dat1$effort_distance_km[79510:88726], 
              hsm = dat1$hsm[19830:46832],
              hsm2 = dat1$hsm[88727:95456], 
              doy = dat1$doy[19830:46832],
              doy2 = dat1$doy[88727:95456],
              doy3 = dat1$doy[46833:79509],
              doy4 = dat1$doy[95457:103729])

initsm1 <- function(){
  list(b = m1$jags.ini$b, lambda = m1$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm1$y>0))
}

outm1 <- jags(data = datm1, parameters.to.save = c("beta","b","y2"), inits = initsm1, 
              model.file = "FinalModels/gwwam1.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500, parallel=TRUE)

#Full Deviance
m1_yp <- outm1$mean$y2
m1_yt <- c(dat1$gwwadet[79510:88726], dat1$gwwatot[88727:95456], dat1$gwwadet[95457:103729])
m1_yt <- cbind(m1_yt, c(1 - dat1$gwwadet[79510:88726], 5 - dat1$gwwatot[88727:95456], 1 - dat1$gwwadet[95457:103729]))
m1_yp[9218:15947] <- m1_yp[9218:15947]/5
m1_yp <- 0.0001 + m1_yp*0.9998
m1_dev <- -2*sum(log((m1_yp^m1_yt[,1])*((1-m1_yp)^(m1_yt[,2]))))
#eBird deviance
Em1_yp <- outm1$mean$y2[1:9217]
Em1_yt <- c(dat1$gwwadet[79510:88726])
Em1_yt <- cbind(Em1_yt, c(1 - dat1$gwwadet[79510:88726]))
Em1_yp <- 0.0001 + Em1_yp*0.9998
Em1_dev <- -2*sum(log((Em1_yp^Em1_yt[,1])*((1-Em1_yp)^(Em1_yt[,2]))))
#BBA deviance
Am1_yp <- outm1$mean$y2[9218:15947]
Am1_yt <- c(dat1$gwwatot[88727:95456])
Am1_yt <- cbind(Am1_yt, c(5 - dat1$gwwatot[88727:95456]))
Am1_yp <- Am1_yp/5
Am1_yp <- 0.0001 + Am1_yp*0.9998
Am1_dev <- -2*sum(log((Am1_yp^Am1_yt[,1])*((1-Am1_yp)^(Am1_yt[,2]))))
#BBS deviance
Sm1_yp <- outm1$mean$y2[15948:24220]
Sm1_yt <- c(dat1$gwwadet[95457:103729])
Sm1_yt <- cbind(Sm1_yt, c(1 - dat1$gwwadet[95457:103729]))
Sm1_yp <- 0.0001 + Sm1_yp*0.9998
Sm1_dev <- -2*sum(log((Sm1_yp^Sm1_yt[,1])*((1-Sm1_yp)^(Sm1_yt[,2]))))

save(outm1, m1, m1_dev, Em1_dev, Am1_dev, Sm1_dev, 
     file = "FinalResults/gwwam1.RData")

brier1 <- mean((outm1$mean$y2[9218:24220] - dat1$gwwadet[88727:103729])^2)

pred1 <- prediction(as.numeric(outm1$mean$y2[9218:24220]), dat1$gwwadet[88727:103729])
auc1 <- performance(pred1, measure = "auc")
auc1 <- auc1@y.values[[1]]

#roc1 <- roc(dat1$gwwadet[88642:103748], as.numeric(outm1$mean$y2[9218:24220]), ci.auc=TRUE, auc=TRUE, parallel=TRUE)

#Sim2Jam
require(rjags)
jm1 <- jags.model("FinalModels/gwwam1.txt", data=datm1, inits=initsm1, n.adapt=500, n.chains=3)
update(jm1, 500)
sam1 <- jags.samples(jm1, c("b","rho"), n.iter=2000, thin=2)
jam1 <- sim2jam(sam1, m1$pregam)

save(m1, jm1, sam1, jam1,
     file = "FinalResults/Jams/gwwam1jam.RData")

pdf("FinalResults/Plots/gwwam1cov.pdf")
plot(jam1, pages=1)
dev.off()


##Model 2: gwwa (no eBird filtering)
#train: eBird- 1:20361, BBA- 20362:47364, BBS- 47365:80041
#test: eBird- 80042:89258, BBA- 89259:95988, BBS- 95989:104261

m2 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
            data = dat2, family='binomial', file = 'placeholder.txt')

datm2 <- list(y = c(dat2$gwwadet[1:20361], dat2$gwwatot[20362:47364], dat2$gwwadet[47365:80041]), 
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
  list(b = m2$jags.ini$b, lambda = m2$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm2$y>0))
}

outm2 <- jags(data = datm2, parameters.to.save = c("beta","b","y2"), inits = initsm2, 
              model.file = "FinalModels/gwwam2.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500, parallel=TRUE)

#Full Deviance
m2_yp <- outm2$mean$y2
m2_yt <- c(dat2$gwwadet[80042:89258], dat2$gwwatot[89259:95988], dat2$gwwadet[95989:104261])
m2_yt <- cbind(m2_yt, c(1 - dat2$gwwadet[80042:89258], 5 - dat2$gwwatot[89259:95988], 1 - dat2$gwwadet[95989:104261]))
m2_yp[9218:15947] <- m2_yp[9218:15947]/5
m2_yp <- 0.0001 + m2_yp*0.9998
m2_dev <- -2*sum(log((m2_yp^m2_yt[,1])*((1-m2_yp)^(m2_yt[,2]))))
#eBird deviance
Em2_yp <- outm2$mean$y2[1:9217]
Em2_yt <- c(dat2$gwwadet[80042:89258])
Em2_yt <- cbind(Em2_yt, c(1 - dat2$gwwadet[80042:89258]))
Em2_yp <- 0.0001 + Em2_yp*0.9998
Em2_dev <- -2*sum(log((Em2_yp^Em2_yt[,1])*((1-Em2_yp)^(Em2_yt[,2]))))
#BBA deviance
Am2_yp <- outm2$mean$y2[9218:15947]
Am2_yt <- c(dat2$gwwatot[89259:95988])
Am2_yt <- cbind(Am2_yt, c(5 - dat2$gwwatot[89259:95988]))
Am2_yp <- Am2_yp/5
Am2_yp <- 0.0001 + Am2_yp*0.9998
Am2_dev <- -2*sum(log((Am2_yp^Am2_yt[,1])*((1-Am2_yp)^(Am2_yt[,2]))))
#BBS deviance
Sm2_yp <- outm2$mean$y2[15948:24220]
Sm2_yt <- c(dat2$gwwadet[95989:104261])
Sm2_yt <- cbind(Sm2_yt, c(1 - dat2$gwwadet[95989:104261]))
Sm2_yp <- 0.0001 + Sm2_yp*0.9998
Sm2_dev <- -2*sum(log((Sm2_yp^Sm2_yt[,1])*((1-Sm2_yp)^(Sm2_yt[,2]))))

save(outm2, m2, m2_dev, Em2_dev, Am2_dev, Sm2_dev, 
     file = "FinalResults/gwwam2.RData")

brier2 <- mean((outm2$mean$y2[9218:24220] - dat2$gwwadet[89259:104261])^2)

pred2 <- prediction(as.numeric(outm2$mean$y2[9218:24220]), dat2$gwwadet[89259:104261])
auc2 <- performance(pred2, measure = "auc")
auc2 <- auc2@y.values[[1]]



##Model 3: gwwa (no eBird spatial balancing)
#train: eBird- 1:111728, BBA- 111729:138731, BBS- 138732:171408
#test: eBird- 171409:180625, BBA- 180626:187355, 187356:195628

m3 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
            data = dat3, family='binomial', file = 'placeholder.txt')

datm3 <- list(y = c(dat3$gwwadet[1:111728], dat3$gwwatot[111729:138731], dat3$gwwadet[138732:171408]), 
              X = m3$jags.data$X, n = m3$jags.data$n, zero = m3$jags.data$zero,
              S1 = m3$jags.data$S1, S2 = m3$jags.data$S2, S3 = m3$jags.data$S3,
              S4 = m3$jags.data$S4, S5 = m3$jags.data$S5, S6 = m3$jags.data$S6,
              S7 = m3$jags.data$S7, S8 = m3$jags.data$S8, S9 = m3$jags.data$S9,
              ehours = dat3$duration_minutes[1:111728], 
              ekm = dat3$effort_distance_km[1:111728], 
              ehours2 = dat3$duration_minutes[171409:180625], 
              ekm2 = dat3$effort_distance_km[171409:180625], 
              hsm = dat3$hsm[111729:138731],
              hsm2 = dat3$hsm[180626:187355], 
              doy = dat3$doy[111729:138731],
              doy2 = dat3$doy[180626:187355],
              doy3 = dat3$doy[138732:171408],
              doy4 = dat3$doy[187356:195628])

initsm3 <- function(){
  list(b = m3$jags.ini$b, lambda = m3$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm3$y>0))
}

outm3 <- jags(data = datm3, parameters.to.save = c("beta","b","y2"), inits = initsm3, 
              model.file = "FinalModels/gwwam3.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m3_yp <- outm3$mean$y2
m3_yt <- c(dat3$gwwadet[171409:180625], dat3$gwwatot[180626:187355], dat3$gwwadet[187356:195628])
m3_yt <- cbind(m3_yt, c(1 - dat3$gwwadet[171409:180625], 5 - dat3$gwwatot[180626:187355], 1 - dat3$gwwadet[187356:195628]))
m3_yp[9218:15947] <- m3_yp[9218:15947]/5
m3_yp <- 0.0001 + m3_yp*0.9998
m3_dev <- -2*sum(log((m3_yp^m3_yt[,1])*((1-m3_yp)^(m3_yt[,2]))))
#eBird deviance
Em3_yp <- outm3$mean$y2[1:9217]
Em3_yt <- c(dat3$gwwadet[171409:180625])
Em3_yt <- cbind(Em3_yt, c(1 - dat3$gwwadet[171409:180625]))
Em3_yp <- 0.0001 + Em3_yp*0.9998
Em3_dev <- -2*sum(log((Em3_yp^Em3_yt[,1])*((1-Em3_yp)^(Em3_yt[,2]))))
#BBA deviance
Am3_yp <- outm3$mean$y2[9218:15947]
Am3_yt <- c(dat3$gwwatot[180626:187355])
Am3_yt <- cbind(Am3_yt, c(5 - dat3$gwwatot[180626:187355])) 
Am3_yp <- Am3_yp/5
Am3_yp <- 0.0001 + Am3_yp*0.9998
Am3_dev <- -2*sum(log((Am3_yp^Am3_yt[,1])*((1-Am3_yp)^(Am3_yt[,2]))))
#BBS deviance
Sm3_yp <- outm3$mean$y2[15948:24220]
Sm3_yt <- c(dat3$gwwadet[187356:195628])
Sm3_yt <- cbind(Sm3_yt, c(1 - dat3$gwwadet[187356:195628]))
Sm3_yp <- 0.0001 + Sm3_yp*0.9998
Sm3_dev <- -2*sum(log((Sm3_yp^Sm3_yt[,1])*((1-Sm3_yp)^(Sm3_yt[,2]))))

save(outm3, m3, m3_dev, Em3_dev, Am3_dev, Sm3_dev, 
     file = "FinalResults/gwwam3.RData")

brier3 <- mean((outm3$mean$y2[9218:24220] - dat3$gwwadet[180626:195628])^2)

pred3 <- prediction(as.numeric(outm3$mean$y2[9218:24220]), dat3$gwwadet[180626:195628])
auc3 <- performance(pred3, measure = "auc")
auc3 <- auc3@y.values[[1]]



##Model 4: gwwa (no eBird filtering or spatial balancing)
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS-193564:201836

m4 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
            data = dat4, family='binomial', file = 'placeholder.txt')

datm4 <- list(y = c(dat4$gwwadet[1:117936], dat4$gwwatot[117937:144939], dat4$gwwadet[144940:177616]), 
              X = m4$jags.data$X, n = m4$jags.data$n, zero = m4$jags.data$zero,
              S1 = m4$jags.data$S1, S2 = m4$jags.data$S2, S3 = m4$jags.data$S3,
              S4 = m4$jags.data$S4, S5 = m4$jags.data$S5, S6 = m4$jags.data$S6,
              S7 = m4$jags.data$S7, S8 = m4$jags.data$S8, S9 = m4$jags.data$S9,
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
  list(b = m4$jags.ini$b, lambda = m4$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm4$y>0))
}

outm4 <- jags(data = datm4, parameters.to.save = c("beta","b","y2"), inits = initsm4, 
              model.file = "FinalModels/gwwam4.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m4_yp <- outm4$mean$y2
m4_yt <- c(dat4$gwwadet[177617:186833], dat4$gwwatot[186834:193563], dat4$gwwadet[193564:201836])
m4_yt <- cbind(m4_yt, c(1 - dat4$gwwadet[177617:186833], 5 - dat4$gwwatot[186834:193563], 1 - dat4$gwwadet[193564:201836]))
m4_yp[9218:15947] <- m4_yp[9218:15947]/5
m4_yp <- 0.0001 + m4_yp*0.9998
m4_dev <- -2*sum(log((m4_yp^m4_yt[,1])*((1-m4_yp)^(m4_yt[,2]))))
#eBird deviance
Em4_yp <- outm4$mean$y2[1:9217]
Em4_yt <- c(dat4$gwwadet[177617:186833])
Em4_yt <- cbind(Em4_yt, c(1 - dat4$gwwadet[177617:186833]))
Em4_yp <- 0.0001 + Em4_yp*0.9998
Em4_dev <- -2*sum(log((Em4_yp^Em4_yt[,1])*((1-Em4_yp)^(Em4_yt[,2]))))
#BBA deviance
Am4_yp <- outm4$mean$y2[9218:15947]
Am4_yt <- c(dat4$gwwatot[186834:193563])
Am4_yt <- cbind(Am4_yt, c(5 - dat4$gwwatot[186834:193563])) 
Am4_yp <- Am4_yp/5
Am4_yp <- 0.0001 + Am4_yp*0.9998
Am4_dev <- -2*sum(log((Am4_yp^Am4_yt[,1])*((1-Am4_yp)^(Am4_yt[,2]))))
#BBS deviance
Sm4_yp <- outm4$mean$y2[15948:24220]
Sm4_yt <- c(dat4$gwwadet[193564:201836])
Sm4_yt <- cbind(Sm4_yt, c(1 - dat4$gwwadet[193564:201836]))
Sm4_yp <- 0.0001 + Sm4_yp*0.9998
Sm4_dev <- -2*sum(log((Sm4_yp^Sm4_yt[,1])*((1-Sm4_yp)^(Sm4_yt[,2]))))

save(outm4, m4, m4_dev, Em4_dev, Am4_dev, Sm4_dev, 
     file = "FinalResults/gwwam4.RData")

brier4 <- mean((outm4$mean$y2[9218:24220] - dat4$gwwadet[186834:201836])^2)

pred4 <- prediction(as.numeric(outm4$mean$y2[9218:24220]), dat4$gwwadet[186834:201836])
auc4 <- performance(pred4, measure = "auc")
auc4 <- auc4@y.values[[1]]



##Model 5: gwwa (no eBird effort)
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729

m5 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
            data = dat1, family='binomial', file = 'placeholder.txt')

datm5 <- list(y = c(dat1$gwwadet[1:19829], dat1$gwwatot[19830:46832], dat1$gwwadet[46833:79509]), 
              X = m5$jags.data$X, n = m5$jags.data$n, zero = m5$jags.data$zero,
              S1 = m5$jags.data$S1, S2 = m5$jags.data$S2, S3 = m5$jags.data$S3,
              S4 = m5$jags.data$S4, S5 = m5$jags.data$S5, S6 = m5$jags.data$S6,
              S7 = m5$jags.data$S7, S8 = m5$jags.data$S8, S9 = m5$jags.data$S9,
              hsm = dat1$hsm[19830:46832],
              hsm2 = dat1$hsm[88727:95456], 
              doy = dat1$doy[19830:46832],
              doy2 = dat1$doy[88727:95456],
              doy3 = dat1$doy[46833:79509],
              doy4 = dat1$doy[95457:103729])

initsm5 <- function(){
  list(b = m5$jags.ini$b, lambda = m5$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm5$y>0))
}

outm5 <- jags(data = datm5, parameters.to.save = c("beta","b","y2"), inits = initsm5, 
              model.file = "FinalModels/gwwam5.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m5_yp <- outm5$mean$y2
m5_yt <- c(dat1$gwwadet[79510:88726], dat1$gwwatot[88727:95456], dat1$gwwadet[95457:103729])
m5_yt <- cbind(m5_yt, c(1 - dat1$gwwadet[79510:88726], 5 - dat1$gwwatot[88727:95456], 1 - dat1$gwwadet[95457:103729]))
m5_yp[9218:15947] <- m5_yp[9287:16204]/5
m5_yp <- 0.0001 + m5_yp*0.9998
m5_dev <- -2*sum(log((m5_yp^m5_yt[,1])*((1-m5_yp)^(m5_yt[,2]))))
#eBird deviance
Em5_yp <- outm5$mean$y2[1:9217]
Em5_yt <- c(dat1$gwwadet[79510:88726])
Em5_yt <- cbind(Em5_yt, c(1 - dat1$gwwadet[79510:88726]))
Em5_yp <- 0.0001 + Em5_yp*0.9998
Em5_dev <- -2*sum(log((Em5_yp^Em5_yt[,1])*((1-Em5_yp)^(Em5_yt[,2]))))
#BBA deviance
Am5_yp <- outm5$mean$y2[9218:15947]
Am5_yt <- c(dat1$gwwatot[88727:95456])
Am5_yt <- cbind(Am5_yt, c(5 - dat1$gwwatot[88727:95456]))
Am5_yp <- Am5_yp/5
Am5_yp <- 0.0001 + Am5_yp*0.9998
Am5_dev <- -2*sum(log((Am5_yp^Am5_yt[,1])*((1-Am5_yp)^(Am5_yt[,2]))))
#BBS deviance
Sm5_yp <- outm5$mean$y2[15948:24220]
Sm5_yt <- c(dat1$gwwadet[95457:103729])
Sm5_yt <- cbind(Sm5_yt, c(1 - dat1$gwwadet[95457:103729]))
Sm5_yp <- 0.0001 + Sm5_yp*0.9998
Sm5_dev <- -2*sum(log((Sm5_yp^Sm5_yt[,1])*((1-Sm5_yp)^(Sm5_yt[,2]))))

save(outm5, m5, m5_dev, Em5_dev, Am5_dev, Sm5_dev, 
     file = "FinalResults/gwwam5.RData")

brier5 <- mean((outm5$mean$y2[9218:24220] - dat1$gwwadet[88727:103729])^2)

pred5 <- prediction(as.numeric(outm5$mean$y2[9218:24220]), dat1$gwwadet[88727:103729])
auc5 <- performance(pred5, measure = "auc")
auc5 <- auc5@y.values[[1]]



##Model 6: gwwa (no filtering, no spatial balancing, & no effort for eBird)
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS- 193564:201836

m6 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
            data = dat4, family='binomial', file = 'placeholder.txt')

datm6 <- list(y = c(dat4$gwwadet[1:117936], dat4$gwwatot[117937:144939], dat4$gwwadet[144940:177616]), 
              X = m6$jags.data$X, n = m6$jags.data$n, zero = m6$jags.data$zero,
              S1 = m6$jags.data$S1, S2 = m6$jags.data$S2, S3 = m6$jags.data$S3,
              S4 = m6$jags.data$S4, S5 = m6$jags.data$S5, S6 = m6$jags.data$S6,
              S7 = m6$jags.data$S7, S8 = m6$jags.data$S8, S9 = m6$jags.data$S9,
              hsm = dat4$hsm[117937:144939],
              hsm2 = dat4$hsm[186834:193563], 
              doy = dat4$doy[117937:144939],
              doy2 = dat4$doy[186834:193563],
              doy3 = dat4$doy[144940:177616],
              doy4 = dat4$doy[193564:201836])

initsm6 <- function(){
  list(b = m6$jags.ini$b, lambda = m6$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm6$y>0))
}

outm6 <- jags(data = datm6, parameters.to.save = c("beta","b","y2"), inits = initsm6, 
              model.file = "FinalModels/gwwam6.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m6_yp <- outm6$mean$y2
m6_yt <- c(dat4$gwwadet[177617:186833], dat4$gwwatot[186834:193563], dat4$gwwadet[193564:201836])
m6_yt <- cbind(m6_yt, c(1 - dat4$gwwadet[177617:186833], 5 - dat4$gwwatot[186834:193563], 1 - dat4$gwwadet[193564:201836]))
m6_yp[9218:15947] <- m6_yp[9218:15947]/5
m6_yp <- 0.0001 + m6_yp*0.9998
m6_dev <- -2*sum(log((m6_yp^m6_yt[,1])*((1-m6_yp)^(m6_yt[,2]))))
#eBird deviance
Em6_yp <- outm6$mean$y2[1:9217]
Em6_yt <- c(dat4$gwwadet[177617:186833])
Em6_yt <- cbind(Em6_yt, c(1 - dat4$gwwadet[177617:186833]))
Em6_yp <- 0.0001 + Em6_yp*0.9998
Em6_dev <- -2*sum(log((Em6_yp^Em6_yt[,1])*((1-Em6_yp)^(Em6_yt[,2]))))
#BBA deviance
Am6_yp <- outm6$mean$y2[9218:15947]
Am6_yt <- c(dat4$gwwatot[186834:193563])
Am6_yt <- cbind(Am6_yt, c(5 - dat4$gwwatot[186834:193563])) 
Am6_yp <- Am6_yp/5
Am6_yp <- 0.0001 + Am6_yp*0.9998
Am6_dev <- -2*sum(log((Am6_yp^Am6_yt[,1])*((1-Am6_yp)^(Am6_yt[,2]))))
#BBS deviance
Sm6_yp <- outm6$mean$y2[15948:24220]
Sm6_yt <- c(dat4$gwwadet[193564:201836])
Sm6_yt <- cbind(Sm6_yt, c(1 - dat4$gwwadet[193564:201836]))
Sm6_yp <- 0.0001 + Sm6_yp*0.9998
Sm6_dev <- -2*sum(log((Sm6_yp^Sm6_yt[,1])*((1-Sm6_yp)^(Sm6_yt[,2]))))

save(outm6, m6, m6_dev, Em6_dev, Am6_dev, Sm6_dev, 
     file = "FinalResults/gwwam6.RData")

brier6 <- mean((outm6$mean$y2[9218:24220] - dat4$gwwadet[186834:201836])^2)

pred6 <- prediction(as.numeric(outm6$mean$y2[9218:24220]), dat4$gwwadet[186834:201836])
auc6 <- performance(pred6, measure = "auc")
auc6 <- auc6@y.values[[1]]


#Model 7: gwwa (full model + false pos for eBird)
#train: eBird- 1:19829, BBA- 19830:46832, BBS- 46833:79509
#test: eBird- 79510:88726, BBA- 88727:95456, BBS- 95457:103729

m7 <- jagam(gwwadet ~ s(elev, k=10) + s(slope, k=10) + s(temp, k=10) + s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + 
              s(forest, k=10) + s(road, k=10) + s(longitude, latitude, bs='ds', k=100), 
            data = dat1, family='binomial', file = 'placeholder.txt')

datm7 <- list(y = c(dat1$gwwadet[1:19829], dat1$gwwatot[19830:46832], dat1$gwwadet[46833:79509]), 
              X = m7$jags.data$X, n = m7$jags.data$n, zero = m7$jags.data$zero,
              S1 = m7$jags.data$S1, S2 = m7$jags.data$S2, S3 = m7$jags.data$S3,
              S4 = m7$jags.data$S4, S5 = m7$jags.data$S5, S6 = m7$jags.data$S6,
              S7 = m7$jags.data$S7, S8 = m7$jags.data$S8, S9 = m7$jags.data$S9,
              ehours = dat1$duration_minutes[1:19829], 
              ekm = dat1$effort_distance_km[1:19829], 
              ehours2 = dat1$duration_minutes[79510:88726], 
              ekm2 = dat1$effort_distance_km[79510:88726], 
              hsm = dat1$hsm[19830:46832],
              hsm2 = dat1$hsm[88727:95456], 
              doy = dat1$doy[19830:46832],
              doy2 = dat1$doy[88727:95456],
              doy3 = dat1$doy[46833:79509],
              doy4 = dat1$doy[95457:103729])

initsm7 <- function(){
  list(b = m7$jags.ini$b, lambda = m7$jags.ini$lambda,  beta = rep(3, 0.001), z = as.numeric(datm7$y>0), f=0.05)
}

outm7 <- jags(data = datm7, parameters.to.save = c("beta","b","y2", "f"), inits = initsm7, 
              model.file = "FinalModels/gwwam7.txt", 
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)

#Full Deviance
m7_yp <- outm7$mean$y2
m7_yt <- c(dat1$gwwadet[79510:88726], dat1$gwwatot[88727:95456], dat1$gwwadet[95457:103729])
m7_yt <- cbind(m7_yt, c(1 - dat1$gwwadet[79510:88726], 5 - dat1$gwwatot[88727:95456], 1 - dat1$gwwadet[95457:103729]))
m7_yp[9218:15947] <- m7_yp[9287:16204]/5
m7_yp <- 0.0001 + m7_yp*0.9998
m7_dev <- -2*sum(log((m7_yp^m7_yt[,1])*((1-m7_yp)^(m7_yt[,2]))))
#eBird deviance
Em7_yp <- outm7$mean$y2[1:9217]
Em7_yt <- c(dat1$gwwadet[79510:88726])
Em7_yt <- cbind(Em7_yt, c(1 - dat1$gwwadet[79510:88726]))
Em7_yp <- 0.0001 + Em7_yp*0.9998
Em7_dev <- -2*sum(log((Em7_yp^Em7_yt[,1])*((1-Em7_yp)^(Em7_yt[,2]))))
#BBA deviance
Am7_yp <- outm7$mean$y2[9218:15947]
Am7_yt <- c(dat1$gwwatot[88727:95456])
Am7_yt <- cbind(Am7_yt, c(5 - dat1$gwwatot[88727:95456]))
Am7_yp <- Am7_yp/5
Am7_yp <- 0.0001 + Am7_yp*0.9998
Am7_dev <- -2*sum(log((Am7_yp^Am7_yt[,1])*((1-Am7_yp)^(Am7_yt[,2]))))
#BBS deviance
Sm7_yp <- outm7$mean$y2[15948:24220]
Sm7_yt <- c(dat1$gwwadet[95457:103729])
Sm7_yt <- cbind(Sm7_yt, c(1 - dat1$gwwadet[95457:103729]))
Sm7_yp <- 0.0001 + Sm7_yp*0.9998
Sm7_dev <- -2*sum(log((Sm7_yp^Sm7_yt[,1])*((1-Sm7_yp)^(Sm7_yt[,2]))))


save(outm7, m7, m7_dev, Em7_dev, Am7_dev, Sm7_dev, 
     file = "FinalResults/gwwam7.RData")

brier7 <- mean((outm7$mean$y2[9218:24220] - dat1$gwwadet[88727])^2)

pred7 <- prediction(as.numeric(outm7$mean$y2[9218:24220]), dat1$gwwadet[88727:103729])
auc7 <- performance(pred7, measure = "auc")
auc7 <- auc7@y.values[[1]]





####################
library(ggplot2)
library(sf)
library(raster)
library(exactextractr)
library(RColorBrewer)
load("FinalResults/Plots/grid_pa_1km.RData")

####GWWA m1 map (load jam first)
#Generate predictive map
grid3$pred <- predict(jam1, newdata = grid3, type="response")
grid_sp <- as_Spatial(grid3)

#Change map extent
scale.parameter = 1.1
original.bbox = grid_sp@bbox 
edges = original.bbox
edges[1, ] <- (edges[1, ] - mean(edges[1, ])) * scale.parameter + mean(edges[1, 
])
edges[2, ] <- (edges[2, ] - mean(edges[2, ])) * scale.parameter + mean(edges[2, 
])

#Plot
pdf("FinalResults/Plots/gwwam1map.pdf")
spplot(grid_sp, "pred", col=NA, par.settings = list(axis.line = list(col = 'transparent')),
       xlim = edges[1, ], ylim = edges[2, ], colorkey=list(height=0.6, cex=0.5))
dev.off()

