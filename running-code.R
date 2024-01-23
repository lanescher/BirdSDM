# model running
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



# Model 3: F2 - CAWA (no eBird spatial balancing) -------------------------
#train: eBird- 1:111728, BBA- 111729:138731, BBS- 138732:171408
train.eBird = 1:111728
train.BBA = 111729:138731
train.BBS = 138732:171408
#test: eBird- 171409:180625, BBA- 180626:187355, BBS- 187356:195628
test.eBird = 171409:180625
test.BBA = 180626:187355
test.BBS = 187356:195628

m3 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) +
              s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + #s(forest, k=10) +
              s(road, k=10) + s(longitude, latitude, bs='ds', k=100),
            data = dat3, family='binomial', file = 'placeholder.txt')

datm3 <- list(y = c(dat3$cawadet[train.eBird], dat3$cawatot[train.BBA], dat3$cawadet[train.BBS]),
              X = m3$jags.data$X, n = m3$jags.data$n, zero = m3$jags.data$zero,
              S1 = m3$jags.data$S1, S2 = m3$jags.data$S2, S3 = m3$jags.data$S3,
              S4 = m3$jags.data$S4, S5 = m3$jags.data$S5, S6 = m3$jags.data$S6,
              S7 = m3$jags.data$S7, #S8 = m3$jags.data$S8, S9 = m3$jags.data$S9,
              ehours = dat3$duration_minutes[train.eBird],
              ekm = dat3$effort_distance_km[train.eBird],
              ehours2 = dat3$duration_minutes[test.eBird],
              ekm2 = dat3$effort_distance_km[test.eBird],
              hsm = dat3$hsm[train.BBA],
              hsm2 = dat3$hsm[test.BBA],
              doy = dat3$doy[train.BBA],
              doy2 = dat3$doy[test.BBA],
              doy3 = dat3$doy[train.BBS],
              doy4 = dat3$doy[test.BBS])

initsm3 <- function(){
  list(b = m3$jags.ini$b,
       lambda = m3$jags.ini$lambda,
       beta = rep(3, 0.001),
       z = as.numeric(datm3$y>0))
}

outm3 <- jags(data = datm3,
              parameters.to.save = c("beta","b","y2"),
              inits = initsm3,
              model.file = "models/cawam3.txt",
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)


### Performance metrics -----------------------------------------------------

#Full Deviance
m3_yp <- outm3$mean$y2
m3_yt <- c(dat3$cawadet[test.eBird], dat3$cawatot[test.BBA], dat3$cawadet[test.BBS])
m3_yt <- cbind(m3_yt, c(1 - dat3$cawadet[test.eBird], 5 - dat3$cawatot[test.BBA], 1 - dat3$cawadet[test.BBS]))
m3_yp[9218:15947] <- m3_yp[9218:15947]/5
m3_yp <- 0.0001 + m3_yp*0.9998
m3_dev <- -2*sum(log((m3_yp^m3_yt[,1])*((1-m3_yp)^(m3_yt[,2]))))
#eBird deviance
Em3_yp <- outm3$mean$y2[1:9217]
Em3_yt <- c(dat3$cawadet[test.eBird])
Em3_yt <- cbind(Em3_yt, c(1 - dat3$cawadet[test.eBird]))
Em3_yp <- 0.0001 + Em3_yp*0.9998
Em3_dev <- -2*sum(log((Em3_yp^Em3_yt[,1])*((1-Em3_yp)^(Em3_yt[,2]))))
#BBA deviance
Am3_yp <- outm3$mean$y2[9218:15947]
Am3_yt <- c(dat3$cawatot[test.BBA])
Am3_yt <- cbind(Am3_yt, c(5 - dat3$cawatot[test.BBA]))
Am3_yp <- Am3_yp/5
Am3_yp <- 0.0001 + Am3_yp*0.9998
Am3_dev <- -2*sum(log((Am3_yp^Am3_yt[,1])*((1-Am3_yp)^(Am3_yt[,2]))))
#BBS deviance
Sm3_yp <- outm3$mean$y2[15948:24220]
Sm3_yt <- c(dat3$cawadet[test.BBS])
Sm3_yt <- cbind(Sm3_yt, c(1 - dat3$cawadet[test.BBS]))
Sm3_yp <- 0.0001 + Sm3_yp*0.9998
Sm3_dev <- -2*sum(log((Sm3_yp^Sm3_yt[,1])*((1-Sm3_yp)^(Sm3_yt[,2]))))


# Brier score
brier3 <- mean((outm3$mean$y2[9218:24220] - dat3$cawadet[180626:195628])^2)

# AUC
pred3 <- prediction(as.numeric(outm3$mean$y2[9218:24220]), dat3$cawadet[180626:195628])
auc3 <- performance(pred3, measure = "auc")
auc3 <- auc3@y.values[[1]]

#roc3 <- roc(dat3$cawadet[180668:195774], as.numeric(outm3$mean$y2[9218:24220]), ci.auc=TRUE, auc=TRUE, parallel=TRUE)

save(outm3, m3, datm3, m3_dev, Em3_dev, Am3_dev, Sm3_dev,
     brier3, pred3, auc3,
     file = "results/out/cawam3.RData")


### GAMs --------------------------------------------------------------------

#Sim2Jam
require(rjags)
# load("results/out/cawam3.RData")

jm3 <- jags.model("models/cawam3.txt",
                  data=datm3,
                  inits=initsm3,
                  n.adapt=500,
                  n.chains=3)
update(jm3, 500)
sam3 <- jags.samples(jm3, c("b","rho"), n.iter=2000, thin=2)
jam3 <- sim3jam(sam3, m3$pregam)

save(m3, jm3, sam3, jam3,
     file = "results/jams/cawam3jam.RData")


# Clear workspace for next run
rm(list=setdiff(ls(), c('test1','train1','train2','train3','train4',
                        'dat1','dat2','dat3','dat4')))




# Model 4: F3 - CAWA (no eBird filtering or spatial balancing) ------------
#train: eBird- 1:117936, BBA- 117937:144939, BBS- 144940:177616
train.eBird = 1:117936
train.BBA = 117937:144939
train.BBS = 144940:177616
#test: eBird- 177617:186833, BBA- 186834:193563, BBS-193564:201836
test.eBird = 177617:186833
test.BBA = 186834:193563
test.BBS = 193564:201836

m4 <- jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) +
              s(ppt, k=10) + s(can, k=10) + s(dev, k=10) + #s(forest, k=10) +
              s(road, k=10) + s(longitude, latitude, bs='ds', k=100),
            data = dat4, family='binomial', file = 'placeholder.txt')

datm4 <- list(y = c(dat4$cawadet[train.eBird], dat4$cawatot[train.BBA],
                    dat4$cawadet[train.BBS]),
              X = m4$jags.data$X, n = m4$jags.data$n, zero = m4$jags.data$zero,
              S1 = m4$jags.data$S1, S2 = m4$jags.data$S2, S3 = m4$jags.data$S3,
              S4 = m4$jags.data$S4, S5 = m4$jags.data$S5, S6 = m4$jags.data$S6,
              S7 = m4$jags.data$S7, S8 = m4$jags.data$S8, S9 = m4$jags.data$S9,
              ehours = dat4$duration_minutes[train.eBird],
              ekm = dat4$effort_distance_km[train.eBird],
              ehours2 = dat4$duration_minutes[test.eBird],
              ekm2 = dat4$effort_distance_km[test.eBird],
              hsm = dat4$hsm[train.BBA],
              hsm2 = dat4$hsm[test.BBA],
              doy = dat4$doy[train.BBA],
              doy2 = dat4$doy[test.BBA],
              doy3 = dat4$doy[train.BBS],
              doy4 = dat4$doy[test.BBS])

initsm4 <- function(){
  list(b = m4$jags.ini$b,
       lambda = m4$jags.ini$lambda,
       beta = rep(3, 0.001),
       z = as.numeric(datm4$y>0))
}

outm4 <- jags(data = datm4,
              parameters.to.save = c("beta","b","y2"),
              inits = initsm4,
              model.file = "models/cawam4.txt",
              n.chains = 3, n.thin = 2, n.adapt = 500, n.burnin = 500, n.iter = 2500)


### Performance metrics -----------------------------------------------------

#Full Deviance
m4_yp <- outm4$mean$y2
m4_yt <- c(dat4$cawadet[test.eBird], dat4$cawatot[test.BBA], dat4$cawadet[test.BBS])
m4_yt <- cbind(m4_yt, c(1 - dat4$cawadet[test.eBird], 5 - dat4$cawatot[test.BBA], 1 - dat4$cawadet[test.BBS]))
m4_yp[9218:15947] <- m4_yp[9218:15947]/5
m4_yp <- 0.0001 + m4_yp*0.9998
m4_dev <- -2*sum(log((m4_yp^m4_yt[,1])*((1-m4_yp)^(m4_yt[,2]))))
#eBird deviance
Em4_yp <- outm4$mean$y2[1:9217]
Em4_yt <- c(dat4$cawadet[test.eBird])
Em4_yt <- cbind(Em4_yt, c(1 - dat4$cawadet[test.eBird]))
Em4_yp <- 0.0001 + Em4_yp*0.9998
Em4_dev <- -2*sum(log((Em4_yp^Em4_yt[,1])*((1-Em4_yp)^(Em4_yt[,2]))))
#BBA deviance
Am4_yp <- outm4$mean$y2[9218:15947]
Am4_yt <- c(dat4$cawatot[test.BBA])
Am4_yt <- cbind(Am4_yt, c(5 - dat4$cawatot[test.BBA]))
Am4_yp <- Am4_yp/5
Am4_yp <- 0.0001 + Am4_yp*0.9998
Am4_dev <- -2*sum(log((Am4_yp^Am4_yt[,1])*((1-Am4_yp)^(Am4_yt[,2]))))
#BBS deviance
Sm4_yp <- outm4$mean$y2[15948:24220]
Sm4_yt <- c(dat4$cawadet[test.BBS])
Sm4_yt <- cbind(Sm4_yt, c(1 - dat4$cawadet[test.BBS]))
Sm4_yp <- 0.0001 + Sm4_yp*0.9998
Sm4_dev <- -2*sum(log((Sm4_yp^Sm4_yt[,1])*((1-Sm4_yp)^(Sm4_yt[,2]))))


# Brier score
brier4 <- mean((outm4$mean$y2[9218:24220] - dat4$cawadet[186834:201836])^2)

# AUC
pred4 <- prediction(as.numeric(outm4$mean$y2[9218:24220]), dat4$cawadet[186834:201836])
auc4 <- performance(pred4, measure = "auc")
auc4 <- auc4@y.values[[1]]


save(outm4, m4, datm4, m4_dev, Em4_dev, Am4_dev, Sm4_dev,
     brier4, pred4, auc4,
     file = "results/out/cawam4.RData")


### GAMs --------------------------------------------------------------------

#Sim2Jam
require(rjags)
# load("results/out/cawam4.RData")

jm4 <- jags.model("models/cawam4.txt",
                  data=datm4,
                  inits=initsm4,
                  n.adapt=500,
                  n.chains=3)
update(jm4, 500)
sam4 <- jags.samples(jm4, c("b","rho"), n.iter=2000, thin=2)
jam4 <- sim4jam(sam4, m4$pregam)

save(m4, jm4, sam4, jam4,
     file = "results/jams/cawam4jam.RData")


# Clear workspace for next run
rm(list=setdiff(ls(), c('test1','train1','train2','train3','train4',
                        'dat1','dat2','dat3','dat4')))
