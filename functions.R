## ---------------------------
## This code was written by: r.o. mummah
## Date Created: 2024-01-23
## ---------------------------

## ---------------------------
## Objective: Store functions required to run models
##
## 
## Input:
##    
##
## Output: 
##
##
## ---------------------------

## load packages ---------------------------
library(tidyverse)
library(magrittr)
library(sf)
library(mgcv)
library(ROCR)


## load functions ---------------------------

# Generate code for GAMs
generate.code <- function(dat) {
  jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) +
          s(ppt, k=10) + s(dev, k=10) + s(forest, k=10) + #s(can, k=10) +
          s(road, k=10) + s(longitude, latitude, bs='ds', k=100),
        data = dat, family='binomial', file = 'placeholder.txt')
}

# generate.jam <- function(mod.name) {
#   load(paste0("results/out/", mod.name, ".RData"))
#   
#   jm1 <- jags.model(paste0("models/", mod.name,".txt"), 
#                     data=datm1, 
#                     inits=initsm1, 
#                     n.adapt=500, 
#                     n.chains=3)
#   update(jm1, 500)
#   
#   sam1 <- jags.samples(jm1, c("b","rho"), n.iter=2000, thin=2)
#   jam1 <- sim2jam(sam1, m1$pregam)
#   
#   save(m1, datm1, jm1, sam1, jam1,
#        file = paste0("results/jams/", mod.name, "jam.RData"))
# 
# }


