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
library(mgcv)
library(ROCR)
library(rjags)


## load functions ---------------------------

# Generate code for GAMs
generate.code <- function(dat) {
  jagam(cawadet ~ s(elev, k=10) + s(slope, k=10) + #s(temp, k=10) +
          s(ppt, k=10) + s(dev, k=10) + s(forest, k=10) + #s(can, k=10) +
          s(road, k=10) + s(longitude, latitude, bs='ds', k=100),
        data = dat, family='binomial', file = 'placeholder.txt')
}


generate.jam <- function(mod.name, dat, inits, pregam) {
  
  j <- jags.model(paste0("/caldera/hovenweep/projects/usgs/ecosystems/eesc/rmummah/proj05-fiona/models/", mod.name,".txt"),
                  data=dat,
                  inits=inits,
                  n.adapt=500,
                  n.chains=3)
  update(j, 500)

  sam <- jags.samples(j, c("b","rho"), n.iter=2000, thin=2)
  jam <- sim2jam(sam, pregam)

  save(j, sam, jam,
       file = paste0("/caldera/hovenweep/projects/usgs/ecosystems/eesc/rmummah/proj05-fiona/results/jams/",mod.name,"jam.RData"))
}


