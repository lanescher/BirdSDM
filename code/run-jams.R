## ---------------------------
## This code was written by: r.o. mummah
## For questions: rmummah@usgs.gov
## Date Created: 2024-01-30
## ---------------------------

## ---------------------------
## Objective: 
##
## 
## Input:
##    
##
## Output: 
##
##
## ---------------------------


## load functions ---------------------------
source('functions.R')

## load data --------------------------------

# Define set up spp and models
expand.grid(spp = c('cawa','cerw','gwwa','woth'),
            mod = c(1:8, 10:16)) %>%
  mutate(name = paste(spp, mod, sep='m')) -> mod.name


# for every mod.name
i = 1
name = mod.name$name[i]
num = mod.name$mod[i]

# Load .RData file
load(paste0('results/out/',name[i],'.RData'))

generate.jam(name, 
             dat = get(paste0('datm',num)),
             inits = get(paste0('initsm',num)),
             pregam = get(paste0('m',num)))


