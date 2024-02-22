
## Objective ---------------------------
## To generate a reduced GAM object suitable for plotting and prediction (sim2jam) 
##
## 
## Input:
##    functions.R
##    .Rdata files in results/out
##
## Output: 
##    outputs/observations.jpg
##    outputs/detection-rate.jpg
##
## ---------------------------


## load functions ---------------------------
source('functions.R')


# Define set up spp and models
expand.grid(spp = c('cawa','cerw','gwwa','woth'),
            mod = c(1:8, 10:16)) %>%
  mutate(name = paste(spp, mod, sep='m')) -> mod.name


# for every mod.name

for (i in 1:nrow(mod.name)){

  name = mod.name$name[i]
  num = mod.name$mod[i]
  
  
  # Load .RData file
  load(paste0('results/out/',name,'.RData'))
  
  generate.jam(name, 
               dat = get(paste0('datm',num)),
               inits = get(paste0('initsm',num)),
               pregam = get(paste0('m',num)))
}

# End script