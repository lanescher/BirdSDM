## ---------------------------
## This code was written by: r.o. mummah
## For questions: rmummah@usgs.gov
## Date Created: 2024-01-30
## ---------------------------

## ---------------------------
## Objective: To import RData files generated from HPC JAGS runs and save
##            performance metrics (brier score, AUC, deviance)
##
## 
## Input:
##    .Rdata files found in results/out folder
##
## Output: 
##    performancemetrics.rds
##
## ---------------------------

## load packages ---------------------------
library(tidyverse)
library(magrittr)


# Extract performance metrics from saved .RData results files -------------

# Define set up spp and models
files <- data <- expand.grid(spp = c('cawa','cerw','gwwa','woth'),
                     mod = c(1:8, 10:16))

for (i in 1:nrow(files)) {
  
  # Load .RData file
  load(paste0('results/out/',files$spp[i],'m',files$mod[i],'.RData'))
  

  # Extract estimates for brier score, auc, and model deviance
  data.frame(get(grep('brier',ls(), value = T)),
             get(grep('auc', ls(), value=T)),
             # Second to last deviance always recorded
             get(nth(grep(paste0('m',files$mod[i],'_dev'), ls(), value=T),-2))) -> x
  
  # Extract names of these estimates
  colnames(x) <- c(grep('brier',ls(), value = T),
                   grep('auc', ls(), value=T),
                   # Second to last deviance always recorded
                   nth(grep(paste0('m',files$mod[i],'_dev'), ls(), value=T),-2))
  
  # Transform into dataframe and merge with spp/model data.frame
  pivot_longer(x, cols = everything(), names_to = 'metric', values_to = 'value') %>%
    mutate(spp = files$spp[i],
           mod = files$mod[i]) %>%
    full_join(data,.) -> data
  
  rm(list = ls()[!ls() %in% c('data','files','i')])
  
  print(paste0('Model ', files$mod[i], ' from ', files$spp[i],' is complete.'))
  
}

# Define lookup dataframe for model number and model name
mod.names <- data.frame(num = c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16),
                        name = c("R1","F1","F2","F3","F5","F6","E1","E2","E3","F4","R2","O3","O2","O1","E4"))

data %>%
  filter(!is.na(metric)) %>%
  left_join(., mod.names, by = c('mod' = 'num')) %>%
  mutate(metric = ifelse(grepl('brier', metric), 'brier', 
                         ifelse(grepl('auc', metric), 'auc',
                                ifelse(grepl('dev', metric), 'dev', metric)))) -> data


saveRDS(data, 'results/performancemetrics.rds')
  

# End script

