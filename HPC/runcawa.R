## ---------------------------
## This code was written by: r.o. mummah
## For questions: rmummah@usgs.gov
## Date Created: 2024-01-26
## ---------------------------

# Use array number in script
args = commandArgs(trailingOnly = TRUE)

spp <- 'cawa'
mod <- c(1:8,10:16)
i=args[1]

source(paste0('/caldera/hovenweep/projects/usgs/ecosystems/eesc/rmummah/proj05-fiona/code/cawa',mod[i],".R"))

# End script


