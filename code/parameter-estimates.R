library(tidyverse)
library(mgcv)

#path <- "G:/.shortcut-targets-by-id/1Clv1O8Hy66BccRXUd0EECkucflHAbxer/Bird SDM/"


gam <- readRDS("results/gams.rds")



# dat1 <- read.csv("/data/final/test1.csv")
# 
# dat2 <- load(paste0(path, "Ch1/results/cawam1.rdata"))
# 
# # loop through each species
# sp <- c("cawa", "cerw", "woth", "gwwa")
# sp <- "cerw"
# 
# all <- c()
# for (s in 1:length(sp)) {
#   load(paste0(path, "Ch1/Results/Jams/", sp[s], "m1jam.RData"))
#   
#   
#   tmp <- plot(jam1, pages=1)
#   
#   # set up empty dataframe
#   # dat <- data.frame(cov=NA,
#   #                   x=NA,
#   #                   fit=NA,
#   #                   se=NA)
#   
#   dat <- c()
#   for (k in 1:8) {
#     dat <- bind_rows(dat, data.frame(sp = sp[s],
#                                      cov=tmp[[k]]$xlab,
#                                      x=tmp[[k]]$x,
#                                      fit=tmp[[k]]$fit[,1],
#                                      se=tmp[[k]]$se))
#   }
#   
#   all <- bind_rows(all, dat)
#   
# }


labs <- c("Elevation", "Slope", "Temperature", "Precipitation", "Canopy cover", "Developed land", "Forested land", "Road length")
names(labs) <- c("elev", "slope", "temp", "ppt", "can", "dev", "forest", "road")

spps <- c("Canada Warbler", "Cerulean Warbler", "Golden-winged Warbler", "Wood Thrush")
names(spps) <- c("cawa", "cerw", "gwwa", "woth")

ggplot(data = gam, aes(x = x)) +
  geom_hline(yintercept = 0) +
  # geom_ribbon(aes(ymin = fit-se, ymax = fit+se, group = interaction(spp, mod), fill = spp),
  #             alpha=0.05) + 
  # geom_line(aes(y=fit, group = interaction(spp, mod), col = spp)) +
  geom_ribbon(aes(ymin = fit-se, ymax = fit+se, group = interaction(spp, name), fill = as.factor(name)),
              alpha=0.05) + 
  geom_line(aes(y=fit, group = interaction(spp, name), col = as.factor(name))) +
  #facet_wrap(~ cov, labeller = labeller(cov = labs), scales = "free") +
  facet_grid(rows = vars(cov), cols = vars(spp), labeller = labeller(cov = labs, spp = spps)) +
  # ylim(-4,4) +
  labs(x='Covariate value', fill = "Model", color = "Model") +
  # scale_color_manual(values = c("gwwa" = "gold1", 
  #                               "woth" = "darkorange3", 
  #                               "cerw" = "steelblue1", 
  #                               "cawa" = "gray40"),
  #                    labels = c("Canada Warbler",
  #                               "Cerulean Warbler",
  #                               "Golden-winged Warbler",
  #                               "Wood Thrush")) +
  # scale_fill_manual(values = c("gwwa" = "gold1", 
  #                              "woth" = "darkorange3", 
  #                              "cerw" = "steelblue1", 
  #                              "cawa" = "gray40"),
  #                   labels = c("Canada Warbler",
  #                              "Cerulean Warbler",
  #                              "Golden-winged Warbler",
  #                              "Wood Thrush")) +
  theme_bw() +
  theme(strip.background = element_blank())
