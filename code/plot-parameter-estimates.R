
## Objective ---------------------------
## To generate figures of nonlinear covariate effects
##
## 
## Input:
##    results/gams.rds
##
## Output: 
##    outputs/parameter-partial.jpg
##    outputs/parameter-full.jpg
##
## ---------------------------


## load packages ---------------------------

library(tidyverse)
library(mgcv)



gam <- readRDS("results/gams.rds")


# Partial figure (for text)

labs <- c("Elevation", "Slope", "Temperature", "Precipitation", "Canopy cover", "Developed land", "Forested land", "Road length")
names(labs) <- c("elev", "slope", "temp", "ppt", "can", "dev", "forest", "road")

spps <- c("Canada Warbler", "Cerulean Warbler", "Golden-winged Warbler", "Wood Thrush")
names(spps) <- c("cawa", "cerw", "gwwa", "woth")

gam <- gam %>%
  mutate(species = case_when(spp == "cawa" ~ "Canada Warbler",
                             spp == "cerw" ~ "Cerulean Warbler",
                             spp == "gwwa" ~ "Golden-winged Warbler",
                             spp == "woth" ~ "Wood Thrush"))

ggplot(data = filter(gam, name == "R1"), aes(x = x)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = fit-se, ymax = fit+se, group = species, fill = species),
              alpha=0.3) + 
  geom_line(aes(y=fit, group = species, col = species)) +
  facet_wrap(~cov, labeller = labeller(cov = labs), scales = "free") +
  labs(x = 'Covariate value', y = "Estimate", fill = "Species", color = "Species") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c("Golden-winged Warbler" = "gold1", 
                                "Wood Thrush" = "darkorange3", 
                                "Cerulean Warbler" = "steelblue1", 
                                "Canada Warbler" = "gray40")) +
  scale_fill_manual(values = c("Golden-winged Warbler" = "gold1", 
                                "Wood Thrush" = "darkorange3", 
                                "Cerulean Warbler" = "steelblue1", 
                               "Canada Warbler" = "gray40"))

ggsave(file = "outputs/parameter-partial.jpg", height = 5, width = 10)






# Full figure (for supplement)

labs <- c("Elevation", "Slope", "Temperature", "Precipitation", "Canopy cover", "Developed land", "Forested land", "Road length")
names(labs) <- c("elev", "slope", "temp", "ppt", "can", "dev", "forest", "road")

spps <- c("Canada Warbler", "Cerulean Warbler", "Golden-winged Warbler", "Wood Thrush")
names(spps) <- c("cawa", "cerw", "gwwa", "woth")

ggplot(data = gam, aes(x = x)) +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = fit-se, ymax = fit+se, group = interaction(spp, name), fill = as.factor(name)),
              alpha=0.05) + 
  geom_line(aes(y=fit, group = interaction(spp, name), col = as.factor(name))) +
  # facet_grid(rows = vars(cov), cols = vars(spp), 
  #            labeller = labeller(cov = labs, spp = spps),
  #            scales = "free") +
  facet_grid(spp ~ cov, scales = "free",
             labeller = labeller(cov = labs, spp = spps)) +
  labs(x = 'Covariate value', y = "Estimate", fill = "Model", color = "Model") +
  theme_bw() +
  theme(strip.background = element_blank())

ggsave(file = "outputs/parameter-full.jpg", height = 12, width = 12)



# Figure out which ones are way different

# there are three blue-green lines that are very different. They all have very low
# y values for GWWA elevation
tmp <- gam %>%
  filter(spp == "gwwa", cov == "elev") %>%
  group_by(name) %>%
  summarize(low = min(fit))
# F2, F3, and F6 have very different values

