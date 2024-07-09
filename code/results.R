
## Objective ---------------------------
## To post-process, assemble output, and generate a figure.
##
## 
## Input:
##    results/performancemetrics.rds
##
## Output: 
##    outputs/metrics.jpg
##    outputs/metricstab1.csv
##    outputs/metricstab2.csv
##
## ---------------------------


## load packages ---------------------------
library(tidyverse)


dat_raw <- readRDS("results/performancemetrics.rds")



#### Q1 ----

dat1 <- dat_raw %>%
  filter(mod %in% c(1, 2, 3, 4, 5, 6, 11)) %>%
  select(!mod) %>%
  pivot_wider() %>%
  pivot_longer(cols = c("F1", "F2", "F3", "F4", "F5", "F6")) %>%
  mutate(val = ((value - R1) / R1) * 100,
         val = case_when(metric != "auc" ~ val*-1,
                         T ~ val))



labs <- c("AUC", "Brier Score", "Deviance")
names(labs) <- c("auc", "brier", "dev")

q1 <- ggplot(filter(dat1, metric != "dev")) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = name, y = val, color = spp)) +
  geom_line(aes(x = name, y = val, color = spp, group = spp)) +
  facet_wrap(~metric, scales = "free", labeller = labeller(metric = labs)) +
  labs(y = "Percent improvement", x = "Model") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c("gwwa" = "gold1", 
                                "woth" = "darkorange3", 
                                "cerw" = "steelblue1", 
                                "cawa" = "gray40"),
                     guide = "none")



#### Q2 ----

dat2a <- dat_raw %>%
  filter(mod %in% c(1, 7, 8, 10, 16)) %>%
  select(!mod) %>%
  pivot_wider() %>%
  pivot_longer(cols = c("E1", "E2", "E3", "E4")) %>%
  mutate(val = ((value - R1) / R1) * 100,
         val = case_when(metric != "auc" ~ val*-1,
                         T ~ val),
         modnum = substr(name, 2, 2),
         modgroup = "E") %>%
  filter(modnum != 4)

dat2b <- dat_raw %>%
  filter(mod %in% c(12, 15, 13, 14)) %>%
  select(!mod) %>%
  pivot_wider() %>%
  pivot_longer(cols = c("O1", "O2", "O3")) %>%
  mutate(val = ((value - R2) / R2) * 100,
         val = case_when(metric != "auc" ~ val*-1,
                         T ~ val),
         modnum = substr(name, 2, 2),
         modgroup = "O")

dat2 <- bind_rows(dat2a, dat2b)


labs <- c("AUC", "Brier Score", "Deviance")
names(labs) <- c("auc", "brier", "dev")

q2 <- ggplot(filter(dat2, metric != "dev")) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = modnum, y = val, color = spp)) +
  geom_line(aes(x = modnum, y = val, color = spp, group = interaction(spp, modgroup), linetype = modgroup)) +
  facet_wrap(~metric, scales = "free", labeller = labeller(metric = labs)) +
  labs(y = "Percent improvement", x = "Model") +
  theme_bw() +
  theme(strip.background = element_blank()) +
  scale_color_manual(values = c("gwwa" = "gold1", 
                                "woth" = "darkorange3", 
                                "cerw" = "steelblue1", 
                                "cawa" = "gray40"),
                     guide = "none") +
  scale_linetype_manual(guide = "none",
                        values = c("solid", "dotted"))




ggpubr::ggarrange(q1, q2, ncol = 1,
                  labels = "auto", align = "hv")


ggsave(file = "outputs/metrics.jpg",
       height = 6, width = 8)



### table

tab <- dat_raw %>%
  mutate(Species = case_when(spp == "cawa" ~ "Canada Warbler",
                             spp == "cerw" ~ "Cerulean Warbler",
                             spp == "gwwa" ~ "Golden-winged Warbler",
                             spp == "woth" ~ "Wood Thrush"),
         Metric = case_when(metric == "auc" ~ "AUC",
                            metric == "brier" ~ "Brier Score",
                            metric == "dev" ~ "Deviance"),
         Value = case_when(metric == "dev" ~ round(value, 1),
                           T ~ round(value, 4)),
         Model = name) %>%
  select(Species, Value, Model, Metric) %>%
  pivot_wider(names_from = c("Species", "Metric"), values_from = "Value")
row.names(tab) <- tab$Model

tab1 <- tab[c("R1", "F1", "F2", "F3", "F4", "F5", "F6"),]
tab2 <- tab[c("R1", "E1", "E2", "E3", "E4", "R2", "O1", "O2", "O3"),]


write.csv(tab1, file = "outputs/metricstab1.csv", row.names = F)
write.csv(tab2, file = "outputs/metricstab2.csv", row.names = F)


# End script

