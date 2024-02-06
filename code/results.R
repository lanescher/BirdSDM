

library(tidyverse)


#dat_raw <- read.csv("data/proj05-fiona/results.csv")
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

q1 <- ggplot(dat1) +
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
         modgroup = "E")

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

q2 <- ggplot(dat2) +
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
