library(tidyverse)

dat <- read.csv("../species-futures/data/proj05-fiona/observations.csv")

dat$code <- factor(dat$code, levels = c("R1", "F1", "F2", "F3", "F4", "E1", "E2",
                                        "I1", "I2", "I3", "R2", "O1", "O2", "O3"))



# total observations
dat1 <- dat %>%
  select(model, code, eBird, BBA, BBS) %>%
  pivot_longer(!c("model", "code"))

dat1a <- dat1 %>%
  group_by(code) %>%
  summarize(total = sum(value)) %>%
  inner_join(dat1, by = "code") %>%
  mutate(prop = value/total)




cols <- c("#D81B60", "#1E88E5", "#FFC107")


ggplot(dat1) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = code, y = value, fill = name, color = name), stat = "identity", position = "stack") +
  scale_fill_manual(values = colorspace::lighten(cols, .1)) +
  scale_color_manual(values = colorspace::darken(cols, .4)) +
  theme_bw() +
  labs(x = "Model", y = "Total observations", fill = "Data \nsource", color = "Data \nsource")

ggsave("outputs/proj05-fiona/figures/totalobservations.jpg", height = 6, width = 9)


# detections

dat2 <- dat %>%
  #select(model, code, Total, Canada.Warbler, Cerulean.Warbler, Golden.winged.Warbler, Wood.Thrush) %>%
  mutate(CAWA = Canada.Warbler/Total,
         CEWA = Cerulean.Warbler/Total,
         GWWA = Golden.winged.Warbler/Total,
         WOTH = Wood.Thrush/Total) %>%
  select(model, code, CAWA, CEWA, GWWA, WOTH) %>%
  pivot_longer(!c("model", "code")) %>%
  mutate(perc = value * 100)

labs <- c("Canada Warbler", "Cerulean Warbler", "Golden-winged Warbler", "Wood Thrush")
names(labs) <- c("CAWA", "CEWA", "GWWA", "WOTH")


ggplot(dat2) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = code, y = value), stat = "identity", position = "stack",
           fill = "gray80", color = "gray20") +
  facet_wrap(~name, scales = "free_y", labeller = labeller(name = labs)) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  labs(x = "Model", y = "Detection rate")


ggsave("outputs/proj05-fiona/figures/detections.jpg", height = 6, width = 9)
