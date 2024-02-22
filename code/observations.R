
## Objective ---------------------------
## To summarize the number of observations and detection rate in 
## each training dataset 
##
## 
## Input:
##    data/train1.csv
##    data/train2.csv
##    data/train3.csv
##    data/train4.csv
##    data/covdat.csv
##    data/covdat2.csv
##    data/hdat.csv
##
## Output: 
##    outputs/observations.jpg
##    outputs/detection-rate.jpg
##
## ---------------------------


## load packages ---------------------------
library(tidyverse)
library(plyr)



summarize_obs <- function(df, mod) {
  
  obs <- as.data.frame(matrix(nrow = 5, ncol = 6))
  colnames(obs) <- c("model", "group", "eBird", "BBS", "BBA", "all")
  
  
  ebd <- df %>% filter(is.na(checklist_id) == F)
  bbs <- df %>% filter(is.na(Route) == F)
  bba <- df %>% filter(is.na(BBA_ID) == F)
  
  # fill in numb obs
  obs[1, 1] <- mod
  obs[1, 2] <- "total"
  obs[1, 3] <- nrow(ebd)
  obs[1, 4] <- nrow(bbs)
  obs[1, 5] <- nrow(bba)
  obs[1, 6] <- nrow(df)
  
  # fill in numb cawa
  obs[2, 1] <- mod
  obs[2, 2] <- "cawa"
  obs[2, 3] <- nrow(ebd[which(ebd$cawadet == 1),])/ nrow(ebd)
  obs[2, 4] <- nrow(bbs[which(bbs$cawadet == 1),])/ nrow(bbs)
  obs[2, 5] <- nrow(bba[which(bba$cawatot == 1),])/ nrow(bba)
  obs[2, 6] <- (nrow(ebd[which(ebd$cawadet == 1),]) + nrow(bbs[which(bbs$cawadet == 1),]) + nrow(bba[which(bba$cawatot == 1),]))/ nrow(df)
  
  # fill in numb cerw
  obs[3, 1] <- mod
  obs[3, 2] <- "cerw"
  obs[3, 3] <- nrow(ebd[which(ebd$cerwdet == 1),])/ nrow(ebd)
  obs[3, 4] <- nrow(bbs[which(bbs$cerwdet == 1),])/ nrow(bbs)
  obs[3, 5] <- nrow(bba[which(bba$cerwtot == 1),])/ nrow(bba)
  obs[3, 6] <- (nrow(ebd[which(ebd$cerwdet == 1),]) + nrow(bbs[which(bbs$cerwdet == 1),]) + nrow(bba[which(bba$cerwtot == 1),]))/ nrow(df)
  
  # fill in numb gwwa
  obs[4, 1] <- mod
  obs[4, 2] <- "gwwa"
  obs[4, 3] <- nrow(ebd[which(ebd$gwwadet == 1),])/ nrow(ebd)
  obs[4, 4] <- nrow(bbs[which(bbs$gwwadet == 1),])/ nrow(bbs)
  obs[4, 5] <- nrow(bba[which(bba$gwwatot == 1),])/ nrow(bba)
  obs[4, 6] <- (nrow(ebd[which(ebd$gwwadet == 1),]) + nrow(bbs[which(bbs$gwwadet == 1),]) + nrow(bba[which(bba$gwwatot == 1),]))/ nrow(df)
  
  # fill in numb woth
  obs[5, 1] <- mod
  obs[5, 2] <- "woth"
  obs[5, 3] <- nrow(ebd[which(ebd$wothdet == 1),])/ nrow(ebd)
  obs[5, 4] <- nrow(bbs[which(bbs$wothdet == 1),])/ nrow(bbs)
  obs[5, 5] <- nrow(bba[which(bba$wothtot == 1),])/ nrow(bba)
  obs[5, 6] <- (nrow(ebd[which(ebd$wothdet == 1),]) + nrow(bbs[which(bbs$wothdet == 1),]) + nrow(bba[which(bba$wothtot == 1),]))/ nrow(df)
  
  
  return(obs)
}


## pull number of observations and number of detections from each file


dat <- c()




# File 1, R1 ----

mod <- "R1"


#test1 <- read.csv("data/test1.csv")
train1 <- read.csv("data/train1.csv")

#dat1 <- rbind.fill(train1, test1)

# we're only counting observations used to train the model
obs <- summarize_obs(train1, mod = mod)

dat <- bind_rows(dat, obs)



# File 2, F1 ----

mod <- "F1"

#test1 <- read.csv("data/test1.csv")
train2 <- read.csv("data/train2.csv")

#dat2 <- rbind.fill(train2, test1)

obs <- summarize_obs(train2, mod = mod)

dat <- bind_rows(dat, obs)



# File 3, F2 ----

mod <- "F2"

#test1 <- read.csv("data/test1.csv")
train3 <- read.csv("data/train3.csv")

#dat3 <- rbind.fill(train3, test1)

obs <- summarize_obs(train3, mod = mod)

dat <- bind_rows(dat, obs)




# File 4, F3 ----

mod <- "F3"

#test1 <- read.csv("data/test1.csv")
train4 <- read.csv("data/train4.csv")

#dat4 <- rbind.fill(train4, test1)

obs <- summarize_obs(train4, mod = mod)

dat <- bind_rows(dat, obs)



# File 5, F5 ----

mod <- "F5"

#test1 <- read.csv("data/test1.csv")
train1 <- read.csv("data/train1.csv")

#dat1 <- rbind.fill(train1, test1)

obs <- summarize_obs(train1, mod = mod)


dat <- bind_rows(dat, obs)




# File 6, F6 ----

mod <- "F6"

#test1 <- read.csv("data/test1.csv")
train4 <- read.csv("data/train4.csv")

#dat4 <- rbind.fill(train4, test1)

obs <- summarize_obs(train4, mod = mod)

dat <- bind_rows(dat, obs)



# File 7, E1 ----

mod <- "E1"

#test1 <- read.csv("data/test1.csv")
train1 <- read.csv("data/train1.csv")

#dat1 <- rbind.fill(train1, test1)

obs <- summarize_obs(train1, mod = mod)


dat <- bind_rows(dat, obs)


# File 8, E2 ----

mod <- "E2"

covdat <- read.csv("data/covdat.csv")
#training data ends at 177616
covdat <- covdat[1:177616,]

# this model doesn't use ebird observations at all
covdat <- filter(covdat, is.na(checklist_id) == T)

obs <- summarize_obs(covdat, mod = mod)

dat <- bind_rows(dat, obs)


# File 10, E3 ----

mod <- "E3"

covdat <- read.csv("data/covdat.csv")

#training data ends at 177616
covdat <- covdat[1:177616,]

# this model doesn't use ebird observations at all
covdat <- filter(covdat, is.na(checklist_id) == T)


obs <- summarize_obs(covdat, mod = mod)


dat <- bind_rows(dat, obs)


# File 11, F4 ----

mod <- "F4"

covdat2 <- read.csv("data/covdat2.csv")

#training data ends at 74405
covdat2 <- covdat2[1:74405,]

# this model doesn't use traveling ebird checklists
covdat2 <- filter(covdat2, protocol_type == "Stationary" | is.na(protocol_type))


obs <- summarize_obs(covdat2, mod = mod)

dat <- bind_rows(dat, obs)


# File 12, R2 ----

mod <- "R2"

train1 <- read.csv("data/train1.csv")
#test2 <- read.csv("data/test2.csv")

#dat12 <- rbind.fill(train1, test2)

obs <- summarize_obs(train1, mod = mod)


dat <- bind_rows(dat, obs)



# File 13, O3 ----

mod <- "O3"

hdat <- read.csv("data/hdat.csv")
# create BBA_ID column so that function works. It'll all be NAs because this model doesnt use BBA
hdat$BBA_ID <- NA

#training data ends at 19556
hdat <- hdat[1:19556,]


obs <- summarize_obs(hdat, mod = mod)

dat <- bind_rows(dat, obs)


# File 14, O2 ----

mod <- "O2"

hdat <- read.csv("data/hdat.csv")
# create BBA_ID column so that function works. It'll all be NAs because this model doesnt use BBA
hdat$BBA_ID <- NA

#training data ends at 19556
hdat <- hdat[1:19556,]

obs <- summarize_obs(hdat, mod = mod)


dat <- bind_rows(dat, obs)


# File 15, O1 ----

mod <- "O1"

train1 <- read.csv("data/train1.csv")
#test2 <- read.csv("data/test2.csv")

#dat12 <- rbind.fill(train1, test2)

obs <- summarize_obs(train1, mod = mod)

dat <- bind_rows(dat, obs)


# File 16, E4 ----

mod <- "E4"

#test1 <- read.csv("data/test1.csv")
train1 <- read.csv("data/train1.csv")

#dat1 <- rbind.fill(train1, test1)

obs <- summarize_obs(train1, mod = mod)


dat <- bind_rows(dat, obs)



dat.pl <- pivot_longer(dat, !c("model", "group"))
dat.pl$value[which(is.na(dat.pl$value))] <- 0

dat.pl$model <- factor(dat.pl$model, levels = c("R1", "F1", "F2", "F3", "F4", "F5", "F6",
                                        "E1", "E2", "E3", "E4", "R2", "O1", "O2", "O3"))

dat.pl$name <- factor(dat.pl$name, levels = c("eBird", "BBA", "BBS"))

spps <- c("Canada Warbler", "Cerulean Warbler", "Golden-winged Warbler", "Wood Thrush")
names(spps) <- c("cawa", "cerw", "gwwa", "woth")


cols <- c("#D81B60", "#1E88E5", "#FFC107")


ggplot(filter(dat.pl, group == "total", name != "all")) +
  geom_hline(yintercept = 0) +
  geom_bar(aes(x = model, y = value, fill = name, color = name), stat = "identity", position = "stack") +
  scale_fill_manual(values = colorspace::lighten(cols, .1)) +
  scale_color_manual(values = colorspace::darken(cols, .4)) +
  theme_bw() +
  labs(x = "Model", y = "Number of observations", fill = "Data \nsource", color = "Data \nsource")
ggsave(file = "outputs/observations.jpg", height = 5, width = 10)



ggplot(filter(dat.pl, group != "total", name != "all", value > 0)) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = model, y = value, color = name)) +
  scale_color_manual(values = colorspace::lighten(cols, .1)) +
  theme_bw() +
  labs(x = "Model", y = "Detection rate", fill = "Data \nsource", color = "Data \nsource") +
  facet_wrap(~ group, scales = "free", labeller = labeller(group = spps))
ggsave(file = "outputs/detection-rate.jpg", height = 5, width = 10)






