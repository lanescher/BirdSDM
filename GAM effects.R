library(mgcv)
library(ROCR)
library(dplyr)


# Define set up spp and models
files <- data <- expand.grid(spp = c('cawa','cerw','gwwa','woth'),
                             mod = c(1:8, 10:16))


for (i in 1:nrow(files)) {
  # load data [j, sam, jam]
  load(paste0("results/jams/",files$spp[i],"m",files$mod[i],"jam.RData"))
  
  # set up empty dataframe
  dat <- data.frame(cov=NA,
                    x=NA,
                    fit=NA,
                    se=NA,
                    mod=NA,
                    spp=NA)
  
  # Plot jam to extract estimates
  tmp <- plot(jam, pages=1)
  
  
  # Collect all but lat/long, which is index 7
  for (k in 1:6) {
    dat <- bind_rows(dat, data.frame(cov=tmp[[k]]$xlab,
                                     x=tmp[[k]]$x,
                                     fit=tmp[[k]]$fit[,1],
                                     se=tmp[[k]]$se,
                                     mod=files$mod[i],
                                     spp=files$spp[i]))
  }
  
  # WIP for plotting lat/long effects
  # data.frame(x=tmp[[7]]$x,
  #            y=tmp[[7]]$fit[,1]) %>% view()
  #   ggplot() + geom_line(aes(x=x,y=y))
  
  dat %>%
    # Remove row of NA
    slice(-1) %>%
    full_join(data,.) -> data
  
  rm(list = ls()[!ls() %in% c('data','files','i')])
  
  print(paste0('Model ', files$mod[i], ' from ', files$spp[i],' is complete.'))
    
}

mod.names <- data.frame(num = c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16),
                        name = c("R1","F1","F2","F3","F5","F6","E1","E2","E3","F4","R2","O3","O2","O1","E4"))

data %>%
  filter(!is.na(cov)) %>% 
  left_join(., mod.names, by = c('mod'='num')) -> data

saveRDS(data, 'results/gams.rds')


data <- readRDS('results/gams.rds')

data %>%
  separate_wider_position(cols = name,
                          cols_remove = FALSE,
                          widths = c('group' = 1, 'num' = 1)) %>%
  select(-num) %>%
ggplot(aes(x=x)) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se,
                  group=mod, fill=as.factor(mod)),
              alpha=0.25) +
  geom_line(aes(y=fit, group=mod, col=as.factor(mod))) +
  facet_grid(spp~cov, scales = 'free') +
  labs(x='') +
  theme_bw()




