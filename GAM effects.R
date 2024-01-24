## FROM CAWA1.R
# jm1 <- jags.model("FinalModels/cawam1.txt", data=datm1, inits=initsm1, n.adapt=500, n.chains=3)
# update(jm1, 500)
# sam1 <- jags.samples(jm1, c("b","rho"), n.iter=2000, thin=2)
# jam1 <- sim2jam(sam1, m1$pregam)
# 
# save(m1, jm1, sam1, jam1,
#      file = "FinalResults/Jams/cawam1jam.RData")
# 
# # pdf("FinalResults/Plots/cawam1cov.pdf")
# plot(jam1, pages=1)
# # dev.off()


load("results/jams/cawam1jam.RData")
load("results/jams/cawam2jam.RData")

tmp <- plot(jam2, pages=1)

# set up empty dataframe
dat <- data.frame(cov=NA,
                  x=NA,
                  fit=NA,
                  se=NA)

# Collect all but lat/long, which is index 7
for (k in 1:6) {
  dat <- bind_rows(dat, data.frame(cov=tmp[[k]]$xlab,
                                   x=tmp[[k]]$x,
                                   fit=tmp[[k]]$fit[,1],
                                   se=tmp[[k]]$se,
                                   mod='F1'))
}

# WIP for plotting lat/long effects
data.frame(x=tmp[[7]]$x,
           y=tmp[[7]]$fit[,1]) %>% view()
  ggplot() + geom_line(aes(x=x,y=y))


dat %>%
  slice(-1) %>%
ggplot(aes(x=x)) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, group=mod, fill=mod),
              alpha=0.5) + 
  geom_line(aes(y=fit, group=mod, col=mod)) +
  facet_wrap(~cov, scales = 'free') +
  labs(x='')


load("results/out/cawam1.RData")
load("results/out/cawam2.RData")
load("results/out/cawam3.RData")

data.frame(brier = c(brier2-brier1, brier3-brier1)/brier1,
           dev = c(m2_dev-m1_dev, m3_dev-m1_dev)/m1_dev,
           auc = c(auc2-auc1, auc3-auc1)/auc1,
           mod = c('F1','F2')) %>% 
  pivot_longer(cols=1:3,
               names_to = 'metric',
               values_to = 'value') %>%
  ggplot() +
  geom_hline(aes(yintercept = 0), col='black') +
  geom_line(aes(x=mod, y=value, group=metric, col=metric))
  # facet_wrap(~)



