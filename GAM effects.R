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


load("~/rileymummah@gmail.com - Google Drive/My Drive/Bird SDM/Ch1/Results/Jams/cawam1jam.RData")

tmp <- plot(jam1, pages=1)

# set up empty dataframe
dat <- data.frame(cov=NA,
                  x=NA,
                  fit=NA,
                  se=NA)

for (k in 1:8) {
  dat <- bind_rows(dat, data.frame(cov=tmp[[k]]$xlab,
                                   x=tmp[[k]]$x,
                                   fit=tmp[[k]]$fit[,1],
                                   se=tmp[[k]]$se))
}



dat %>%
  slice(-1) %>%
ggplot(aes(x=x)) +
  geom_ribbon(aes(ymin=fit-se, ymax=fit+se, group=cov, fill=cov),
              alpha=0.5) + 
  geom_line(aes(y=fit, group=cov, col=cov)) +
  facet_wrap(~cov) +
  # ylim(-4,4) +
  labs(x='')





