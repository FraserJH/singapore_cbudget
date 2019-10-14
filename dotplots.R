


## Coral cover
(c_cover<-ggplot(dat,aes(x=Site, y=coral_cover))+geom_boxplot() + 
  geom_jitter( width = 0.15) + theme_bw())
(c_coverw<-ggplot(dat,aes(x=Site, y=cover_general))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw())
(c_cover<-ggplot(dat,aes(x=Site, y=cover_stress))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw())

## dot plots
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=numb_gen, y=gnet)) + stat_smooth(method='lm') + geom_point(aes(colour=Site))) 
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=cover_general/100*coral_cover, y=gnet, colour = Site))
  + geom_point()) 
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=cover_stress/100*coral_cover, y=hcprod_mean, colour = Site))
  + geom_point()) 
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=cover_weedy/100*coral_cover, y=hcprod_mean, colour = Site))
  + geom_point())

## contribution of functional coral groups
