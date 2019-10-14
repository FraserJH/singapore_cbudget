## ==============================
## boxplot scripts ==
## ==============================

## load libraries
library(devtools)
library(fishualize)
library(ggplot2)
library(cowplot)
library(lme4)
library(sjPlot)
library(Rmisc)
library(lmerTest)
library(mosaic)
library(tidyverse)
library(psycho)
#library(mosaic)
library(here)
#library(RColorBrewer)
library(reshape2)
library(scales)
library(ggsci)
## read data
dat<-read.csv('summarydat_newintbio.csv')
head(dat)
dat<-transform(dat, Site = reorder(Site, dist_port))
dat$sitenum <- as.numeric(dat$Site)
dat<-dat[-c(43,86),]
## ==============================
## ===       Box plots        ===
## ==============================
pal<-fish(8, option = 'Naso_lituratus')
## Gnet
(gnet<-ggplot(dat,aes(x=sitenum, y=gnet, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -4, ymax =12), fill='lightgrey')+
  geom_boxplot(outlier.shape=NA, aes(fill = Site)) + 
    scale_fill_fish_d(option = "Naso_lituratus") +
  geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('Gnet (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                  breaks = c(-4.0,-2.0,0.0,2.0,4.0,6.0,8.0,10.0,12), limits = c(-4,12),
                                                  labels = c('-4.0','-2.0', '0.0','2.0','4.0','6.0','8.0','10.0', ''),
                                                  expand=c(0,0))+
   scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
                      labels = c('P.Hantu', 'TPT','P.Jong', 'P.Semakau', 'Sisters',
                                 'P.Kusu', 'P. Satuma', 'All'))+
   theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    geom_hline(yintercept=0, linetype = 2)+
   theme(panel.grid = element_blank(),
         panel.background = element_rect(fill = NA, colour = NA),
         axis.line = element_line(colour = 'black'),
         axis.ticks = element_line(colour = 'black'),
         legend.position = 'none'))

## HC prod
(hcprod<-ggplot(dat,aes(x=sitenum, y=hcprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =12), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = Site)) + 
    scale_fill_fish_d(option = "Naso_lituratus") + 
    geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('Coral G (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                    breaks = c(0.0,2.0,4.0,6.0,8.0,10.0,12), limits = c(0,12),
                                                    labels = c('0.0','2.0','4.0','6.0','8.0','10.0', ''),
                                                    expand=c(0,0))+
    scale_x_continuous(name = " ",breaks = c(1,2,3,4,5,6,7,8), 
                       labels = NULL)+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'), 
          legend.position = 'none'))


## cca
(cca<-ggplot(dat,aes(x=sitenum, y=ccaprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =0.25), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = Site)) + 
    scale_fill_fish_d(option = "Naso_lituratus") + 
    geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('CCA G (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                    breaks = c(0,0.05,0.10,0.15,0.20,0.25), limits = c(0,0.25),
                                                    expand=c(0,0))+
    scale_x_continuous(name = " ",breaks = c(1,2,3,4,5,6,7,8), 
                       labels = NULL)+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'),
          legend.position = 'none'))



## Erosion


(eros<-ggplot(dat,aes(x=sitenum, y=eros_tot, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -5, ymax =2), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = Site)) + 
    scale_fill_fish_d(option = "Naso_lituratus") + 
    geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('Erosion (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                    breaks = c(2,1,0,-1,-2,-3,-4,-5), limits = c(-5,2),
                                                    labels = c('2.0','1.0','0.0','-1.0','-2.0','-3.0','-4.0',''),expand=c(0,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
                       labels = c('P.Hantu', 'TPT','P.Jong', 'P.Semakau', 'Sisters',
                                  'P.Kusu', 'P. Satuma', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    geom_hline(yintercept = 0, linetype = 2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'), 
          legend.position = 'none'))

(rug<-ggplot(dat,aes(x=sitenum, y=rug, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =3.5), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = Site)) + 
    scale_fill_fish_d(option = "Naso_lituratus") + 
    geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('Erosion (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                    breaks = c(3.5,3, 2,1,0,-1,-2,-3,-4,-5), limits = c(0,3.5),
                                                    labels = c('', '3.0', '2.0','1.0','0.0','-1.0','-2.0','-3.0','-4.0',''),expand=c(0,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
                       labels = c('P.Hantu', 'TPT','P.Jong', 'P.Semakau', 'Sisters',
                                  'P.Kusu', 'P. Satuma', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    geom_hline(yintercept = 0, linetype = 2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'), 
          legend.position = 'none'))


plot_grid(hcprod,  cca,eros,gnet, labels = c('A', 'B', 'C', 'D'), label_size = 12, align = 'v',
          nrow =2, axis='l')
ggsave(here("figs", 
            "Fig_3_nib.png"), 
       width = 12, height = 8)


## Coral cover ################

## work out which is dominant PRODUCER morphology
dat$pprodmass <- dat$prod_mass/dat$hcprod_mean
dat$pprodenc <- dat$prod_enc/dat$hcprod_mean
dat$pprodbranch <- dat$prod_branchinf/dat$hcprod_mean
dat$propoth<-1-(dat$pprodbranch+dat$pprodenc+dat$pprodmass)

dat$domprod<-ifelse(dat$pprodmass > 0.5, 'M', ifelse(dat$pprodenc >0.5, 'E',
              ifelse(dat$pprodbranch > 0.5, 'B', ifelse(dat$propoth >0.5,'O', 'N'))))

## work out which is dominant cover LHS

dat$domcoverlhs<-ifelse(dat$prod_com > 50, 'C', ifelse(dat$prod_ > 50, 'G',
                                                     ifelse(dat$cover_weedy > 50, 'W',
                                                            ifelse(dat$cover_stress > 50, 'S', 'N'))))

## to do the LHS contribution graph
lhdat<-dat[,c(3,62:65)]
lhdatmelt<-melt(lhdat, id.vars='Site')

a<-summarySE(data=lhdatmelt, measurevar = 'value', groupvars = c('Site', ' variable'))
b<-summarySE(data=dat, measurevar = 'coral_cover', groupvars = c('Site'))
ggplot(a,aes(x = Site, y = value)) + 
  geom_col(aes(fill = variable), position = "fill",stat = "identity", colour = 'black', linetype=1) +
  scale_y_continuous(labels = percent_format(), expand = c(0,0), sec.axis=sec_axis(~./2, name = 'Total Coral Cover (%)', labels=percent_format() ))+

   geom_point(data=b, aes(x=Site, y = coral_cover/50), size=4)+
   geom_errorbar(data=b, aes(x = Site, ymin = (coral_cover-ci)/50, ymax = (coral_cover+ci)/50),
                 width = 0, inherit.aes = F)+
  # geom_boxplot(data=dat, aes(x = Site, y = coral_cover/100), fill = 'lightgrey', colour = 'black',
  #              outlier.shape = NA, alpha = 0.25)+
  # geom_jitter(data =dat, aes(x = Site, y= coral_cover/100), width = 0.15) +
   scale_x_discrete(name = NULL, 
                     labels = c('P.Hantu', 'TPT','P.Djong', 'P.Semakau', 'Sisters',
                                'P.Kusu', 'Raffles', 'All'))+
  scale_fill_fish_d(option = 'Naso_lituratus', labels = c('Competitive', 'Weedy', 'Generalist', 'Stress Tolerant'))+
  theme_minimal() + 
  labs(y = 'Contribution to Coral G (%)')+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = 'black'),
        axis.ticks = element_line(colour = 'black'),
        axis.text.x = element_text(size = 12, angle = 90),
        legend.position = 'top', legend.title = element_blank(),
        legend.spacing.x = unit(0.4, 'cm'),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        axis.text = element_text(colour ='black'))

ggsave(here("figs", 
            "Fig_2A.png"), 
       width = 6, height = 5)

## ============================== ##

## ====   ANALYSIS ==============

## ==============================

##Gnet

## check for normality
qqnorm(subset(dat, Site != 'Mean')$gnet)
qqnorm(log(subset(dat, Site != 'Mean')$gnet +( 1 - min(dat$gnet))))
qqline(log(subset(dat, Site != 'Mean')$gnet +( 1 - min(dat$gnet))))

## transform gnet
dat$tgnet<-log(dat$gnet+(1-min(dat$gnet)))

m1<-aov(tgnet ~ Site, data = subset(dat, Site != 'Mean'))
summary(m1)               
TukeyHSD(m1, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'gnet', groupvars = 'Site')

## Djong is different from TPT, Hantu, Raffles & Semakau at > 0.05, 
## and from Sisters, and Kusu at < 0.1
## Hantu different from Sisters and Kusu at < 0.1

##Hc production

## check for normality
qqnorm(subset(dat, Site != 'Mean')$hcprod_mean)
qqline(subset(dat, Site != 'Mean')$hcprod_mean)

m2<-aov(hcprod_mean ~ Site, data = subset(dat, Site != 'Mean'))
summary(m2)               
TukeyHSD(m2, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'hcprod_mean', groupvars = 'Site')

## Djong is different from TPT, Hantu, Raffles & Semakau at > 0.05, 
## and from Sisters, and Kusu at < 0.1

##CCA production

## check for normality
qqnorm(subset(dat, Site != 'Mean')$ccaprod_mean)
qqnorm(log(subset(dat, Site != 'Mean')$ccaprod_mean +( 1 - min(dat$ccaprod_mean))))
qqline(log(subset(dat, Site != 'Mean')$ccaprod_mean+ ( 1 - min(dat$ccaprod_mean))))

## transform gnet
dat$tcca<-log(dat$ccaprod_mean+(1-min(dat$ccaprod_mean)))

m3<-aov(sqrt(tcca) ~ Site, data = subset(dat, Site != 'Mean'))
summary(m3)               
TukeyHSD(m3, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'ccaprod_mean', groupvars = 'Site')
## total erosion
## check for normality
qqnorm(0-dat$eros_tot)
qqline(0-dat$eros_tot)

m4<-aov(eros_tot ~ Site, data = subset(dat, Site != 'Mean'))
summary(m4)               
TukeyHSD(m4, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'eros_tot', groupvars = 'Site')

## Hantu is different from all but TPT at < 0.05
##  TPT is different from Sisters, Kusu & Raffles
## Djong is different from hantu, kuku & raffles at < 0.05 (Siters at <0.1)
## Semakau is different from Kuku, sisters and raffles
##  Sisters is different from Raffles

qqnorm(sqrt(subset(dat, Site != 'Mean')$coral_cover))
qqline(sqrt(subset(dat, Site != 'Mean')$coral_cover))

m5<-aov(coral_cover ~ Site, data = subset(dat, Site != 'Mean'))
summary(m5)               
TukeyHSD(m5, 'Site', ordered = FALSE, conf.level = 0.95)

qqnorm(sqrt(subset(dat, Site != 'Mean')$rug))
qqline(sqrt(subset(dat, Site != 'Mean')$rug))

summarySE(measurevar = 'coral_cover', groupvars = 'Site', data=dat)
## transform rug
dat$trug<-sqrt(dat$rug)

m6<-aov(trug ~ Site, data = subset(dat, Site != 'Mean'))
summary(m6)               
TukeyHSD(m6, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'rug', groupvars = 'Site')


################ PLAY ######################################

(g<-ggplot(dat, aes(x=coral_cover, y=gnet, colour = domcoverlhs))+geom_point()+stat_smooth(method='lm'))

(ccover<-ggplot(dat,aes(x=sitenum, y=coral_cover, colour = domcoverlhs))+
   # geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -5, ymax =15), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(group=Site)))  +
    geom_jitter( width = 0.15, size = 5) + scale_y_continuous(name = expression(paste('Coral cover (%)'),
                                                    breaks = c(0,20,40,60,80), limits = c(0,80),
                                                    expand=c(0,0))+
    scale_x_continuous(name = " ",breaks = c(1,2,3,4,5,6,7,8), labels = NULL)+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black')))



m1<-lm(eros_tot ~ dist_port  , data = subset(dat, Site != 'Mean'))
m1
summary(m1)               
TukeyHSD(m1, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'coral_cover', groupvars = 'Site')

colnames(dat2)[max.col(dat2, ties.method = 'first')]
dat$domlhs<-colnames(dat2)[max.col(dat2, ties.method = 'first')]

datnom<-subset(dat, Site != "Mean")

(g<-ggplot(datnom, aes(x=rug, y=gnet))+geom_point()+stat_smooth(method='lm'))
g<-g + geom_point(aes(x = cca_cover, y = gnet), shape = 22, colour = 'red')+stat_smooth(method='lm')

(ccover<-ggplot(dat,aes(x=sitenum, y=coral_cover, colour = domcoverlhs))+
    # geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -5, ymax =15), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(group=Site)))  +
  geom_jitter( width = 0.15, size = 5) + scale_y_continuous(name = expression(paste('Coral cover (%)'),
                                                                              breaks = c(0,20,40,60,80), limits = c(0,80),
                                                                              expand=c(0,0))+
                                                              scale_x_continuous(name = " ",breaks = c(1,2,3,4,5,6,7,8), labels = NULL)+
                                                              theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
                                                              theme(panel.grid = element_blank(),
                                                                    panel.background = element_rect(fill = NA, colour = NA),
                                                                    axis.line = element_line(colour = 'black'),
                                                                    axis.ticks = element_line(colour = 'black')))



