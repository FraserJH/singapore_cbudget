## ==============================
## boxplot scripts ==
## ==============================

## load libraries
library(ggplot2)
library(cowplot)
library(lme4)
library(sjPlot)
library(Rmisc)
library(lmerTest)
library(mosaic)
library(tidyverse)
library(psycho)
library(mosaic)
library(here)
library(Rmisc)
library(RColorBrewer)
library(reshape2)
library(scales)
library(ggsci)
## read data
dat<-read.csv('summarydat.csv')
head(dat)
dat<-transform(dat, Site = reorder(Site, dist_port))
dat$sitenum <- as.numeric(dat$Site)
dat<-dat[-c(43,86),]
## ==============================
## ===       Box plots        ===
## ==============================

## Gnet
(gnet<-ggplot(dat,aes(x=sitenum, y=gnet, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -5, ymax =12), fill='lightgrey')+
  geom_boxplot(outlier.shape=NA) + 
  geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('Gnet (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                  breaks = c(-5.0,0.0,5.0,10.0,12), limits = c(-5,12),
                                                  labels = c('-5.0','0.0','5.0','10.0',''),
                                                  expand=c(0,0))+
   scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
                      labels = c('P.Hantu', 'TPT','P.Djong', 'P.Semakau', 'Sisters',
                                 'P.Kusu', 'Raffles', 'All'))+
   theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
   theme(panel.grid = element_blank(),
         panel.background = element_rect(fill = NA, colour = NA),
         axis.line = element_line(colour = 'black'),
         axis.ticks = element_line(colour = 'black')))

## HC prod
(hcprod<-ggplot(dat,aes(x=sitenum, y=hcprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =12), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA) + 
    geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('Coral G (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                    breaks = c(0.0,5.0,10.0,12), limits = c(0,12),
                                                    labels = c('0.0','5.0','10.0', ''),expand=c(0,0))+
    scale_x_continuous(name = " ",breaks = c(1,2,3,4,5,6,7,8), 
                       labels = NULL)+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black')))


## cca
(cca<-ggplot(dat,aes(x=sitenum, y=ccaprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =0.2), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA) + 
    geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('CCA G (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                    breaks = c(0,0.05,0.10,0.15,0.20), limits = c(0,0.20),
                                                    expand=c(0,0))+
    scale_x_continuous(name = " ",breaks = c(1,2,3,4,5,6,7,8), 
                       labels = NULL)+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black')))



## Erosion


(eros<-ggplot(dat,aes(x=sitenum, y=eros_tot, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -3.2, ymax =0), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA) + 
    geom_jitter( width = 0.15) + scale_y_continuous(name = expression(paste('Erosion (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
                                                    breaks = c(0,-1,-2,-3,-3.2), limits = c(-3.2,0),
                                                    labels = c('0.0','-1.0','-2.0','-3.0',''),expand=c(0,0))+
    scale_x_continuous(name = " ",breaks = c(1,2,3,4,5,6,7,8), 
                       labels = NULL)+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'),
          axis.text.x = element_text(size = 12, angle = 90)))


plot_grid(hcprod, eros,  gnet,  align = 'v', nrow=3, rel_heights = 
            c(5/16,5/16,6/16))

ggsave(here("figs", 
            "Fig_2.png"), 
       width = 6, height = 12)

## Coral cover ################

## work out which is dominant PRODUCER morphology
dat$pprodmass <- dat$prod_mass/dat$hcprod_mean
dat$pprodenc <- dat$prod_enc/dat$hcprod_mean
dat$pprodbranch <- dat$prod_branchinf/dat$hcprod_mean
dat$propoth<-1-(dat$pprodbranch+dat$pprodenc+dat$pprodmass)

dat$domprod<-ifelse(dat$pprodmass > 0.5, 'M', ifelse(dat$pprodenc >0.5, 'E',
              ifelse(dat$pprodbranch > 0.5, 'B', ifelse(dat$propoth >0.5,'O', 'N'))))

## work out which is dominant cover LHS

dat$domcoverlhs<-ifelse(dat$cover_comp > 50, 'C', ifelse(dat$cover_general > 50, 'G',
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
  scale_fill_npg(labels = c('Competitive', 'Weedy', 'Generalist', 'Stress Tolerant'))+
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
            "Fig_3A.png"), 
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

## total erosion
## check for normality
qqnorm(sqrt(subset(dat, Site != 'Mean')$eros_tot+( 1 - min(dat$eros_tot))))
qqline(subset(dat, Site != 'Mean')$eros_tot+( 1 - min(dat$eros_tot)))

m4<-aov(eros_tot ~ Site, data = subset(dat, Site != 'Mean'))
summary(m4)               
TukeyHSD(m4, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'eros_tot', groupvars = 'Site')

## Hantu is different from all but TPT at < 0.05
##  TPT is different from Sisters, Kusu & Raffles
## Djong is different from hantu, kuku & raffles at < 0.05 (Siters at <0.1)
## Semakau is different from Kuku, sisters and raffles
##  Sisters is different from Raffles

qqnorm(subset(dat, Site != 'Mean')$coral_cover)
qqline(subset(dat, Site != 'Mean')$coral_cover)

m5<-aov(coral_cover ~ Site, data = subset(dat, Site != 'Mean'))
summary(m5)               
TukeyHSD(m5, 'Site', ordered = FALSE, conf.level = 0.95)


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

