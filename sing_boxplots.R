## ==============================
## boxplot scripts ==
## ==============================

## load libraries
library(ggplot2)
library(lme4)
library(sjPlot)
library(Rmisc)
library(lmerTest)
require(mosaic)
library(tidyverse)
library(psycho)

## read data
dat<-read.csv('summarydat.csv')
head(dat)
dat<-transform(dat, Site = reorder(Site, dist_port))

## ==============================
## ===       Box plots        ===
## ==============================
## Gnet
(gnet<-ggplot(dat,aes(x=Site, y=gnet))+geom_boxplot(outlier.shape=NA) + 
  geom_jitter( width = 0.15) + theme_bw())

## HC prod
(hcprod<-ggplot(dat,aes(x=Site, y=hcprod_mean))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw())

## cca
(cca<-ggplot(dat,aes(x=Site, y=ccaprod_mean))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw())

## Erosion
(eros<-ggplot(dat,aes(x=Site, y=eros_tot))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw()) + ylim(c(-4.5,0))

## Coral cover
(c_cover<-ggplot(dat,aes(x=Site, y=coral_cover))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw())
(c_coverw<-ggplot(dat,aes(x=Site, y=cover_general))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw())
(c_cover<-ggplot(dat,aes(x=Site, y=cover_stress))+geom_boxplot() + 
    geom_jitter( width = 0.15) + theme_bw())

## dot plots
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=coral_cover, y=gnet, colour = Site)) + geom_point()) 
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=cover_general/100*coral_cover, y=gnet, colour = Site))
  + geom_point()) 
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=cover_stress/100*coral_cover, y=hcprod_mean, colour = Site))
  + geom_point()) 
(cov_prod <- ggplot(subset(dat, Site != 'Mean'), aes(x=cover_weedy/100*coral_cover, y=hcprod_mean, colour = Site))
  + geom_point())



## contribution of functional coral groups
p<-ggplot(data = dat, aes(x = Site, y = ))


