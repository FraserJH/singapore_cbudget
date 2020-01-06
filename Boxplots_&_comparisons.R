## ==============================
## boxplot scripts ==
## ==============================

## load libraries
library(ggplot2)
library(cowplot)
library(here)
library(reshape2)
library(Rmisc)
library(fishualize)
library(scales)
library(ggsci)
library(devtools)
library(fishualize)

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
dat<-read.csv('data/summarydat.csv')
head(dat)
dat<-transform(dat, Site = reorder(Site, Long)) ## to order west to east
dat$sitenum <- as.numeric(dat$Site)

## =====================================
## ===      Summary Box plots        ===
## =====================================

## Gnet
(gnet<-ggplot(dat,aes(x=sitenum, y=gnet, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -4, ymax =8), fill='lightgrey')+
    geom_hline(yintercept = 1.41, linetype =1,size=2,
      colour = 'grey50' )+ ## IP mean (from Perry et al. 2018 Nature)
    geom_hline(yintercept = 4.78, linetype =1,size=2, 
      colour = 'grey90' )+ ## IP max (from Perry et al. 2018 Nature)
    geom_hline(yintercept = -3.1, linetype =1,size=2, 
      colour = 'grey90' )+ ## IP min (from Perry et al. 2018 Nature)
    geom_boxplot(outlier.shape=NA, aes(group = Site, fill = NULL),width = 0.5) + 
    geom_jitter( width = 0.15) + 
    scale_y_continuous(name =  expression(paste('CCA G (kg CaCO'[3],
                                              ' m'^-2, ' yr'^-1, ')')),
      breaks = c(-4.0,-2.0,0.0,2.0,4.0,6.0,8.0,10.0), limits = c(-4,8),
      labels = c('-4.0','-2.0', '0.0','2.0','4.0','6.0','8.0','10.0'),
      expand = c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal(base_family = 'sans')+
    geom_vline(xintercept = 7.5, linetype=2)+
    geom_hline(yintercept=0, linetype = 2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_text(size = 8),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'),
      legend.position = 'none'))

## HC prod
(hcprod<-ggplot(dat,aes(x=sitenum, y=hcprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =10), fill='lightgrey')+
    geom_hline(yintercept = 3.8, linetype =1,size=2, 
      colour = 'grey50'  )+ #IP mean calculated from Perry et al. 2018
    geom_hline(yintercept = 8.4, linetype =1,size=2, 
      colour = 'grey90' )+ #IP max calculated from Perry et al. 2015
    geom_hline(yintercept = 1.0, linetype =1,size=2, 
      colour = 'grey90' )+ #IP min calculated from Perry et al. 2018
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site),width = 0.5) + 
    scale_fill_grey() +
    geom_jitter( width = 0.15) + scale_y_continuous(name = NULL,
      breaks = c(0.0,2.0,4.0,6.0,8.0,10.0,11), limits = c(0,10),
      labels = c('0.0','2.0','4.0','6.0','8.0','10.0', ''),expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'),
      axis.text = element_text(size = 8), legend.position = 'none'))


# ## cca
(cca<-ggplot(dat,aes(x=sitenum, y=ccaprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =0.25), 
      fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+
    scale_fill_grey() +
    geom_jitter( width = 0.15) + 
    scale_y_continuous(name = expression(paste('CCA G (kg CaCO'[3],' m'^-2,
                                               ' yr'^-1, ')')),
      breaks = c(0,0.05,0.10,0.15,0.20,0.25), limits = c(0,0.25),
      expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'), legend.position = 'none'))

# ## Erosion
erosdat<-dat[,c(3,12:13,22,39,41)]
erosdat$int<-erosdat$macberos_mean + erosdat$micberos
erosdat$eros_tot<-0-erosdat$eros_tot
(eros<-ggplot(dat,aes(x=sitenum, y=0-eros_tot, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =10), fill='lightgrey')+
    geom_hline(yintercept = 2.8, linetype =1,size=2,
      colour = 'grey50' )+ #IP mean calc from Perry et al. 2018
    geom_hline(yintercept = 1.2, linetype =1,size=2, 
      colour = 'grey90' )+ #IP min from Perry et al. 2018
    geom_hline(yintercept = 5.0, linetype =1,size=2, 
      colour = 'grey90' )+ #IP max site from Perry et al. 2018
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    scale_fill_grey() +
    geom_jitter( width = 0.15) + 
    scale_y_continuous(name = NULL, breaks = c(-4,-2,0,2,4,6,8,10,11),
      limits = c(0,10),
      labels = c(-4.0,-2.0,'0.0','2.0','4.0','6.0','8.0','10.0',''),
      expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ', 'SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'), 
      axis.text = element_text(size = 8), legend.position = 'none'))

cowplot::plot_grid(gnet, hcprod,  eros, labels = c('(a)', '(b)', '(c)'), 
                   label_size = 12, align = 'v',
          nrow =1, axis='l')

ggsave(here("figs", 
            "Figure_2.eps"), device='eps', family = 'sans',
       width = 9, height = 4)

ggsave(here("figs", 
            "Figure_2.png"), 
       width = 9, height = 4)

######## benthic cover 

(coral<-ggplot(dat,aes(x=sitenum, y=coral_cover, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =50), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'Coral cover (%)',
      breaks = c(0,10,20,30,40,50,60,70,80), limits = c(0,50),
      labels = c('0', '10', '20','30','40','50','60','70','80'),
      expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'),
      axis.text = element_text(size = 8), legend.position = 'none'))

(ccacover<-ggplot(dat,aes(x=sitenum, y=cca_cover, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =25), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'Coral cover (%)',
      breaks = c(0,5,10,15,20,25), limits = c(0,25),
      labels = c('0', '5', '10','15','20','25'),expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'),
      axis.text = element_text(size = 8), legend.position = 'none'))

(mac<-ggplot(dat,aes(x=sitenum, y=(cca_cover), group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =30), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    scale_fill_grey() +
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'Algal cover (%)',
      breaks = c(0,10,20,30,40,50,60), limits = c(0,30),
      labels = c('0', '10', '20','30','40','50','60'),expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'), 
      axis.text = element_text(size = 8), legend.position = 'none'))

(turf<-ggplot(dat,aes(x=sitenum, y=(turf_cover), group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =50), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    scale_fill_grey() +
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'Algal cover %',
      breaks = c(0,10,20,30,40,50,60), limits = c(0,50),
      labels = c('0', '10', '20','30','40','50','60'),expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'), 
      axis.text = element_text(size = 8), legend.position = 'none'))

## genera nuimbers - unused
# (gen<-ggplot(dat,aes(x=sitenum, y=(rug ), group = Site))+
#   geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =5), fill='lightgrey')+
#   geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
#   scale_fill_grey() +
#   geom_jitter( width = 0.15) + scale_y_continuous(name = 'Number of genera',
#     breaks = c(0,5,10,15,20), limits = c(0,5),
#     labels = c('0','5', '10','15', '20'),expand=c(0,0))+
#   scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
#     labels = c('PH', 'TPT','PJ', 'PSem', 'SIS','K', 'PSat', 'All'))+
#   theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
#   geom_hline(yintercept = 0, linetype = 2)+
#   theme(panel.grid = element_blank(),
#     panel.background = element_rect(fill = NA, colour = NA),
#     axis.text = element_text(size = 8),
#     axis.line = element_line(colour = 'black'),
#     axis.ticks = element_line(colour = 'black'), legend.position = 'none'))

## rugosity
(rug<-ggplot(dat,aes(x=sitenum, y=(rug ), group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 1, ymax =4), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    scale_fill_grey() +
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'Rugosity',
      breaks = c(1,2,3,4), limits = c(1,4),
      labels = c('1', '2','3', '4'),expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),axis.text = element_text(size = 8),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'), 
          legend.position = 'none'))

cowplot::plot_grid(coral, ccacover, rug,turf, mac, labels = c('(a)', '(b)', '(c)', '(d)','(e)'), 
                   label_x = -0.03,
          label_size = 12, align = 'v', nrow =3, axis='l')

ggsave(here("figs", 
            "Fig_S1.png"), 
       width = 6, height = 7.5)
ggsave(here("figs", 
            "Fig_S1.eps"), device = 'eps', family ='sans', 
       width = 6, height = 7.5)

cowplot::plot_grid(cca, 
          label_size = 12, align = 'v', nrow =1, axis='l')

ggsave(here("figs", 
            "Fig_S2.png"), 
       width = 3, height = 2.5)
ggsave(here("figs", 
            "Fig_S2.eps"), device = 'eps', family ='sans', 
       width = 3, height = 2.5)

## LHS DOMINANCE PRODUCTION ####

## work out which is dominant PRODUCER morphology
dat$pprodmass <- dat$prod_mass/dat$hcprod_mean
dat$pprodenc <- dat$prod_enc/dat$hcprod_mean
dat$pprodbranch <- dat$prod_branchin/dat$hcprod_mean
dat$propoth<-1-(dat$pprodbranch+dat$pprodenc+dat$pprodmass)

dat$domprod<-ifelse(dat$pprodmass > 0.5, 'M', ifelse(dat$pprodenc >0.5, 'E',
              ifelse(dat$pprodbranch > 0.5, 'B', 
                ifelse(dat$propoth >0.5,'O', 'N'))))

## work out which is dominant PRODUCER LHS

dat$domcoverlhs<-ifelse(dat$prod_com > 50, 'C', 
                  ifelse(dat$cover_stress > 50, 'G',
                    ifelse(dat$prod_ > 50, 'W',
                      ifelse(dat$cover_stress > 50, 'S', 'N'))))
dat$dom<-colnames(dat[,c(32:35)])[max.col(dat[,c(32:35)],
                                                      ties.method="first")]
## to do the LHS contribution graph
lhdat<-dat[,c(3,62:65)]
lhdatmelt<-melt(lhdat, id.vars='Site')

a<-summarySE(data=lhdatmelt, measurevar = 'value', 
             groupvars = c('Site', ' variable'))
b<-summarySE(data=dat, measurevar = 'hcprod_mean', groupvars = c('Site'))

(lhs<-ggplot(a,aes(x = Site, y = value)) + 
  geom_col(aes(fill = variable), position = "fill",stat = "identity", 
            colour = 'black', linetype=1)+
  scale_y_continuous(labels = percent_format(), expand = c(0,0), 
            sec.axis = sec_axis(~./8, 
            breaks=c(0,0.125/8, 2*0.125/8, 3*0.125/8,
                      4*0.125/8, 5*0.125/8, 6*0.125/8,
                      7*0.125/8, 0.125), 
            labels=c('0','1','2','3','4','5','6','7','8'),
            name = expression(paste("HC production (kg CaCO"[3],
                                    " m"^-2, " yr"^-1,")"))))+
  geom_errorbar(data=b, aes(x = Site, ymin = (hcprod_mean-ci)/8, 
                ymax = (hcprod_mean+ci)/8),
                width = 0, inherit.aes = F, colour ='grey50')+
  geom_point(data=b, aes(x=Site, y = (hcprod_mean)/8), size=4, pch = 21,
            fill = 'white')+
   scale_x_discrete(name = NULL, 
            labels = c('TPT', 'P. Satumu','P. Hantu', 'P. Semakau', 'P. Jong',
                                'Sisters', 'Kusu', 'All'))+
  scale_fill_fish_d(option = 'Naso_lituratus', 
          labels = c('Competitive', 'Weedy', 'Generalist', 'Stress Tolerant'))+
  theme_minimal() + 
  labs(y = 'Contribution to Coral G (%)')+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = 'black'),
        axis.ticks = element_line(colour = 'black'),
        axis.text.x = element_text(size = 12, angle = 90),
        legend.position = 'top', legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        axis.text = element_text(colour ='black')))


lform<-dat[,c(3,68:71)]
lformmelt<-melt(lform, id.vars='Site')
c<-summarySE(data=lformmelt, measurevar = 'value', groupvars = c('Site', 
                                                                 ' variable'))
d<-summarySE(data=dat, measurevar = 'coral_cover', groupvars = c('Site'))
morph<-ggplot(c,aes(x = Site, y = value)) + 
  geom_col(aes(fill = variable), position = "fill",stat = "identity", 
          colour = 'black', linetype=1) +
  scale_y_continuous(labels = percent_format(), expand = c(0,0), 
          sec.axis = sec_axis(~./8, 
          breaks=c(0,0.125/8, 2*0.125/8, 3*0.125/8, 4*0.125/8, 5*0.125/8,
                   6*0.125/8, 7*0.125/8, 0.125), 
          labels=c('0','1','2','3','4','5','6','7','8'),
          name = expression(paste("HC production (kg CaCO"[3],
                                  " m"^-2, " yr"^-1,")"))))+
  geom_errorbar(data=b, aes(x = Site, ymin = (hcprod_mean-ci)/8, 
                ymax = (hcprod_mean+ci)/8),
                width = 0, inherit.aes = F, colour = 'grey50')+
  geom_point(data=b, aes(x=Site, y = hcprod_mean/8), size=4, pch = 21, 
            fill = 'white')+
  scale_x_discrete(name = NULL, 
            labels = c('TPT', 'P. Satumu','P. Hantu', 'P. Semakau', 'P. Jong',
                              'Sisters', 'Kusu', 'All'))+
  scale_fill_fish_d(option = 'Naso_lituratus', 
                    labels = c('Massive', 'Encrusting', 'Branching', 'Other'))+
  theme_minimal() + 
  labs(y = 'Contribution to Coral G (%)')+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = 'black'),
        axis.ticks = element_line(colour = 'black'),
        axis.text.x = element_text(size = 12, angle = 90),
        legend.position = 'top', legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        axis.text = element_text(colour ='black'))

cowplot::plot_grid(morph, lhs, labels = c('(a)', '(b)'), label_size = 12, align = 'v',
          nrow =2, axis='l')
ggsave(here::here("figs", 
            "Figure_5.png"), 
       width = 5, height = 8)
ggsave(here::here("figs", 
            "Fig_5.eps"), family = 'sans', device = 'eps', 
       width = 5, height = 8)


## contribution of erosion
erosmelt<-melt(erosdat[c(1,4,5,7)], id.vars='Site')

e<-summarySE(data=erosmelt, measurevar = 'value', 
             groupvars = c('Site', ' variable'))
f<-summarySE(data=erosdat, measurevar = 'eros_tot', groupvars = c('Site'))
(ero<-ggplot(e,aes(x = Site, y = value)) + 
  geom_col(aes(fill = variable), position = "fill",stat = "identity", 
           colour = 'black', linetype=1) +
  scale_y_continuous(labels = percent_format(), expand = c(0,0), 
                    sec.axis=sec_axis(~./4.5, 
                    breaks = c(0,0.05,0.1,0.15,0.20,0.222), 
                    labels = c('0.0','1.0','2.0','3.0','4.0',''), 
                    name = expression(paste("Bioerosion (kg CaCO"[3],
                                            " m"^-2, " yr"^-1,")"))))+
  geom_errorbar(data=f, aes(x = Site, ymin = (eros_tot-ci)/4.5, 
                ymax = (eros_tot+ci)/4.5), width = 0, inherit.aes = F)+
  geom_point(data=f, aes(x=Site, y = eros_tot/4.5), size=4, pch = 21, 
             fill = 'white')+
  scale_x_discrete(name = NULL, 
              labels = c('TPT', 'P. Satumu','P. Hantu', 'P. Semakau', 'P. Jong',
                              'Sisters', 'Kusu', 'All'))+
  scale_fill_fish_d(option = 'Naso_lituratus', 
                    labels = c('Urchin', 'Fish', 'Internal'))+
  theme_minimal() + 
  labs(y = 'Contribution to Bioerosion (%)')+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = 'black'),
        axis.ticks = element_line(colour = 'black'),
        axis.text.x = element_text(size = 12, angle = 90),
        legend.position = 'top', legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'),
        legend.key.height = unit(0.2, 'cm'),
        legend.key.width = unit(0.8, 'cm'),
        axis.text = element_text(colour ='black')))

ggsave(here::here("figs", 
            "Fig_S3.png"), 
       width = 5, height = 4)

ggsave(here::here("figs", 
            "Fig_S3.eps"), device = 'eps', family = 'sans',
       width = 5, height = 4)

## ============================== ##

## ====   ANALYSIS ==============

## ==============================

### NET CARBONATE BUDGET ###

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


### HARD CORAL PRODUCTION ###

## check for normality
qqnorm(subset(dat, Site != 'Mean')$hcprod_mean)
qqline(subset(dat, Site != 'Mean')$hcprod_mean)

m2<-aov(hcprod_mean ~ Site, data = subset(dat, Site != 'Mean'))
summary(m2)               
TukeyHSD(m2, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'hcprod_mean', groupvars = 'Site')

### CCA PRODUCTION ###

## check for normality
qqnorm(subset(dat, Site != 'Mean')$ccaprod_mean)
qqnorm(sqrt(subset(dat, Site != 'Mean')$ccaprod_mean +( 1 - min(dat$ccaprod_mean))))
qqline(log(subset(dat, Site != 'Mean')$ccaprod_mean+ ( 1 - min(dat$ccaprod_mean))))

## can't transfrom - to Kruskal Wallis

m3<-kruskal.test(ccaprod_mean ~ Site, data = subset(dat, Site != 'Mean'))
print(m3)               

dat1<-subset(dat, Site != 'Mean')
PT = pairwise.wilcox.test(dat1$ccaprod_mean,dat1$Site,  p.adjust.method = 'none')
summarySE(data=dat, measurevar = 'ccaprod_mean', groupvars = 'Site')

### TOTAL EROSION ###
## check for normality
qqnorm(log(0-dat$eros_tot))
qqline(log(0-dat$eros_tot))

dat1<-subset(dat, Site != 'Mean')
m4<-aov(log(0-eros_tot) ~ Site, data = dat1)
summary(m4)               
TukeyHSD(m4, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'eros_tot', groupvars = 'Site')

### RUGOSITY ###

qqnorm(sqrt(subset(dat, Site != 'Mean')$rug))
qqline(sqrt(subset(dat, Site != 'Mean')$rug))


## transform rug
dat$trug<-sqrt(dat$rug)

m6<-aov(trug ~ Site, data = subset(dat, Site != 'Mean'))
summary(m6)               
TukeyHSD(m6, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'rug', groupvars = 'Site')


##coral cover
qqnorm(dat$coral_cover)
qqline(dat$coral_cover)

m7<-aov(coral_cover ~ Site, data = subset(dat, Site != 'Mean'))
summary(m7)               
TukeyHSD(m7, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'coral_cover', groupvars = 'Site')


##turf
qqnorm(dat$turf_cover)
qqline(dat$turf_cover)

qqnorm(sqrt(dat$turf_cover))
qqline(sqrt(dat$turf_cover))

m8<-aov(sqrt(turf_cover) ~ Site, data = subset(dat, Site != 'Mean'))
summary(m8)               
TukeyHSD(m8, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'turf_cover', groupvars = 'Site')

qqnorm(dat$mac_cover)
qqline(dat$mac_cover)
m8<-aov(sqrt(mac_cover) ~ Site, data = subset(dat, Site != 'Mean'))
summary(m8)               
TukeyHSD(m8, 'Site', ordered = FALSE, conf.level = 0.95)

summarySE(data=dat, measurevar = 'mac_cover', groupvars = 'Site')


