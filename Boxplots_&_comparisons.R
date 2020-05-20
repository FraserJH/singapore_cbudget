## ========================================
## boxplots, barplots & analysis scripts ==
## ========================================
.libPaths("~/Users/Dropbox/R-library")
## load libraries
library(ggplot2)
library(cowplot)
library(here)
library(magrittr)
library(fishualize)
library(scales)
library(ggsci)
library(devtools)
library(plotrix)
library(lme4)
library(lmerTest)
library(mosaic)
library(tidyverse)
library(psycho)
library(reshape2)

## read data
dat<-read.csv('data/summarydat_sing.csv')
head(dat)
dat<-transform(dat, Site = reorder(Site, Long)) ## to order west to east
dat$sitenum <- as.numeric(dat$Site)

## =======================================================
## ===      Summary Box plots - carbonate budget       ===
## =======================================================

####### GNET #######
(gnet<-ggplot(dat,aes(x=sitenum, y=gnet, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -4, ymax =8), fill='lightgrey')+
    geom_hline(yintercept = 1.16, linetype =1,size=2,
      colour = 'grey50' )+ ## IP mean (from Perry et al. 2018 Nature)
    geom_hline(yintercept = 5.91, linetype =1,size=2, 
      colour = 'grey90' )+ ## IP max (from Perry et al. 2018 Nature)
    geom_hline(yintercept = -3.0, linetype =1,size=2, 
      colour = 'grey90' )+ ## IP min (from Perry et al. 2018 Nature)
    geom_boxplot(outlier.shape=NA, aes(group = Site, fill = NULL),width = 0.5) + 
    geom_jitter( width = 0.15) + 
    scale_y_continuous(name =  expression(paste('kg CaCO'[3], ' m'^-2, ' yr'^-1)),
      breaks = c(-4.0,-2.0,0.0,2.0,4.0,6.0,8.0,10.0), limits = c(-4,8),
      labels = c('-4.0','-2.0', '0.0','2.0','4.0','6.0','8.0','10.0'),
      expand = c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal(base_family = 'sans')+ geom_vline(xintercept = 7.5, linetype=2)+
    geom_hline(yintercept=0, linetype = 2)+theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.text = element_text(size = 8),axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'), legend.position = 'none'))

#### means and standard error
dat %>% 
 dplyr:: group_by(Site) %>% 
  dplyr::summarise(mean=mean(gnet), std.err = std.error(gnet))

####### HC production #######
(hcprod<-ggplot(dat,aes(x=sitenum, y=hcprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =10), fill='lightgrey')+
    geom_hline(yintercept = 4, linetype =1,size=2, 
      colour = 'grey50'  )+ #IP mean calculated from Perry et al. 2018
    geom_hline(yintercept = 7.8, linetype =1,size=2, 
      colour = 'grey90' )+ #IP max calculated from Perry et al. 2015
    geom_hline(yintercept = 1.46, linetype =1,size=2, 
      colour = 'grey90' )+
    geom_hline(yintercept=0, linetype = 2)+ #IP min calculated from Perry et al. 2018
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site),width = 0.5) + 
    scale_fill_grey() + geom_jitter( width = 0.15) + 
    scale_y_continuous(name =  expression(paste('kg CaCO'[3], ' m'^-2, ' yr'^-1)),
      breaks = c(-4.0,-2.0,0.0,2.0,4.0,6.0,8.0,10.0,11), limits = c(0,10),
      labels = c('-4.0', '-2.0', '0.0','2.0','4.0','6.0','8.0','10.0', ''),
      expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(), panel.background = element_rect(fill = NA,
      colour = NA), axis.line = element_line(colour = 'black'), 
      axis.ticks = element_line(colour = 'black'),
      axis.text = element_text(size = 8), legend.position = 'none'))

#### cca production #####
(cca<-ggplot(dat,aes(x=sitenum, y=ccaprod_mean, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =0.50), 
      fill='lightgrey')+
    geom_hline(yintercept = 0.16, linetype =1,size=2, 
               colour = 'grey50'  )+ #IP mean calculated from Perry et al. 2018
    geom_hline(yintercept = 7.8, linetype =1,size=2, 
               colour = 'grey90' )+ #IP max calculated from Perry et al. 2015
    geom_hline(yintercept = 0.05, linetype =1,size=2, 
               colour = 'grey90' )+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+
    scale_fill_grey() +geom_jitter( width = 0.15) + 
    scale_y_continuous(name = expression(paste('CCA G (kg CaCO'[3],' m'^-2,
                                               ' yr'^-1, ')')),
      breaks = c(0,0.05,0.10,0.15,0.20,0.25), limits = c(0,0.25), expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'), legend.position = 'none'))

#### Erosion
erosdat<-dat[,c(3,12:13,22,39,41)]
erosdat$int<-erosdat$macberos_mean + erosdat$micberos
erosdat$eros_tot<-0-(erosdat$int + erosdat$eros_pfish +erosdat$eros_urch)
(eros<-ggplot(dat,aes(x=sitenum, y=0-eros_tot, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = -6, ymax =2), fill='lightgrey')+
    geom_hline(yintercept = -3.0, linetype =1,size=2,
      colour = 'grey50' )+ #IP mean calc from Perry et al. 2018
    geom_hline(yintercept = -5.3, linetype =1,size=2, 
      colour = 'grey90' )+ #IP min from Perry et al. 2018
    geom_hline(yintercept = -1.21, linetype =1,size=2, 
      colour = 'grey90' )+ #IP max site from Perry et al. 2018
    geom_hline(yintercept=0, linetype = 2)+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    scale_fill_grey() +geom_jitter( width = 0.15) + 
    scale_y_continuous(name =  expression(paste('kg CaCO'[3],
                                                ' m'^-2, ' yr'^-1, )),
                       breaks = c(-6,-4,-2,0,2,4,6,8,10,11),
      limits = c(-6,2),
      labels = c('-6.0', '-4.0','-2.0','0.0','2.0','4.0','6.0','8.0','10.0',''),
      expand=c(0.01,0))+
    scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8), 
      labels = c('TPT', 'PSat','PH', 'PSem', 'PJ', 'SIS', 'K', 'All'))+
    theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
    theme(panel.grid = element_blank(),
      panel.background = element_rect(fill = NA, colour = NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks = element_line(colour = 'black'), 
      axis.text = element_text(size = 8), legend.position = 'none'))

### use cowplot to put them together
cowplot::plot_grid(hcprod,  eros, gnet, labels = c('(a)', '(b)', '(c)'), 
                   label_size = 12, align = 'v',
          nrow =3, axis='l')

#### eps for submission, png for preview
ggsave(here::here("figs_s", 
            "Figure_2.eps"), device='eps', family = 'sans',
       width = 3, height = 9)

ggsave(here::here("figs_s", 
            "Figure_2.png"), 
       width = 3, height = 9)

cowplot::plot_grid(cca, 
                   label_size = 12, align = 'v', nrow =1, axis='l')

ggsave(here::here("figs_s", 
                  "Figure_S3.png"), 
       width = 3, height = 2.5)
ggsave(here::here("figs_s", 
                  "Figure_S3.eps"), device = 'eps', family ='sans', 
       width = 3, height = 2.5)

## =======================================================
## ===      Summary Box plots - benthic cover       ===
## =======================================================

### hard coral cover
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

#### cca cover
(ccacover<-ggplot(dat,aes(x=sitenum, y=cca_cover, group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =25), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'CCA cover (%)',
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

#### macroalgae
(mac<-ggplot(dat,aes(x=sitenum, y=(mac_cover), group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =30), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    scale_fill_grey() +
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'Macroalgal cover (%)',
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

#####EAM cover ####
(turf<-ggplot(dat,aes(x=sitenum, y=(turf_cover), group = Site))+
    geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =50), fill='lightgrey')+
    geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+ 
    scale_fill_grey() +
    geom_jitter( width = 0.15) + scale_y_continuous(name = 'EAM cover %',
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

## genera numbers - 
(gen<-ggplot(dat,aes(x=sitenum, y=(numb_gen ), group = Site))+
  geom_rect(aes(xmin = 7.5, xmax = 8.5,ymin = 0, ymax =5), fill='lightgrey')+
  geom_boxplot(outlier.shape=NA, aes(fill = NULL, group = Site), width = 0.5)+
  scale_fill_grey() +
  geom_jitter( width = 0.15) + scale_y_continuous(name = 'Coral genera #',
    breaks = c(0,5,10,15,20), limits = c(0,20),
    labels = c('0','5', '10','15', '20'),expand=c(0,0))+
  scale_x_continuous(name = NULL,breaks = c(1,2,3,4,5,6,7,8),
    labels = c('TPT', 'PSat','PH', 'PSem', 'PJ','SIS', 'K', 'All'))+
  theme_minimal() + geom_vline(xintercept = 7.5, linetype=2)+
  geom_hline(yintercept = 0, linetype = 2)+
  theme(panel.grid = element_blank(),
    panel.background = element_rect(fill = NA, colour = NA),
    axis.text = element_text(size = 8),
    axis.line = element_line(colour = 'black'),
    axis.ticks = element_line(colour = 'black'), legend.position = 'none'))

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
          panel.background = element_rect(fill = NA, colour = NA),
          axis.text = element_text(size = 8),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black'), 
          legend.position = 'none'))

cowplot::plot_grid(coral, ccacover, rug,turf, mac, gen, 
                   labels = c('(a)', '(b)', '(c)', '(d)','(e)', '(d)'), 
                   label_x = -0.03,
          label_size = 12, align = 'v', nrow =3, axis='l')

ggsave(here::here("figs_s", 
            "Fig_S2.png"), 
       width = 6, height = 7.5)
ggsave(here::here("figs_s", 
            "Fig_S2.eps"), device = 'eps', family ='sans', 
       width = 6, height = 7.5)

## ======================================================================
## ===       Proportion carbonate production - carbonate budget       ===
## ======================================================================
dat$pprodmass <- dat$prod_mass/dat$hcprod_mean
dat$pprodenc <- dat$prod_enc/dat$hcprod_mean
dat$pprodbranch <- dat$prod_branchin/dat$hcprod_mean
dat$propoth<-1-(dat$pprodbranch+dat$pprodenc+dat$pprodmass)
## work out which is dominant PRODUCER MORPHOLOGY

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

a <- lhdatmelt %>% 
  group_by(Site,variable) %>% summarise(mean = mean(value))
b <- dat %>% 
  group_by(Site) %>% 
  summarise(mean = mean(hcprod_mean), ssd = sd(hcprod_mean), count = n()) %>% 
  mutate(se = ssd /sqrt(count), ci = qt(1-(0.05/2), count-1) * se)

(lhs<-ggplot(a,aes(x = Site, y = mean)) + 
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
  geom_errorbar(data=b, aes(x = Site, ymin = (mean-ci)/8, 
                ymax = (mean+ci)/8),
                width = 0, inherit.aes = F, colour ='grey50')+
  geom_point(data=b, aes(x=Site, y = (mean)/8), size=4, pch = 21, fill = 'white')+
   scale_x_discrete(name = NULL, 
            labels = c('TPT', 'P. Satumu','P. Hantu', 'P. Semakau', 'P. Jong',
                                'Sisters', 'Kusu', 'All'))+
  scale_fill_fish_d(option = 'Naso_lituratus', 
          labels = c('Competitive', 'Weedy', 'Generalist', 'Stress Tolerant'))+
  theme_minimal() + labs(y = 'Contribution to Coral G (%)')+
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

c <- lformmelt %>% 
  group_by(Site,variable) %>% 
  summarise(mean = mean(value))

(morph<-ggplot(c,aes(x = Site, y = mean)) + 
  geom_col(aes(fill = variable), position = "fill",stat = "identity", 
          colour = 'black', linetype=1) +
  scale_y_continuous(labels = percent_format(), expand = c(0,0), 
          sec.axis = sec_axis(~./8, 
          breaks=c(0,0.125/8, 2*0.125/8, 3*0.125/8, 4*0.125/8, 5*0.125/8,
                   6*0.125/8, 7*0.125/8, 0.125), 
          labels=c('0','1','2','3','4','5','6','7','8'),
          name = expression(paste("HC production (kg CaCO"[3],
                                  " m"^-2, " yr"^-1,")"))))+
  geom_errorbar(data=b, aes(x = Site, ymin = (mean-ci)/8,  ymax = (mean+ci)/8),
                width = 0, inherit.aes = F, colour = 'grey50')+
  geom_point(data=b, aes(x=Site, y = mean/8), size=4, pch = 21, fill = 'white')+
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
        axis.text = element_text(colour ='black')))

cowplot::plot_grid(morph, lhs, labels = c('(a)', '(b)'), label_size = 12, align = 'v',
          nrow =2, axis='l')
ggsave(here::here("figs_s", 
            "Figure_4.png"), 
       width = 5, height = 8)
ggsave(here::here("figs_s", 
            "Figure_4.eps"), family = 'sans', device = 'eps', 
       width = 5, height = 8)

erosdat %>% 
 dplyr:: group_by(Site) %>% 
  dplyr::summarise(mean=mean(eros_pfish), n = n())
## contribution of erosion
erosmelt<-melt(erosdat[c(1,4,5,7)], id.vars='Site')

d <- erosmelt %>% 
  group_by(Site,variable) %>% 
  summarise(mean = mean(value))
erosdat$eros_tot<-0-erosdat$eros_tot
e <- erosdat %>% 
  group_by(Site) %>% 
  summarise(mean = mean(eros_tot), ssd = sd(eros_tot), count = n()) %>% 
  mutate(se = ssd /sqrt(count), ci = qt(1-(0.05/2), count-1) * se)

(ero<-ggplot(d,aes(x = Site, y = mean)) + 
  geom_col(aes(fill = variable), position = "fill",stat = "identity", 
           colour = 'black', linetype=1) +
  scale_y_continuous(labels = percent_format(), expand = c(0,0), 
                    sec.axis=sec_axis(~./6, 
                    breaks = c(0,0.028,0.056,0.084,0.110,0.138,0.16666), 
                    labels = c('0.0','1.0','2.0','3.0','4.0','5.0','6.0'), 
                    name = expression(paste("Bioerosion (kg CaCO"[3],
                                            " m"^-2, " yr"^-1,")"))))+
  geom_errorbar(data=e, aes(x = Site, ymin = (mean-ci)/6, 
                ymax = (mean+ci)/6), width = 0, inherit.aes = F)+
  geom_point(data=e, aes(x=Site, y = mean/6), size=4, pch = 21, 
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

ggsave(here::here("figs_s", 
            "Figure_S4.png"), 
       width = 5, height = 4)

ggsave(here::here("figs_s", 
            "Figure_S4.eps"), device = 'eps', family = 'sans',
       width = 5, height = 4)

## ========================================
## ===      ANOVA/KRUSKAL-WALLIS        ===
## ========================================

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


### HARD CORAL PRODUCTION ###

## check for normality
qqnorm(subset(dat, Site != 'Mean')$hcprod_mean)
qqline(subset(dat, Site != 'Mean')$hcprod_mean)

m2<-aov(hcprod_mean ~ Site, data = subset(dat, Site != 'Mean'))
summary(m2)               
TukeyHSD(m2, 'Site', ordered = FALSE, conf.level = 0.95)


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

### TOTAL EROSION ###

dat$eros_tot<-erosdat$eros_tot
## check for normality
qqnorm(log(dat$eros_tot))
qqline(log(dat$eros_tot))

dat1<-subset(dat, Site != 'Mean')
m4<-aov(log(eros_tot) ~ Site, data = dat1)
summary(m4)               
TukeyHSD(m4, 'Site', ordered = FALSE, conf.level = 0.95)

### RUGOSITY ###

qqnorm(sqrt(subset(dat, Site != 'Mean')$rug))
qqline(sqrt(subset(dat, Site != 'Mean')$rug))

## transform rug
dat$trug<-sqrt(dat$rug)

m6<-aov(trug ~ Site, data = subset(dat, Site != 'Mean'))
summary(m6)               
TukeyHSD(m6, 'Site', ordered = FALSE, conf.level = 0.95)


##coral cover
qqnorm(dat$coral_cover)
qqline(dat$coral_cover)

m7<-aov(coral_cover ~ Site, data = subset(dat, Site != 'Mean'))
summary(m7)               
TukeyHSD(m7, 'Site', ordered = FALSE, conf.level = 0.95)


##turf
qqnorm(dat$turf_cover)
qqline(dat$turf_cover)

qqnorm(sqrt(dat$turf_cover))
qqline(sqrt(dat$turf_cover))

m8<-aov(sqrt(turf_cover) ~ Site, data = subset(dat, Site != 'Mean'))
summary(m8)               
TukeyHSD(m8, 'Site', ordered = FALSE, conf.level = 0.95)


qqnorm(dat$mac_cover)
qqline(dat$mac_cover)
m8<-aov(sqrt(mac_cover) ~ Site, data = subset(dat, Site != 'Mean'))
summary(m8)               
TukeyHSD(m8, 'Site', ordered = FALSE, conf.level = 0.95)



