library(ggplot2)
library(cowplot)
library(here)
library(reshape)
library(plotrix)
library(fishualize)

maj_cover<-read.csv('data/major_cover_sing.csv')
maj_cover<-maj_cover[1:42,]
maj_cover2<-maj_cover[7:30,]
slr<-read.csv('data/slr.csv')
slr$sitenum<-rep(sort(rep(c(1:7), 6)),3)

slrmelt<-melt(slr[,-3], id = c("Site", "sitenum", "Scen"))

Sites<-c('1'='TPT','2'= ' PSat','3'= 'PH','4'= 'PSem', '5'='PJ', '6'='SIS','7'= 'K')


library(tidyr)
mc<-maj_cover[,c(2,3,32,33,34,17,18)]

maj_cover %>% 
    dplyr:: group_by(Site) %>% 
    dplyr::summarise(mean=mean(v_accret), std.err = std.error(v_accret))

####### boxplot for RAP no sedimentation

slrmelt$Site = with(slrmelt, reorder(Site, sitenum))
(vaccret<-ggplot(dat=slrmelt,aes(x=variable, y=(value)))+
    geom_boxplot(aes(group =variable, fill = Site), outlier.size = 0)+
    geom_point(aes(fill = Site), pch = 21,position = position_jitterdodge(), show.legend = T)+
        geom_hline(yintercept = 0, lty=2)+
       scale_fill_fish_d(option = 'Naso_lituratus')+
    facet_grid(.~Scen)+
        labs(y = expression(paste('Difference between SLR & Vertical Accretion (mm yr'^-1,')')))+
               theme(panel.grid = element_blank(),
              strip.background = element_blank(), strip.placement = "outside",
              panel.background = element_rect(fill = NA, colour = 'black'),
              axis.line.y = element_line(colour = 'black'),
              axis.text.x = element_text(angle = 90),
              axis.title.x = element_blank(),
   
              axis.ticks = element_line(colour = 'black')))

ggsave(here::here('figs_s', 'Fig_6.png'), width = 9, height = 6)
ggsave(here::here('figs_s', 'Fig_6.eps'), width = 9, height = 6, family = 'sans')

slr2<-read.csv('data/slr2.csv')

slr2$sitenum<-rep(sort(rep(c(1:4), 6)),3)

slrmelt2<-melt(slr2[,-3], id = c("Site", "Scen", "sitenum"))

####### boxplot for RAP with sedimentation
slrmelt2$Site = with(slrmelt2, reorder(Site, sitenum))
(vaccret2<-ggplot(dat=slrmelt2,aes(x=variable, y=(value)))+
        geom_boxplot(aes(group =variable, fill = Site), outlier.size = 0)+
    geom_point(aes(fill = Site), pch = 21,position = position_jitterdodge(), show.legend = T)+
    geom_hline(yintercept = 0, lty=2)+
        scale_fill_fish_d(option = 'Naso_lituratus')+
       facet_grid(.~Scen))+
        labs(y = expression(paste('Difference between SLR & Vertical Accretion (mm yr'^-1,')')))+
        theme(panel.grid = element_blank(),
              strip.background = element_blank(), strip.placement = "outside",
              panel.background = element_rect(fill = NA, colour = 'black'),
              axis.line.y = element_line(colour = 'black'),
              axis.text.x = element_text(angle = 90),
              axis.title.x = element_blank(),
              axis.ticks = element_line(colour = 'black'))

ggsave(here::here('figs_s', 'Fig_S4.png'), width = 9, height = 6)
ggsave(here::here('figs_s', 'Fig_S4.eps'), width = 9, height = 6, family = 'sans')


