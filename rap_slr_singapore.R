library(ggplot2)
library(cowplot)
library(here)

maj_cover<-read.csv('major_cover.csv')
maj_cover<-maj_cover[1:42,]

# delta rates  - from Tkalich et al. 2013
# recent
maj_cover$smean<-maj_cover$v_accret-3
maj_cover$sup<-maj_cover$v_accret-4.3
maj_cover$slow<-maj_cover$v_accret-1.7

#RCP 4.5 & 8.5 - from Cannaby et al. 2016
maj_cover$smean4.5<-maj_cover$v_accret-5.47
maj_cover$sup4.5<-maj_cover$v_accret-7.68
maj_cover$slow4.5<-maj_cover$v_accret-3.05

maj_cover$smean8.5<-maj_cover$v_accret-7.79
maj_cover$sup8.5<-maj_cover$v_accret-10.73
maj_cover$slow8.5<-maj_cover$v_accret-4.74

maj_cover<-transform(maj_cover, Site = reorder(Site, -v_accret))
maj_cover$sitenum <- as.numeric(maj_cover$Site)

####### boxplot in one chart if wanted
# (vaccret<-ggplot(dat=maj_cover,aes(x=sitenum, y=(smean)))+
#     geom_boxplot(aes(group=Site), outlier.colour = 'black', pch=21)+
#     geom_boxplot(aes(x = sitenum, y=smean4.5, group = Site), fill='grey80', outlier.colour = 'grey80')+
#     geom_boxplot(aes(x = sitenum, y=smean8.5, group = Site), fill='grey50', outlier.colour = 'grey50')+
#     scale_x_continuous(breaks = c(1,2,3,4,5,6,7), labels = c('PSem', 'TPT', 'PH', 'PSat', 'Kusu', 'Sisters', 'PJ'))+
#     scale_y_continuous(limits = c(-8,2), breaks = c(-8,-6,-4,-2,0,2), expand = c(0,0))+theme_minimal()+
#     geom_hline(yintercept = 0, lty=2)+
#     theme(panel.grid = element_blank(),
#           panel.background = element_rect(fill = NA, colour = NA),
#           axis.line.y = element_line(colour = 'black'),
#           axis.ticks = element_line(colour = 'black')))


### by transect delta rates
maj_cover<-maj_cover[order(-maj_cover$v_accret),]
maj_cover$order<-seq(1:42)

#recent
(vaccret<-ggplot(dat=maj_cover,aes(x=order, y=smean))+
    geom_point()+
    geom_point(aes(x=order, y=sup), fill='white', pch=21)+
    geom_point(aes(x=order, y=slow), fill='grey50', pch=21)+
    scale_y_continuous(name = NULL,
                       limits=c(-12,4), expand=c(0,0))+
    scale_x_continuous(name = '', labels=NULL )+
    geom_hline(yintercept = 0, linetype =2, colour = 'grey50' )+theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line.y = element_line(colour = 'black'),
          axis.text = element_text(size = 8),
          legend.position = 'none'))
#RCp4.5
(vaccret4.5<-ggplot(dat=maj_cover,aes(x=order, y=(smean4.5)))+
    geom_point()+
    geom_point(aes(x=order, y=(sup4.5)), fill='white', pch=21)+
    geom_point(aes(x=order, y=slow4.5), fill='grey50', pch=21)+
    scale_y_continuous(name = NULL,
                       limits=c(-12,4), expand=c(0,0))+
    scale_x_continuous(name = '', labels=NULL )+
    geom_hline(yintercept = 0, linetype =2, colour = 'grey50' )+theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line.y = element_line(colour = 'black'),
          axis.text = element_text(size = 8),
          legend.position = 'none'))
#RCP 8.5
(vaccret8.5<-ggplot(dat=maj_cover,aes(x=order, y=(smean8.5)))+
    geom_point()+
    geom_point(aes(x=order, y=(sup8.5)), fill='white', pch=21)+
    geom_point(aes(x=order, y=slow8.5), fill='grey50',  pch=21)+
    scale_y_continuous(name = NULL,
                       limits=c(-12,4), expand=c(0,0))+
    scale_x_continuous(name = '', labels=NULL )+
    geom_hline(yintercept = 0, linetype =2, colour = 'grey50' )+theme_minimal()+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line.y = element_line(colour = 'black'),
          axis.text = element_text(size = 8),
          legend.position = 'none'))

(plot_grid(vaccret, vaccret4.5, vaccret8.5, labels = c("A", "B","C"), label_size = 8,ncol=3)+
    draw_label(expression(paste('Difference between SLR & Vertical Accretion (mm yr'^-2,')')),
               x=-0, y = 0.5, vjust = 1, angle = 90, size = 8))
ggsave(here('figs', 'Fig_SLR.png'), width = 8, height = 3)
