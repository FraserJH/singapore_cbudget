##### LIBRARIES #######
library(ggplot2)
library(cowplot)
library(here)
library(lme4)
library(MuMIn)
library(tidyverse)
library(car)
library(grid)

maj_cover<-read.csv('major_cover.csv')

## ID dominant (largest cover) genera at each site
maj_cover$dom<-colnames(maj_cover[,c(32:35)])[max.col(maj_cover[,c(32:35)],
                                                      ties.method="first")]
# Gnet (kg CaCO'[3],' m'^-2, ' yr'^-1, ')

pal<-c('#e9ef6f','#90b3fa','#626175','#ff9100') 
## taken from the Naso_lituratus scale in the Fishualize package
(all<-ggplot(data=maj_cover, aes(x = tot_cover, y = gnet))+
    geom_point(aes(colour = dom, shape=dom), size =2)+
    stat_smooth(method = 'lm', colour = 'black', linetype =3, alpha =0.2)+
    scale_shape_manual(values = c(15,17,18,19), guide = F)+
    scale_colour_manual(values = pal, 
                    labels = c('Branching', 'Encrusting', 'Massive', 'Other'))+
  stat_smooth(aes(colour = dom, group = dom),method='lm', alpha = 0.2)+
#scale_colour_fish_d(option = 'Naso_lituratus', labels = c('Branching', 'Encrusting', 'Massive', 'Other'))+theme_minimal()+
    geom_hline(yintercept = 0, linetype = 2)+
    scale_x_continuous(name = '',limits = c(-1,50), expand = c(0,0))+
    scale_y_continuous(name = expression(paste('')),
            limits=c(-4,10), breaks = c(-4,-2,0,2,4,6,8,10), expand = c(0,0))+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = 'black'),
        axis.ticks = element_line(colour = 'black'),
  legend.position = 'top', legend.title = element_blank(),
  legend.spacing.x = unit(0.4, 'cm'),
  legend.key.height = unit(0.2, 'cm'),
  legend.key.width = unit(0.8, 'cm')))

(mass<-ggplot(data=maj_cover, aes(x = tot_mass, y = gnet))+
    geom_point(size = 2, colour = 	'#626175')+
    stat_smooth(method='lm', colour = '#626175', linetype =3, alpha =0.2)+
    scale_x_continuous(name = ' ', limits = c(-0.33,15), expand = c(0,0))+
    scale_y_continuous(name = '',
                       limits=c(-4,8), breaks = c(-4,0,4,8), expand = c(0,0))+
    geom_hline(yintercept = 0, linetype = 2)+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = NA, colour = NA),
        axis.line = element_line(colour = 'black'),
        axis.ticks = element_line(colour = 'black')))

(other<-ggplot(data=maj_cover, aes(x = tot_other, y = gnet))+
    geom_point(size = 2,colour = '#ff9100')+
    stat_smooth(method='lm', colour = '#ff9100', linetype =3, alpha =0.2)+
    scale_x_continuous(name = ' ', limits = c(-0.8,40), expand = c(0,0))+
    scale_y_continuous(name = '',
                       limits=c(-4,8), breaks = c(-4,0,4,8), expand = c(0,0))+
    geom_hline(yintercept = 0, linetype = 2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black')))

(enc<-ggplot(data=maj_cover, aes(x = tot_enc, y = gnet))+
    geom_point(size = 2,colour = '#90b3fa')+
    stat_smooth(method='lm', colour = '#90b3fa', linetype =3, alpha =0.2)+
    scale_x_continuous(name = ' ', expand = c(0,0), limits=c(-0.5,26))+
    scale_y_continuous(name = '',
                       limits=c(-4,8), breaks = c(-4,0,4,8), expand = c(0,0))+
    geom_hline(yintercept = 0, linetype = 2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black')))

(branch<-ggplot(data=maj_cover, aes(x = tot_branch, y = gnet))+
    geom_point(size = 2,colour = 	'#e9ef6f')+
   # stat_smooth(method='lm', colour = '#e9ef6f', linetype =3, alpha =0.2)+
    scale_x_continuous(name = ' ', expand = c(0,0), limits=c(-0.5, 15))+
    scale_y_continuous(name = '',
                       limits=c(-4,8), breaks = c(-4,0,4,8), expand = c(0,0))+
    geom_hline(yintercept = 0, linetype = 2)+
    theme(panel.grid = element_blank(),
          panel.background = element_rect(fill = NA, colour = NA),
          axis.line = element_line(colour = 'black'),
          axis.ticks = element_line(colour = 'black')))


# bottom_row <- plot_grid(mass, other, enc, branch, 
# labels = c( 'B', 'C', 'D', 'E'), nrow = 1)
right_col <- plot_grid(mass, other, enc, branch, 
                       labels = c( 'B', 'C', 'D', 'E'),
                        nrow = 4)
# rowcov<-plot_grid(all, bottom_row, labels = 'A', nrow = 2, rel_heights = c(6/10,4/10))
# ggsave(here("figs", 
#             "Fig_4_row.png"), 
#        width = 8, height = 6)

(colcov<-plot_grid(all, right_col, labels = 'A', nrow = 1, 
                   rel_widths = c(7/10,3/10),scale = 0.95)+
draw_label('Coral Cover (%)', x=0.5, y = 0, vjust = -0.8, angle = 0)+
    draw_label(expression(paste('Gnet (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
               x=0, y = 0.5, vjust = 1, angle = 90))
    
ggsave(here("figs", 
            "Fig_4_col.png"), 
       width = 8, height = 6)



####model selection##########

## morphological categories
##null models
m0<-lm(tgnet~1, data = maj_cover)
mm0<-lmer(tgnet~1+(1|Site), data = maj_cover) ##site as random
AIC(m0,mm0)

##full models
m1<-lm(tgnet~cca_cover + mac_cover +turf_cover + numb_gen + tot_mass + 
          tot_enc +tot_branch + tot_other + Rugosity, dat = maj_cover)
mm1<-lmer(tgnet~cca_cover + mac_cover +turf_cover + numb_gen + tot_mass +
            tot_enc +tot_branch + tot_other + (1|Site), dat = maj_cover)

AIC(m0,mm0, m1, mm1) # not even close - lm
plot(m1)
summary(m1)
vif(m1)


options(na.action = 'na.fail')
mods<-dredge(m1, beta='sd', extra = c("R^2", "adjR^2") )
head(mods)
top_mods<-subset(mods, delta <2)
b<-model.avg(top_mods)
summary(b)

## for the table
b$sw
b$coefficients
confint(b)
b$msTable

###### to draw forest plot
confint_95 <- as.data.frame(confint(b,full=T, level = 0.95))
confint_95$variable <- c(1,2,3,4,5,6,7)

morphs <- bind_cols(confint_95, as.data.frame(coefTable(b, full = TRUE)))
morphs
colnames(morphs)<-c('lower', 'upper', 'mean', 'estimate', 'sd', 'df')
morphs$mean <- factor(morphs$mean, levels = morphs$mean[order(morphs$estimate)])

(MORPHS <- ggplot(data=morphs[-1,], aes(x=mean, y=estimate, ymin=lower, ymax=upper)) +
  geom_pointrange()  + ylim(-1,1)+
    annotate("text", label = '**', x = 4.2, y = 0.20666, size = 5)+
    annotate("text", label = '***', x = 5.2, y = 0.47772, size = 5)+
    annotate("text", label = '***', x = 6.2, y = 0.57407, size = 5)+
  geom_hline(yintercept=0, lty=2)+ # add a dotted line at x=1 after flip
   scale_x_discrete(name = NULL, labels = c('Macroalgae','Branching', 'Rugosity','Genera (#)','Massive','Other'))+ 
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Variable") + ylab("Mean (95% CI)") +
  theme_minimal())

#### LHS models


m2<-lm(tgnet~cca_cover + mac_cover +turf_cover + numb_gen + stress + general+
         comp + weedy + Rugosity, dat = maj_cover)
mm2<-lmer(tgnet~cca_cover + mac_cover +turf_cover + numb_gen + stress + general+
         comp + weedy + Rugosity +(1|Site), dat = maj_cover)

AIC(m0,mm0,m2,mm2) #again with the lm

plot(m2)
summary(m2)
vif(m2)

mods2<-dredge(m2, beta='sd', extra = c("R^2", "adjR^2") )
head(mods2)
top_mods2<-subset(mods2, delta <2)
c<-model.avg(mods2, subset = delta<2)

## for the table
c$sw
c$coefficients
confint(c)
c$msTable

######
confint_95 <- as.data.frame(confint(c,full=T, level = 0.95))
confint_95$variable <- c(1,2,3,4,5,6,7,8)

lhs <- bind_cols(confint_95, as.data.frame(coefTable(c, full = TRUE)))
lhs
colnames(lhs)<-c('lower', 'upper', 'mean', 'estimate', 'sd', 'df')
lhs$mean <- factor(lhs$mean, levels = lhs$mean[order(lhs$estimate)])

(LHS <- ggplot(data=lhs[-1,], aes(x=mean, y=estimate, ymin=lower, ymax=upper)) +
    geom_pointrange()  + ylim(-1,1)+
    annotate("text", label = '**', x = 4.2, y = 0.25509, size = 5)+
    annotate("text", label = '**', x = 5.2, y = 0.29952, size = 5)+
    annotate("text", label = '***', x = 6.2, y = 0.33359, size = 5)+
    annotate("text", label = '***', x = 7.2, y = 0.43162, size = 5)+
    geom_hline(yintercept=0, lty=2) +# add a dotted line at x=1 after flip
    scale_x_discrete(name = NULL, labels = c('CCA','Macroalgae', 'Turf','Generalist','Rugosity','Stress-tolerant', 'Weedy'))+ 
    coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Variable") + ylab("Mean (95% CI)") +
    theme_minimal())




#### coral cover model

mc<-lm(tgnet~cca_cover + mac_cover +turf_cover + numb_gen +tot_cover + Rugosity, 
       data = maj_cover)
mmc<-lmer(tgnet~cca_cover + mac_cover +turf_cover + numb_gen +tot_cover 
        + Rugosity + (1|Site),  data = maj_cover)

AIC(m0,mm0,mc,mmc) #lm for the win


mods3<-dredge(mc, beta='sd', extra = c("R^2", "adjR^2") )
head(mods3)
top_mods3<-subset(mods3, delta <2)
d<-model.avg(mods3, subset = delta<2)

summary(d)

## for the table
d$sw
d$coefficients
confint(d)
d$msTable

############
confint_95 <- as.data.frame(confint(d,full=T, level = 0.95))
confint_95$variable <- c(1,2,3,4,5,6)

tot <- bind_cols(confint_95, as.data.frame(coefTable(d, full = TRUE)))
tot
colnames(tot)<-c('lower', 'upper', 'mean', 'estimate', 'sd', 'df')
tot$mean <- factor(tot$mean, levels = tot$mean[order(tot$estimate)])

(TOT <- ggplot(data=tot[-1,], aes(x=mean, y=estimate, ymin=lower, ymax=upper)) +
    geom_pointrange()  + ylim(-1,1)+
    annotate("text", label = '*', x = 1.2, y = -0.26, size = 5)+
    annotate("text", label = '***', x = 5.2, y = 0.58, size =5)+
    geom_hline(yintercept=0, lty=2) +# add a dotted line at x=1 after flip
    scale_x_discrete(name = NULL, labels = c('CCA','Macroalgae', 'Genera #','Rugosity','Total corals')) +
     coord_flip() +  # flip coordinates (puts labels on y axis)
    xlab("Variable") + ylab("Mean (95% CI)") +
   
    theme_minimal())



plot_grid(MORPHS, LHS, TOT, labels = c('A', 'B', 'C'), label_size = 12, align = 'v',
          nrow =3, axis='l')
ggsave(here("figs", 
            "Fig_5.png"), 
       width = 4, height = 6)

plot(m1)
plot(mm1)
summary(m1)
summary(mm1)
AIC(m0,m1,mm0,mm1)





########### NOT FREQUENT ENOUGH OCCURENCES OF ANY GENERA TO REALLY DO THE ANALYSIS ############
gen_cover<-read.csv('cover_genera.csv')

gen_cover2<-melt(gen_cover[,c(3,4,6:46)], id.vars = c('Site', 'Transect'), value.name = 'percent')
head(gen_cover)
gen_cover2 %>% 
  group_by(variable) %>% 
  summarise(sumer =  sum(percent)) %>% 
  arrange(desc(sumer)) %>% 
  View()

(por<-ggplot(data=gen_cover, aes(x = Porites, y = gnet))+geom_point())
(gal<-ggplot(data=gen_cover, aes(x = Echinopora, y = gnet))+geom_point())
(merl<-ggplot(data=gen_cover, aes(x = Merulina, y = gnet))+geom_point())

View(dat)
head(gen_cover)




