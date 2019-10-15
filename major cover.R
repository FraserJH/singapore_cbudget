gen_cover<-read.csv('cover_genera.csv')

gen_cover2<-melt(gen_cover[,c(3,4,6:46)], id.vars = c('Site', 'Transect'), value.name = 'percent')
head(gen_cover)
gen_cover2 %>% 
  group_by(variable, Site) %>% 
  summarise(sumer =  mean(percent)) %>% 
  arrange(desc(sumer)) %>% 
  View()

(por<-ggplot(data=gen_cover, aes(x = Porites, y = gnet))+geom_point())
(mont<-ggplot(data=gen_cover, aes(x = Montipora, y = gnet))+geom_point())
(merl<-ggplot(data=gen_cover, aes(x = Merulina, y = gnet))+geom_point())

View(dat)
head(gen_cover)



maj_cover<-read.csv('major_cover.csv')
head(maj_cover)
maj_cover<-maj_cover[-7,]
maj_cover2<-melt(maj_cover[,c(2,3,4:18)], id.vars = c('Site', 'Transect'), value.name = 'percent')
maj_cover2 %>% 
  group_by(variable) %>% 
  summarise(sumer =  mean(percent)) %>% 
  arrange(desc(sumer)) %>% 
  View()
maj_cover$tot_mass <- maj_cover$mass_por +maj_cover$mass_oth
maj_cover$tot_enc <- maj_cover$enc_other + maj_cover$enc_por
maj_cover$tot_branch <-maj_cover$br_acro + maj_cover$poc + maj_cover$oth_br + maj_cover$br_por
maj_cover$tot_other <-maj_cover$fol + maj_cover$mush +maj_cover$oth
maj_cover$tot_cover <-maj_cover$tot_mass + maj_cover$tot_enc + maj_cover$tot_other + maj_cover$tot_branch

maj_cover$dom<-colnames(maj_cover[,c(31:34)])[max.col(maj_cover[,c(31:34)],ties.method="first")]
Gnet (kg CaCO'[3],' m'^-2, ' yr'^-1, ')

pal<-c('#e9ef6f','#90b3fa','#626175','#ff9100')
(all<-ggplot(data=maj_cover, aes(x = tot_cover, y = gnet))+
    geom_point(aes(colour = dom, shape=dom), size =2)+stat_smooth(method = 'lm', colour = 'black', linetype =3, alpha =0.2)+
    scale_shape_manual(values = c(15,17,18,19), guide = F)+
    scale_colour_manual(values = pal, labels = c('Branching', 'Encrusting', 'Massive', 'Other'))+
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


# bottom_row <- plot_grid(mass, other, enc, branch, labels = c( 'B', 'C', 'D', 'E'),
#                         nrow = 1)
right_col <- plot_grid(mass, other, enc, branch, labels = c( 'B', 'C', 'D', 'E'),
                        nrow = 4)
# rowcov<-plot_grid(all, bottom_row, labels = 'A', nrow = 2, rel_heights = c(6/10,4/10))
# ggsave(here("figs", 
#             "Fig_4_row.png"), 
#        width = 8, height = 6)

(colcov<-plot_grid(all, right_col, labels = 'A', nrow = 1, rel_widths = c(7/10,3/10),
                   scale = 0.95)+
draw_label('Coral Cover (%)', x=0.5, y = 0, vjust = -0.8, angle = 0)+
    draw_label(expression(paste('Gnet (kg CaCO'[3],' m'^-2, ' yr'^-1, ')')),
               x=0, y = 0.5, vjust = 1, angle = 90))
    
ggsave(here("figs", 
            "Fig_4_col.png"), 
       width = 8, height = 6)







(other<-ggplot(data=maj_cover, aes(x = tot_other, y = gnet))+geom_point())+stat_smooth(method='lm')
(br<-ggplot(data=maj_cover, aes(x = tot_branch, y = gnet))+geom_point())+stat_smooth(method='lm')
(enc<-ggplot(data=maj_cover, aes(x = tot_enc, y = gnet))+geom_point())+stat_smooth(method='lm')

min(maj_cover$gnet)
tnet<-(1-min(maj_cover$gnet))
qqnorm(log(maj_cover$gnet +( 1 - min(maj_cover$gnet))))
qqline(log(maj_cover$gnet +( 1 - min(maj_cover$gnet))))
qqnorm(sqrt(maj_cover$gnet +( 1 - min(maj_cover$gnet))))
qqline(sqrt(maj_cover$gnet +( 1 - min(maj_cover$gnet))))
qqnorm(maj_cover$gnet)
qqline(maj_cover$gnet)

maj_cover$tgnet<-log(maj_cover$gnet +( 1 - min(maj_cover$gnet)))

maj_cover
m1<-lm(tgnet~tot_mass + tot_enc + tot_branch + tot_other , data=maj_cover)
summary(m1)
m2<-lmer(tgnet~tot_mass + tot_enc + tot_branch + tot_other + (1|Site), data=maj_cover)
summary(m2)
plot(m2)
AIC(m1,m2)

ggpredict(m1, 'tot_enc')

sjp.lmer(m1, type='pred', terms='Rugosity')
plot(m2)
(a<-plot_model(m1, type='resid'))
names(a)
a<-a$data
a$predicted<-exp(a$predicted) - 1-min(maj_cover$gnet)
a$conf.low<-exp(a$conf.low) - 1-min(maj_cover$gnet)
a$conf.high<-exp(a$conf.high) - 1-min(maj_cover$gnet)
plot(predicted~ x, data =a, type ='l', ylim = c(-4,10))
lines(conf.low~ x, data =a, type ='l', ylim = c(-4,10))
lines(conf.high~ x, data =a, type ='l', ylim = c(-4,10))

b<-plot_model(m1, type='pred', terms = 'tot_mass')
names(b)
b<-b$data
b$predicted<-exp(b$predicted) - 1-min(maj_cover$gnet)
b$conf.low<-exp(b$conf.low) - 1-min(maj_cover$gnet)
b$conf.high<-exp(b$conf.high) - 1-min(maj_cover$gnet)
plot(predicted~ x, data =b, type ='l', ylim = c(-4,10))
lines(conf.low~ x, data =b, type ='l', ylim = c(-4,10))
lines(conf.high~ x, data =b, type ='l', ylim = c(-4,10))

c<-plot_model(m1, type='pred', terms = 'tot_other')
names(c)
c<-c$data
c$predicted<-exp(c$predicted) - 1-min(maj_cover$gnet)
c$conf.low<-exp(c$conf.low) - 1-min(maj_cover$gnet)
c$conf.high<-exp(c$conf.high) - 1-min(maj_cover$gnet)
plot(predicted~ x, data =c, type ='l', ylim = c(-4,10))
lines(conf.low~ x, data =c, type ='l', ylim = c(-4,10))
lines(conf.high~ x, data =c, type ='l', ylim = c(-4,10))

d<-plot_model(m1, type='pred', terms = 'tot_branch')
names(d)
d<-d$data
d$predicted<-exp(d$predicted) - 1-min(maj_cover$gnet)
d$conf.low<-exp(d$conf.low) - 1-min(maj_cover$gnet)
d$conf.high<-exp(d$conf.high) - 1-min(maj_cover$gnet)
plot(predicted~ x, data =d, type ='l', ylim = c(-4,10))
lines(conf.low~ x, data =d, type ='l', ylim = c(-4,10))
lines(conf.high~ x, data =d, type ='l', ylim = c(-4,10))

e<-plot_model(m1, type='pred', terms = 'tot_branch')
names(e)
e<-e$data
e$predicted<-exp(e$predicted) - 1-min(maj_cover$gnet)
e$conf.low<-exp(e$conf.low) - 1-min(maj_cover$gnet)
e$conf.high<-exp(e$conf.high) - 1-min(maj_cover$gnet)
plot(predicted~ x, data =e, type ='l', ylim = c(-4,10))
lines(conf.low~ x, data =e, type ='l', ylim = c(-4,10))
lines(conf.high~ x, data =e, type ='l', ylim = c(-4,10))

plot_model(m1, type='pred', terms = 'tot_branch')
plot_model(m1, type='pred', terms = 'tot_other')
plot_model(m1, type='pred', terms = 'tot_mass')
plot_model(m1, type='slope')



predict(m1)

predict(m2)
f<-(maj_cover$tot_mass*0.061234)
plot(maj_cover$tgnet, f)
plot_model(m2, type = "pred", terms = "tot_mass")
     