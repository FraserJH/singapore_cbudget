gen_cover<-read.csv('genera_cover.csv')

gen_cover2<-melt(gen_cover[,c(2,3,5:45)], id.vars = c('Site', 'Transect'), value.name = 'percent')
head(gen_cover)
gen_cover2 %>% 
  group_by(variable) %>% 
  summarise(sumer =  countif(percent > 0)) %>% 
  arrange(desc(sumer)) %>% 
  View()

(por<-ggplot(data=gen_cover, aes(x = Porites, y = gnet))+geom_point())
(mont<-ggplot(data=gen_cover, aes(x = Montipora, y = gnet))+geom_point())
(merl<-ggplot(data=gen_cover, aes(x = Merulina, y = gnet))+geom_point())

View(dat)
head(gen_cover)

write.csv(gen_cover, 'cover_genera.csv')

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
(mass<-ggplot(data=maj_cover, aes(x = tot_mass, y = gnet))+geom_point())+stat_smooth(method='lm')
(other<-ggplot(data=maj_cover, aes(x = oth, y = gnet))+geom_point())+stat_smooth(method='lm')
(br<-ggplot(data=maj_cover, aes(x = tot_branch, y = gnet))+geom_point())+stat_smooth(method='lm')
(enc<-ggplot(data=maj_cover, aes(x = tot_enc, y = gnet))+geom_point())+stat_smooth(method='lm')
