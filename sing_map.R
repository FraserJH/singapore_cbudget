
#http://stackoverflow.com/questions/28001212/making-a-world-map-with-plots-based-on-the-density-using-ggmap

library(rgdal)
library(rgeos)
library(httr)
library(ggplot2)
library(dplyr)
library(magrittr)
library(countrycode)
library(geosphere)
library(plyr)
library(here)


sites <- read.csv('sites.csv')
sing <- readOGR("SGP_adm0.json")
sing_map <- fortify(sing)
head(sing_map)


# Plot our world map ----------------------------------------------
gg <- ggplot(sing_map, aes(x=long, y=lat)) + 
  geom_map(map=sing_map, data=sing_map, 
           aes(map_id=id), 
           fill = "grey80", colour = "grey15") +
  scale_x_continuous(limits = c(103.65,103.9), breaks=c(103.70,103.80,103.9), name = 'Longitude') +
  scale_y_continuous(limits = c(1.15,1.28),  breaks = c(1.15,1.2,1.25), name = 'Latitude') +
  labs(x=NULL, y=NULL) + theme_bw()+
  geom_point(data=sites, aes(x = Long, y = Lat), colour = 'black', size = 4)+
 coord_equal() 
gg

(gg <- gg + annotate('text', x = sites$LabLong, y = sites$LabLat, label = sites$Site))


ggsave(here("figs", 
            "Fig_1.pdf"), 
       width = 8, height = 5)
