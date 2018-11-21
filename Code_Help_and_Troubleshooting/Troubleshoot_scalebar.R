# load libraries
library(tidyverse)
library(viridis)
library(sp)
library(rgdal)
library(ggsn)

# data
my.data.df <- data.frame(value = c(-1.2269832, -1.0153486, -0.9581069, -1.1534667, -1.0139735, -0.8711997, -0.8618286, -0.8524683),
                         x = c(538127.3, 538129.3, 538125.3, 538127.3, 538129.3,538131.3, 538121.3, 538123.3),
                         y = c(1101055, 1101055, 1101053, 1101053, 1101053, 1101053, 1101051, 1101051))
bb <- data.frame(long = c(538107, 538142), lat = c(1101020.5, 1101058.5))

# make a map
my.map <- ggplot(my.data.df, aes(x=x, y=y, fill=value)) +
  geom_tile() +
  scale_fill_viridis("Subsidence (m)",
                     limits = c(-1.5, 0.5)) +
  scale_x_continuous(limits = c(538107, 538142)) +
  scale_y_continuous(limits = c(1101020.5, 1101058.5)) +
  coord_fixed() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.justification=c(1,0),
        legend.position=c(1,0),
        plot.margin=unit(c(0,0,0,0), "cm"))

my.map + 
  ggsn::scalebar(dist = 5, dd2km = FALSE, dist_unit = 'm', location = 'bottomleft')

my.map + 
  ggsn::scalebar(dist = 5, dd2km = FALSE, dist_unit = 'm', location = 'bottomleft', x.min = 538107, x.max = 538142, y.min = 1101020.5, y.max = 1101058.5)

my.map + 
  ggsn::scalebar(data = my.data.df, dist = 5, dd2km = FALSE, dist_unit = 'm', location = 'bottomleft', x.min = 538107, x.max = 538142, y.min = 1101020.5, y.max = 1101058.5)

my.map + 
  ggsn::scalebar(data = bb, dist = 5, dd2km = FALSE, dist_unit = 'm', location = 'bottomleft', x.min = 538107, x.max = 538142, y.min = 1101020.5, y.max = 1101058.5)
