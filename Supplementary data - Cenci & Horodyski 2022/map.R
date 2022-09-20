library(cowplot)
library(magick)
library(png)
library(grid)
library(rmarkdown)
library(geobr)
library(sf)
library(magrittr)
library(dplyr)
library(ggplot2)
library(rgbif)
library(ggspatial)
library(maps) 
library(mapdata) 
library(ggmap)
library(raster)
library(maptools)
library("rnaturalearth")
library("rnaturalearthdata")

# pre-select and create object with data of each sf archives to plot

conj_dados <- list_geobr()

biomeaf <- read_biomes()

plot(biomeaf)

biomeaf

Atlantic <- biomeaf[4,]

plot(Atlantic)

Atlantic_limit <- st_crop(Atlantic, xmin = -60, xmax = -30,
                        ymin = -10, ymax = -35) 

ggplot() +
  geom_sf(data = Atlantic_limit, fill="#2D3E50", color="#FEBF57", size=.15, show.legend=F)

stateslimit <- read_state()
states_limit <- st_crop(stateslimit, xmin = -60, xmax = -30,
                          ymin = -10, ymax = -35) 

ggplot() +
  geom_sf(data = states_limit, fill="#2D3E50", color="#FEBF57", size=.15, show.legend=F)

# locate coordinates of the cities to be displayed in the map, 
# they are set with geom_point in the map, see below

POA <- data.frame(c(-51.2286604637023, -30.0300367747664))
          FLOR <- c(-48.5476373781933, -27.5877955485499)
SAO <- c(-46.5703831821127, -23.5673865)
Rio <- c(-43.2278751249952, -22.8766521181864)          

# Setting South America boundaries in the map
 
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
plot(world)  

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-60, -30), ylim = c(-10,-35), expand = FALSE)

mapa_limit <- st_crop(world, xmin = -60, xmax = -30,
                      ymin = -10, ymax = -35)

        
# plotting araucaria map scrip


araucariadata <- occ_data(scientificName = 'Araucaria angustifolia', hasCoordinate = TRUE, 
                          decimalLongitude = "-53, -30", decimalLatitude = "-30, -10")


mapUL<- ggplot() +
  theme(plot.margin = unit(c(0.1,1,0.2,0.1), "cm"))+
  geom_sf(data=world, size=0.1, color = "black", fill="#f7f7f7") + 
  geom_sf(data = states_limit, size=0.1, color = "black", fill="#f7f7f7") +
  geom_sf(data = Atlantic_limit, fill="#abfda1", color="#bdbdbd", size=.15, show.legend=F) +
          theme_bw() + theme(axis.text=element_text(size=6)) +
  coord_sf(xlim = c(-60,-30), ylim=c( -10,-35), expand = FALSE) +
  theme(axis.line = element_blank(), axis.title.x = element_blank(), 
        axis.title.y = element_blank(), panel.border = element_blank(),
        panel.grid = element_line(color="#e1e1e1", size = 0.5, linetype=2)) +
  geom_point(data = araucariadata$data,
             aes(araucariadata$data$decimalLongitude, araucariadata$data$decimalLatitude), alpha=0.6, 
             size = 1, color = "forestgreen") +
  geom_point(aes(x=-51.2286604637023, y=-30.0300367747664), size =2, shape=17, color = "red", fill="red") +
  geom_point(aes(x=-46.5703831821127, y= -23.5673865), size =2, shape=17, color = "red", fill="red") +
  geom_point(aes(x=-50.2308, y=-29.2528), size =2, shape=15, color = "#fec44f", fill="#fec44f") +
  annotate(geom = "text", x = -54.5 , y = -32.5, label = "Uruguay",
           size = 1.9,  hjust = "right") +
  annotate(geom = "text", x = -56.3 , y = -27.9, label = "Argentina",
           size = 1.9,  hjust = "right", angle=30) +
  annotate(geom = "text", x = -56.5 , y = -24.5, label = "Paraguay",
           size = 1.9,  hjust = "right", angle = 320) +
  annotate(geom = "text", x = -50.6 , y = -30.4, label = "Porto Alegre",
           size = 2.1,  hjust = "left") +
  annotate(geom = "text", x = -49.75 , y = -29.5, label = "UL",
           size = 2.1,  hjust = "left") +
  annotate(geom = "text", x = -46 , y = -24.3, label = "São Paulo",
           size = 2.1,  hjust = "left") +
  annotate(geom = "text", x = -45 , y = -27.5, label = "Atlantic Ocean",
           size = 2.1,  hjust = "left") +
  annotation_scale(location = "br", width_hint = .2) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "cm"), pad_y = unit(0.7, "cm"),
                         style = north_arrow_fancy_orienteering)

theme_set(theme_cowplot())


imageUL <- ggdraw() +
   theme(plot.margin = unit(c(1,1,1,1), "mm")) +
  draw_image("/Users/Romulo/Desktop/Backup/Documents/Doutorado Unisinos/paper 1/Revision received date February 5/map/UL_locality.png")


plot_grid(mapUL, imageUL, nrow=2, ncol=1, rel_heights = c(0.8,1),
          labels=c("A", "B"),
          align = "v")

ggsave(filename = "figure 1.pdf",
       path = "C:/Users/Romulo/Desktop/Backup/Documents/Doutorado Unisinos/paper 1/Revision received date February 5/map",
       width = 8, 
       height = 15, 
       units = "cm", 
       dpi = 1000)
