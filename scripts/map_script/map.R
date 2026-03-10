library(sf)
library(tidyverse)

proj <-  "+proj=lcc +lat_0=66.3 +lon_0=-42 +lat_1=66.3 +lat_2=66.3 +no_defs +R=6.371e+06"
locations <-  readxl::read_excel(path = "data/NVE_20_locations.xlsx") %>% 
  select(name, lon,lat) %>% distinct()

land <- rnaturalearth::ne_countries(country = "Norway", 
                                    returnclass = "sf", scale = 10) %>%
  st_transform(crs=4326) %>% 
  select(geometry)
locsSF <- st_as_sf(locations, coords = c("lon","lat"), crs= 4326)  %>% st_transform(crs=4326)
locsSF$lon <- st_coordinates(locsSF)[,"X"]
locsSF$lat <- st_coordinates(locsSF)[,"Y"]
centroids <- locsSF %>%
  group_by(name) %>%
  summarise(lon = mean(lon), lat = mean(lat))
area_order <- unique(centroids$name)[c(1:7, 15:20, 9:14, 8)]
centroids$name = factor(centroids$name, levels =area_order)
locsSF$name = factor(locsSF$name, levels =area_order)

map_norway <-land %>%
  ggplot() + 
  #geom_sf(data = st_as_sfc(st_bbox(land)), fill = "lightblue", color = NA) +  # blue ocean
  geom_sf(data = land, fill = "wheat", color = "black") +  # skin color for land
  geom_sf(pch = 15,fill = "transparent")+#fill = "wheat") +
  coord_sf(expand = FALSE, xlim = range(locsSF$lon)+c(-1,1), 
           ylim = range(locsSF$lat)+c(-1,1)) +
  geom_polygon(data = locsSF, aes(x=lon, y = lat, fill = name))+
  geom_text(data = centroids, aes(x=lon, y = lat, label = as.numeric(name)))+
  #geom_text(data = locsSF, aes(x=lon,y=lat,label=name), color = "black", size = 8)+
  theme(axis.title =element_blank(),
        axis.text=element_text(color = "gray30", size=8), #element_blank(),
        legend.title = element_blank(),
        axis.ticks=element_line(color="gray30"), #element_blank(),
        panel.border=element_blank(),
        plot.background = element_rect(fill = "transparent", color = "transparent"),
        panel.background = element_rect(fill = "transparent", color = "transparent")
        )


# Save map
ggsave("_figures/map.png", map_norway, width = 7, height=6, dpi=300)
