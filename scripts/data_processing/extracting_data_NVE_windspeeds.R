rm(list=ls())
library(sf)
library(tidyverse)
library(lubridate)
library(ncdf4)
library(reshape2)
Sys.setlocale("LC_ALL", "Norwegian")
Sys.getlocale()

# Location and area paths
pathtoDropbox <- "data/"


locations <- readxl::read_excel(paste0(pathtoDropbox,"NVE_20_locations.xlsx")) %>% 
  group_by(name) %>% 
  summarize(lon = mean(lon), lat = mean(lat))

areal <- readxl::read_excel(paste0(pathtoDropbox,"NVE_areal.xlsx")) %>% 
  mutate(locID = 1:20) %>% select(name,locID)


locations<-locations %>% left_join(areal,by = "name") %>% arrange(locID)


lon <- ncvar_get(nc_open("data/WindSpeed_hourly_1996.nc"),
                 "lon")
lat <- ncvar_get(nc_open("data/WindSpeed_hourly_1996.nc"),
                 "lat")

nora3.loc <-
  left_join(melt(lon) %>%rename(lon = value),
            melt(lat) %>%rename(lat = value), 
            by = c("Var1","Var2"))%>%
  rename(row = Var1, col = Var2)  %>%
  filter(between(lon, min(locations$lon)-1, 1+max(locations$lon)),
         between(lat, min(locations$lat)-1, 1+max(locations$lat)))%>% 
  st_as_sf(crs = 4326, coords = c("lon","lat"))


locations <- locations %>% 
  st_as_sf(crs = 4326, coords = c("lon","lat"))

distmat = st_distance(locations, nora3.loc)

str(distmat)
nora3.gridpoints<- nora3.loc[apply(distmat, 1, which.min),]
nora3.gridpoints$lon <- st_coordinates(nora3.gridpoints)[,"X"]
nora3.gridpoints$lat <- st_coordinates(nora3.gridpoints)[,"Y"]
nora3.gridpoints$orglon <- st_coordinates(locations)[,"X"]
nora3.gridpoints$orglat <- st_coordinates(locations)[,"Y"]
nora3.gridpoints$distance <- diag(distmat[1:nrow(locations), apply(distmat, 1, which.min)])
nora3.gridpoints$name <- locations$name
nora3.gridpoints %>%as_tibble() %>% select(name, lon,lat,orglon,orglat,distance) %>% print(n = 13)

land <- rnaturalearth::ne_countries(continent = "Europe", 
                                    returnclass = "sf", scale = 10) %>%
  select(geometry)
ggplot(land) + 
  geom_sf() + coord_sf(ylim = c(56, 75), xlim = c(-8, 35)) +
  geom_point(data = nora3.gridpoints, aes(x=lon, y = lat), col = 2) +
  geom_text(data = nora3.gridpoints, aes(x=lon, y = lat, label = name), col = 2, size =4, hjust = 1) + 
  theme_minimal() + theme(panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(), 
                          axis.text = element_blank(),
                          axis.title = element_blank())



locs <- tibble(
  name = locations$name,
  lon = nora3.gridpoints$lon,
  lat = nora3.gridpoints$lat,
  row = nora3.gridpoints$row,
  col = nora3.gridpoints$col
) %>% mutate(locID = 1:n())

years <- c(1996:2019)

for(i in 1:nrow(locs)) {
  print(i)
  x <- locs[i,]
  lwpy <- list()
  k = 1

  for(year in years){
    print(year)
    wpy <- ncvar_get(nc_open(paste0("data/WindSpeed_hourly_",year,".nc")),
                     "WindSpeed_hourly",
              start = c(x$row, x$col, 3,1),
              count = c(1,1,1,-1))
    time <- ncvar_get(nc_open(paste0("data/WindSpeed_hourly_",year,".nc")),
                      "time")
    lwpy[[k]] <- melt(wpy) %>% rename(hourOfYear = Var1) %>% mutate(year = year,
                                                                       time = time[hourOfYear],
                                                                       name = x$name,
                                                                       lon = x$lon,
                                                                       lat = x$lat,
                                                                       row = x$row,
                                                                       col = x$col,
                                                                       locID = x$locID) %>%
      select(-hourOfYear)
    k <-k+1
  }
  cat("Location ", i, " completed.\n")
  lwpy <- lwpy %>% bind_rows()
  saveRDS(lwpy, file = paste0("data/NVE_areas_windspeeds/",x$locID, ".rds"))
  
}

data <- lapply(list.files("data/NVE_areas_windspeeds/", full.names = T), readRDS) %>% bind_rows()
data <- data %>% 
  mutate(datetime =as.POSIXct((time-1)*60*60,origin='1970-01-01 00:00:01') )
second(data$datetime) <- 0
data <- data %>% select(-time,-row,-col,-year) %>% rename(windspeed = value, region = name)
saveRDS(data, file = "data/NVE_windspeeds_1996-97.rds")

ggplot(data %>% filter(year(datetime)==1996, locID == 19),
        aes(x = datetime, y = powerprod, col = factor(locID))) + geom_line() +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank())

ggplot(data %>% filter(locID==19)) +
       geom_density(aes(x = powerprod))+
  facet_wrap( ~ lubridate::month(datetime, label = T))+
  geom_vline(xintercept = c(3,10.59,25), lty = 2)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank())
