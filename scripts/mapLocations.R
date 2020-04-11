library(raster)
library(dplyr)
library(rgdal)
library(sf)
library(ggplot2)
library(mapview)
library(gganimate)
library(lwgeom)

theme_set(theme_light())

badBirds <- c("99696852_0809")

# -----------------

coast <- readOGR("C:/Users/Allison/Documents/EnvirFiles/Coastline/GSHHS_shp/i/GSHHS_i_L1.shp")
coast <- as(coast, "sf")
coast <- st_make_valid(coast)
coast <- st_crop(coast, xmin = -100, xmax = -25, ymin = 35, ymax =72)
colony <- as(SpatialPoints(data.frame(lon = -82.01, lat = 62.95), proj4string = crs("+proj=longlat +datum=WGS84 +no_defs")), "sf")

# -----------------
year <- "Coats_2007_2009"
theFolder <- paste0("E:/Geolocators/Analysis/probGLS/",year)
theFiles <- list.files(theFolder, pattern = "RDS", full.names = F)

myData <- data.frame()
for (i in 1:length(theFiles)) {
  temp <- data.frame(readRDS(paste0(theFolder, "/",theFiles[i]))$'most probable track')
  temp$Band <- gsub(".RDS","",theFiles[i])
  myData <- rbind(myData, temp)
  print(paste("Finished:", theFiles[i]))
}

myData <- subset(myData, !(myData$Band %in% badBirds))

myData$dtime <- as.POSIXct(myData$dtime, tz = "UTC")
myData$date <- as.Date(myData$dtime, tz = "UTC")
myData$month <- as.numeric(strftime(myData$date, "%m"))
myData$year <- as.numeric(strftime(myData$date, "%Y"))
myData$week <- as.numeric(strftime(myData$date, "%V"))

weekMean <- myData %>% 
  group_by(Band, week, year) %>% 
  summarize(
    meanDate = min(date, na.rm = T),
    meanLon = mean(lon, na.rm = T),
    meanLat = mean(lat, na.rm = T),
    medianLon = NA,
    medianLat = NA
  ) %>% 
  arrange(Band, meanDate) %>% 
  data.frame()

for (i in 1:nrow(weekMean)) {
  temp <- subset(myData, myData$Band == weekMean$Band[i] & myData$week == weekMean$week[i] & myData$year == weekMean$year[i])
  idx <- which.min(pointDistance(p1 = temp[,c("lon", "lat")], p2 = weekMean[i,c("meanLon", "meanLat")], lonlat = T))
  weekMean$medianLon[i] <- temp$lon[idx]
  weekMean$medianLat[i] <- temp$lat[idx]
}

theBirds <- unique(weekMean$Band)

for (myBird in theBirds) {
  
  temp <- weekMean[weekMean$Band == myBird,]
  
  p <- ggplot() +
    geom_sf(data = coast, fill = grey(0.9), size = 0.3) +
    geom_sf(data = colony, shape = 17, size = 2) +
    geom_path(data = temp, aes(x = medianLon, y = medianLat)) +
    geom_point(data = temp, aes(x = medianLon, y = medianLat, fill = as.numeric(meanDate)), shape = 21) +
    scale_fill_distiller(palette = "YlOrRd", direction = 1) +
    guides(fill = F) +
    labs(title = temp$Band[1], x = "", y = "") +
    theme(axis.text.y = element_text(angle = 90))
  print(p)
  readline("Next [enter]")
}

# ---------------------

weekMean$trackYear <- NA
weekMean$trackYear[weekMean$meanDate >= "2017-08-01" & weekMean$meanDate < "2018-08-01" ] <- "2017/18"
weekMean$trackYear[weekMean$meanDate >= "2018-08-01" & weekMean$meanDate < "2019-08-01" ] <- "2018/19"
weekMean$trackYear[weekMean$meanDate >= "2007-08-01" & weekMean$meanDate < "2008-08-01" ] <- "2007/08"
weekMean$trackYear[weekMean$meanDate >= "2008-08-01" & weekMean$meanDate < "2009-08-01" ] <- "2008/09"
weekMean$trackYear[weekMean$meanDate >= "2009-08-01" & weekMean$meanDate < "2010-08-01" ] <- "2009/10"

for (yy in unique(weekMean$trackYear)) {
  
  temp <- weekMean[weekMean$trackYear == yy,]
  #temp <- subset(temp, temp$meanLat > 45 & temp$meanLat < 75)
  temp$Month <- factor(strftime(temp$meanDate, "%b"), c("Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun",'Jul'))
  
  p <- ggplot() +
    geom_path(data = temp, aes(x = medianLon, y = medianLat, group = Band), 
              size = 0.2, linetype = 2) +
    geom_sf(data = coast, alpha = 0.9, fill = grey(0.9), size = 0.3) +
    geom_point(data = temp, aes(x = medianLon, y = medianLat, fill = Month, group = Band), 
               shape = 21, size = 1.1, alpha = 0.7) +
    scale_fill_viridis_d( direction = 1) +
    scale_x_continuous(expand = c(0,0), breaks = seq(-10,-110, -20))+
    scale_y_continuous(expand = c(0,0), breaks = seq(5,75,10))+
    geom_sf(data = colony, shape = 24, size = 2.5, fill = "red") +
    #guides(fill = F) +
    labs(title = paste("Thick-billed murre migration:", yy), x = "", y = "") +
    theme(axis.text.y = element_text(angle = 90), text = element_text(size = 9), legend.text = element_text(size = 7)) 
  print(p)
  
  ggsave(paste0("E:/Geolocators/Plots/AnnualTracks_",sub("/","_",yy),".png"), units = "cm",
         width = 16, height = 10)

  readline("Next [enter]")
  
}

# -----

temp <- weekMean[weekMean$trackYear == "2007/08",]
for (j in unique(temp$week)) {
  temp$meanDate[temp$week == j] <- min(temp$meanDate[temp$week == j])
}

temp <- subset(temp, temp$meanDate > "2007-09-01" & temp$meanDate < "2008-06-01")
length(unique(temp$meanDate))

myGif <- ggplot() +
  geom_sf(data = coast, alpha = 1, fill = grey(0.9), size = 0.1) +
  geom_point(data = temp, aes(x = medianLon, y = medianLat, col = (Band)), 
             alpha = 0.85, show.legend = F, size = 4) +
  transition_states(meanDate, transition_length = 1, state_length = 1) +
  labs(title = 'Date: {closest_state}', x = '', y = '') +
  scale_color_viridis_d(direction = 1) +
  scale_x_continuous(expand = c(0,0), breaks = seq(-10,-110,-20))+
  scale_y_continuous(expand = c(0,0), breaks = seq(50,75,10))+
  geom_sf(data = colony, shape = 24, size = 2.5, fill = "red") +
  guides(fill = F) +
  theme(axis.text.y = element_text(angle = 90), text = element_text(size = 12))
animate(myGif, nframes = 500, fps = 5, width = 1200, height = 900)
anim_save("E:/Geolocators/plots/Migration_2007.gif")

# -----
