# This code estimates twilights using the GeoLight package and saves data in TAGS format for use in FLightR

library(BAStag)
library(GeoLight)
library(ggplot2)
library(TwGeos)
library(dplyr)
library(suncalc)
library(imputeTS)
library(probGLS)
library(xlsx)

coatsTwl <- getSunlightTimes(
  date = seq(as.Date("2007-07-01"), as.Date("2010-07-31"),1),
  lat = 62.95,
  lon = -82.01
)

coatsTwl <- coatsTwl %>% 
  dplyr::select(date, dawn, dusk)

tw <- twilight_error_estimation()

# -------------------------------------------------------------

dep <- read.xlsx("E:/Geolocators/data/Coats Deployments_2007_2009.xlsx", sheetIndex = 3, stringsAsFactors = F)

dep$Deployment.Start.GMT <- as.POSIXct(dep$Deployment.Start.GMT, tz = "GMT")

# -------------------------------------------------------------

folders <- list.dirs("E:/Geolocators/data/Coats_2007_2009")[-1]
tags <- list.dirs("E:/Geolocators/data/Coats_2007_2009", full.names = F)[-1]

# -------------------------------------------------------------

i = 1

# -------------------------------------------------------------
setwd(folders[i])
tagID <- tags[i]

idx <- which(dep$Device == tagID)
print(idx)

rm(myData, light, temp)

theFiles <- list.files(pattern = "lig", full.names = T)
myData <- read.csv(theFiles, skip = 0, header = F)
names(myData) <- c("Valid","Timestamp","Date_num","Light")
head(myData)

theFiles <- list.files(pattern = "tem", full.names = T)
if (length(theFiles) > 0) {
  tem <- read.csv(theFiles, skip = 0, header = F)
  names(tem) <- c("Valid","Timestamp","Date_num","Temp")
  myData <- merge(myData, tem, all = T)
} else myData$Temp <- NA

myData$Timestamp <- as.POSIXct(strptime(myData$Timestamp, "%d/%m/%y %H:%M:%S"), tz = "GMT")
myData <- myData[order(myData$Timestamp),]
myData <- subset(myData, myData$Timestamp >= dep$Deployment.Start.GMT[idx])
head(myData)
summary(myData$Temp)

# -------------------------------------------------------------

light <- myData[!is.na(myData$Light),c("Timestamp","Light")]
names(light) <- c("Date","Light")

# Look at light levels throughout the year
lightImage(light, offset = -5, zlim = c(0, 70), dt = 10 * 60) # dt specifies the recording interval
tsimageLines(coatsTwl$dawn, offset = -5, col = "orange", lwd = 2)
tsimageLines(coatsTwl$dusk, offset = -5, col = "blue", lwd = 2)

# -------------------------------------------------------------

rm(twl)
twl <- preprocessLight(light, 
                       threshold = 15,
                       lmax = 70)
head(twl)

trn <- data.frame(
  tFirst = twl$Twilight[1:(nrow(twl) - 1)],
  tSecond= twl$Twilight[2:(nrow(twl))],
  type = ifelse(twl$Rise[1:(nrow(twl) - 1)] == T, 1, 2)
) 

# -------------------------------------------------------------

if (!is.infinite(max(myData$Temp, na.rm = T))) {
temp <- myData %>% 
  filter(Temp < 12, Temp >-2) %>% 
  mutate(date = as.Date(Timestamp, tz = "GMT")) %>% 
  dplyr::select(date, Temp) %>% 
  group_by(date) %>% 
  summarize(
    SST = median(Temp),
    n = n()
  ) %>% 
  ungroup() %>% 
  mutate(
    SST.remove = F,
    SST = na_interpolation(SST)
  ) %>% 
  data.frame()

  p <- ggplot(temp, aes(date, SST)) +
    geom_line() +
    geom_point()
  print(p)
}

# -------------------------------------------------------------

if (exists("temp")) {
  save(light, temp, twl, trn, myData, file = paste0("E:/Geolocators/analysis/processed/Coats_2007_2009/", dep$Band[idx], "_", dep$Device[idx],".RData"))
} else {
  save(light, twl, trn, myData, file = paste0("E:/Geolocators/analysis/processed/Coats_2007_2009/", dep$Band[idx], "_", dep$Device[idx],".RData"))
}

# -------------------------------------------------------------
