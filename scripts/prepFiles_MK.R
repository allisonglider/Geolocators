# This code estimates twilights using the GeoLight package and saves data in TAGS format for use in FLightR

library(BAStag)
library(GeoLight)
library(ggplot2)
library(TwGeos)
library(dplyr)
library(suncalc)
library(imputeTS)
library(probGLS)

coatsTwl <- getSunlightTimes(
  date = seq(as.Date("2007-07-01"), as.Date("2010-07-31"),1),
  lat = 62.95,
  lon = -82.01
)

coatsTwl <- coatsTwl %>% 
  dplyr::select(date, dawn, dusk)

tw <- twilight_error_estimation()

# -------------------------------------------------------------

setwd("E:/Geolocators/data/Coats_2007/99697041_5222")
rm(myData, temp, act)

theFiles <- list.files(pattern = "lig", full.names = T)
myData <- read.csv(theFiles, skip = 0, header = F)
names(myData) <- c("Valid","Timestamp","Date_num","Light")
head(myData)

theFiles <- list.files(pattern = "act", full.names = T)
if (length(theFiles)>0) {
  act <- read.csv(theFiles, skip = 0, header = F)
  names(act) <- c("Valid","Timestamp","Date_num","Act")
  myData <- merge(myData, act, all = T)
} else myData$Act <- NA

theFiles <- list.files(pattern = "tem", full.names = T)
if (length(theFiles)>0) {
  tem <- read.csv(theFiles, skip = 0, header = F)
  names(tem) <- c("Valid","Timestamp","Date_num","Temp")
  myData <- merge(myData, tem, all = T)
} else myData$Temp <- NA

myData$Timestamp <- as.POSIXct(strptime(myData$Timestamp, "%d/%m/%y %H:%M:%S"), tz = "GMT")
myData <- myData[order(myData$Timestamp),]
head(myData)

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
                       threshold = 5,
                       lmax = 70)
head(twl)

trn <- data.frame(
  tFirst = twl$Twilight[1:(nrow(twl) - 1)],
  tSecond= twl$Twilight[2:(nrow(twl))],
  type = ifelse(twl$Rise[1:(nrow(twl) - 1)] == T, 1, 2)
) 

# -------------------------------------------------------------

myData %>% 
  mutate(
    date = as.Date(Timestamp),
    dry = 600 - (Act * 3),
    dry = ifelse(Light > 60, dry, 0)) %>% 
  group_by(date) %>% 
  summarise(
    dry = sum(dry)/(60 * 60)
  ) %>% ggplot(aes(date, dry)) +
  geom_line()

# -------------------------------------------------------------


temp <- myData %>% 
  filter(Temp < 12, Temp >-2) %>% 
  mutate(date = as.Date(Timestamp, tz = "GMT")) %>% 
  dplyr::select(date, Temp) %>% 
  group_by(date) %>% 
  summarize(
    SST = median(Temp)
  ) %>% 
  mutate(
    SST.remove = F
  ) %>% 
  data.frame()

ggplot(temp, aes(date, SST)) +
  geom_line() +
  geom_point()

# -------------------------------------------------------------

pr   <- prob_algorithm(trn                         = trn,
                       sensor                      = temp,
                       act                         = NULL,
                       tagging.date                = min(trn$tFirst),
                       retrieval.date              = max(trn$tSecon),
                       loess.quartile              = NULL,
                       tagging.location            = c(-82.01,62.95),
                       particle.number             = 500,
                       iteration.number            = 50,
                       sunrise.sd                  = tw,
                       sunset.sd                   = tw,
                       range.solar                 = c(-7,0),
                       speed.wet                   = c(3, 5, 15),
                       speed.dry                   = c(3, 5, 15),
                       sst.sd                      = 0.1,      
                       max.sst.diff                = 1,         
                       days.around.spring.equinox  = c(21,14),  
                       days.around.fall.equinox    = c(14,21),
                       ice.conc.cutoff             = 0.9,
                       boundary.box                = c(-110,-10,30,75),
                       land.mask                   = T,
                       med.sea                     = T,       
                       black.sea                   = T,       
                       baltic.sea                  = T,     
                       caspian.sea                 = T,   
                       east.west.comp              = T,
                       wetdry.resolution           = 1,
                       NOAA.OI.location            = "E:\\Geolocators\\data\\envir")

# plot lat, lon, SST vs time ----
plot_timeline(pr,degElevation = NULL)

# plot lon vs lat map ----
plot_map(pr)

