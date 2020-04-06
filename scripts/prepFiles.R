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
  date = seq(as.Date("2017-07-01"), as.Date("2020-07-31"),1),
  lat = 62.95,
  lon = -82.01
)

coatsTwl <- coatsTwl %>% 
  dplyr::select(date, dawn, dusk)

#####
depDates <- read.csv("C:/Users/Allison/Documents/PhD research/Coats 2019 Data/TBMU GLS Data/Analysis/Deployment_Dates_2019.csv",
                     stringsAsFactors = F)
depDates$Deploy.Off <- as.Date(depDates$Deploy.Off)
depDates$Deploy.On <- as.Date(depDates$Deploy.On)
depDates$leaveColony <- as.Date(depDates$leaveColony)

# -------------------------------------------------------------

setwd("E:/Geolocators/raw")
theFiles <- list.files()

# Light threshold, I've found 175 works well at getting the basic location estimates close to the colony
LT <- 225

# -------------------------------------------------------------

i = 44

# -------------------------------------------------------------

# Extract the band number from the file name
birdID <- as.numeric(strsplit(theFiles[i], "_")[[1]][5])
tagID <- as.numeric(sub(".csv","",strsplit(theFiles[i], "_")[[1]][2]))
idx <- which(depDates$Band == birdID & depDates$GLS == tagID)

# -------------------------------------------------------------

# Read in data
myData <- read.csv(theFiles[i], skip = 2, header = T)
names(myData) <- c("FID","Date","Time","Pressure","Temp","Light","Dry","Dim","Valid")
myData$Timestamp <- as.POSIXct(paste(myData$Date, myData$Time), tz = "GMT")

k <- nrow(myData)
while(myData$Valid[k] == 0) k <- k - 1
myData <- myData[1:k,]
myData$Light[myData$Light > 500] <- NA
myData$Light <- na_interpolation(myData$Light)

myData <- subset(myData, myData$Timestamp > as.POSIXct(depDates$leaveColony[idx]))
myData$Date <- as.Date(myData$Date, tz = "GMT")

if (birdID == 99696840 & tagID == 687) myData <- subset(myData, myData$Timestamp < as.POSIXct("2018-11-01"))
if (birdID == 99696998 & tagID == 715) myData$Timestamp  <- (myData$Timestamp - 60 * 60 * 7)

# -------------------------------------------------------------

# Select just the date and light
light <- myData[,c("Timestamp","Light")]
names(light) <- c("Date","Light")

light <- light %>% 
  mutate(
    idx = ceiling(as.numeric(Date)/(300))
    ) %>% 
  group_by(idx) %>% 
  summarize(
    Date = max(Date),
    Light = max(Light)
  ) %>% 
  ungroup() %>% 
  dplyr::select(Date, Light) %>% 
  data.frame()

# -------------------------------------------------------------

# -------------------------------------------------------------

# Look at light levels throughout the year
lightImage(light, offset = -5, zlim = c(50, 400), dt = 10) # dt specifies the recording interval
tsimageLines(coatsTwl$dawn, offset = -5, col = "orange", lwd = 2)
tsimageLines(coatsTwl$dusk, offset = -5, col = "blue", lwd = 2)

# -------------------------------------------------------------

rm(twl)
twl <- preprocessLight(light, 
                       threshold = LT,
                       lmax = 400)
head(twl)

# -------------------------------------------------------------

temp <- myData %>% 
  filter(Dry == 0, Temp < 12, Temp >-2, Pressure < 10, Valid == 1) %>% 
  mutate(date = as.Date(Date, tx = "GMT")) %>% 
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

act <- myData %>% 
  mutate(
    dtime  = Timestamp,
    duration = 10,
    wet.dry = ifelse(Dry == 1 & Temp < 5 &  Valid == 1, "dry", "wet"),
    wetdry = duration
  ) %>% 
  dplyr::select(dtime, duration, wet.dry, wetdry) %>% 
  filter(wet.dry == "wet") %>% 
  data.frame()

# -------------------------------------------------------------
trn <- data.frame(
  tFirst = twl$Twilight[1:(nrow(twl) - 1)],
  tSecond= twl$Twilight[2:(nrow(twl))],
  type = ifelse(twl$Rise[1:(nrow(twl) - 1)] == T, 1, 2)
) 

# -------------------------------------------------------------

# Save file
save(light, temp, twl, trn, act, myData, file = paste0("E:/Geolocators/analysis/processed/", birdID, "_0", tagID,".RData"))
  
