library(probGLS)

years <- "Coats_2007_2009"

folder <- paste0("E:/Geolocators/analysis/processed/",years)
theFiles <- list.files(folder)
tw <- twilight_error_estimation()
gE <- -3.5

# -------------------------------------------------------------

for (i in c(4, 9, 17, 24, 26, 20)) {
  
  if (exists("temp")) rm(temp)
  if (exists("act")) rm(act)
  if (exists("trn")) rm(trn)
  
  load(paste0(folder,"/",theFiles[i]))
  
  idx <- sub(".RData", "", theFiles[i])
  
  #getElevation(twl = trn[1:20,], known.coord = c(-82.01,62.95))
  
  pr   <- prob_algorithm(trn                         = trn,
                         sensor                      = if (exists("temp")) temp else (NULL),
                         act                         = if (exists("act")) act else (NULL),
                         tagging.date                = min(trn$tFirst),
                         retrieval.date              = max(trn$tSecon),
                         loess.quartile              = NULL,
                         tagging.location            = if (years == "Digges_2008_2010") c(-77.72, 62.56) else c(-82.01, 62.95),
                         particle.number             = 2000,
                         iteration.number            = 100,
                         sunrise.sd                  = tw,
                         sunset.sd                   = tw,
                         range.solar                 = c(-6,-1),
                         speed.wet                   = if (exists("act")) c(1, 1.3, 5) else c(3, 5, 15),
                         speed.dry                   = if (exists("act")) c(15, 5, 25) else c(3, 5, 15),
                         sst.sd                      = 0.1,      
                         max.sst.diff                = 2,         
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
  
  png(file=paste0("E:/Geolocators/plots/probGLS/",years,"/",idx,"_LL.png"), width=6.5, height=8, units="in", res=300)
  plot_timeline(pr,degElevation = NULL)
  dev.off()
  
  # plot lon vs lat map ----
  plot_map(pr)
  
  png(file=paste0("E:/Geolocators/plots/probGLS/",years,"/",idx,"_Map.png"), width=6.5, height=8, units="in", res=300)
  plot_map(pr)
  dev.off()
  
  output <- data.frame(pr[[1]])
  
  saveRDS(pr, paste0("E:/Geolocators/analysis/probGLS/",years,"/",idx,".RDS"))
  write.csv(output, paste0("E:/Geolocators/analysis/probGLS/",years,"/",idx,".csv"), row.names = F)
}

