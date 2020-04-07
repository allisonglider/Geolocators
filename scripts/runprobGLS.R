library(probGLS)

folder <- "E:/Geolocators/analysis/processed"
theFiles <- list.files(folder)
tw <- twilight_error_estimation()
gE <- -3.5

# -------------------------------------------------------------

for (i in c(5, 25, 36, 37, 46, 47, 50, 51, 59)) {
  
  idx <- sub(".RData", "", theFiles[i])
  
  # Save file
  load(paste0("E:/Geolocators/analysis/processed/", theFiles[i]))
  
  pr   <- prob_algorithm(trn                         = trn,
                         sensor                      = temp,
                         act                         = act,
                         tagging.date                = min(trn$tFirst),
                         retrieval.date              = max(trn$tSecon),
                         loess.quartile              = NULL,
                         tagging.location            = c(-82.01,62.95),
                         particle.number             = 1000,
                         iteration.number            = 500,
                         sunrise.sd                  = tw,
                         sunset.sd                   = tw,
                         range.solar                 = c(-7,0),
                         speed.wet                   = c(1 , 1.3, 5),
                         speed.dry                   = c(15, 5, 25),
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
                         NOAA.OI.location            = "C:\\Users\\Allison\\Documents\\PhD research\\Coats 2019 Data\\TBMU GLS Data\\Analysis\\envir")
  
  # plot lat, lon, SST vs time ----
  plot_timeline(pr,degElevation = NULL)
  
  png(file=paste0("E:/Geolocators/plots/probGLS/",idx,"_LL.png"), width=6.5, height=8, units="in", res=300)
  plot_timeline(pr,degElevation = NULL)
  dev.off()
  
  # plot lon vs lat map ----
  plot_map(pr)
  
  png(file=paste0("E:/Geolocators/plots/probGLS/",idx,"_Map.png"), width=6.5, height=8, units="in", res=300)
  plot_map(pr)
  dev.off()
  
  output <- data.frame(pr[[1]])
  
  saveRDS(pr, paste0("E:/Geolocators/analysis/probGLS/",idx,".RDS"))
  write.csv(output, paste0("E:/Geolocators/analysis/probGLS/",idx,".csv"), row.names = F)
}

