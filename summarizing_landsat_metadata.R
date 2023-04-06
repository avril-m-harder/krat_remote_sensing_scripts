# install.packages('pROC')
# install.packages('raster')
# install.packages('RStoolbox')
library(sp)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(TeachingDemos)
source('/Users/Avril/Documents/krat_remote_sensing/krat_remote_sensing_scripts/03b_c2l2readMeta.R')
`%notin%` <- Negate(`%in%`)

## read in list of final scenes used (n = 167)
scenes <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/final_list_of_landsat_scenes.csv')

## loop over directories of raw data to get metadata information
setwd('/Volumes/avril_data/nsf_postdoc_data/krat_remote_sensing/C2L2_raw_landsat45tm_scene_downloads/')
dirs <- scenes$prod.id

## just get acquisition times
OUT <- NULL
for(i in 1:length(dirs)){
  setwd(paste0('/Volumes/avril_data/nsf_postdoc_data/krat_remote_sensing/C2L2_raw_landsat45tm_scene_downloads/',dirs[i],'/'))
  ## read in metadata
  md.file <- list.files(pattern=glob2rx("*MTL.txt"), full.names=TRUE)
  m.data <- c2l2readMeta(md.file) ## uses an edited version of readMeta to read in metadata from C2L2-formatted *_MTL.txt files
  
  ## get acquisition times
  time <- strsplit(as.character(m.data$ACQUISITION_DATE), split = ' ', fixed = TRUE)[[1]][2]
  hour <- strsplit(time, split = ':', fixed = TRUE)[[1]][1]
  min <- strsplit(time, split = ':', fixed = TRUE)[[1]][2]
  sec <- strsplit(time, split = ':', fixed = TRUE)[[1]][3]
  
  save <- c(scenes$scene.id[i], hour, min, sec)
  OUT <- rbind(OUT, save)
}
dat <- as.data.frame(OUT) 
colnames(dat) <- c('scene.id','hour','minute','second')
dat$mst.hour <- as.numeric(dat$hour) - 7 ## always the same because AZ never changes time zones outside the Navajo Nation
write.csv(dat, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/final_scenes_acq_time.csv', row.names = FALSE)

## write all necessary info for SI metadata table
OUT <- NULL
for(i in 1:length(dirs)){
  setwd(paste0('/Volumes/avril_data/nsf_postdoc_data/krat_remote_sensing/C2L2_raw_landsat45tm_scene_downloads/',dirs[i],'/'))
  ## read in metadata
  md.file <- list.files(pattern=glob2rx("*MTL.txt"), full.names=TRUE)
  m.data <- c2l2readMeta(md.file) ## uses an edited version of readMeta to read in metadata from C2L2-formatted *_MTL.txt files
  
  ## get date info
  date <- strsplit(as.character(m.data$ACQUISITION_DATE), split = ' ', fixed = TRUE)[[1]][1]
  year <- strsplit(date, split = '-', fixed = TRUE)[[1]][1]
  month <- strsplit(date, split = '-', fixed = TRUE)[[1]][2]
  day <- strsplit(date, split = '-', fixed = TRUE)[[1]][3]
  
  ## get acquisition times
  time <- strsplit(as.character(m.data$ACQUISITION_DATE), split = ' ', fixed = TRUE)[[1]][2]
  hour <- strsplit(time, split = ':', fixed = TRUE)[[1]][1]
  hour <- as.numeric(hour) - 7 ## always the same offset from GMT because AZ never changes time zones outside the Navajo Nation
  min <- strsplit(time, split = ':', fixed = TRUE)[[1]][2]
  sec <- strsplit(time, split = ':', fixed = TRUE)[[1]][3]
  
  save <- c(dirs[i], scenes$scene.id[i], year, month, day, hour, min, sec)
  OUT <- rbind(OUT, save)
}
dat <- as.data.frame(OUT)
colnames(dat) <- c('prod.id','scene.id','year','month','day','hour','min','sec')

write.csv(dat, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/SI_metadata.csv', row.names = FALSE)
