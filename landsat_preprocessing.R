# install.packages('pROC')
# install.packages('raster')
# install.packages('RStoolbox')
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)

##### Look at distribution of data availability over timeframe of interest (1989-2005) #####
setwd('/Users/Avril/Documents/krat_remote_sensing/landsat_5_data_overviews/')
scenes.avail <- read.csv('LS5_C1L1_1989_to_2005.csv')

## examine data attributes
scenes.avail$Acquisition.Date <- as.Date(scenes.avail$Acquisition.Date)
table(scenes.avail$Day.Night.Indicator) ## only 1 night scene, rest are day
scenes.avail <- scenes.avail[-which(scenes.avail$WRS.Path==134),] ## get rid of the night scene that comes from a weird path #
all(scenes.avail$Land.Cloud.Cover == scenes.avail$Scene.Cloud.Cover) ## same information
hist(scenes.avail$Land.Cloud.Cover, breaks=30, col='grey')
table(scenes.avail$Sensor.Anomalies) ## 57 'BUMPER_MODE' anomalies (idk what this is)
hist(scenes.avail$Image.Quality, breaks=30, col='grey')
## plot availability of data over time (color by cloud cover)
scenes.avail$year <- as.numeric(do.call(rbind, strsplit(as.character(scenes.avail$Acquisition.Date), split='-', fixed=TRUE))[,1])
scenes.avail$doy <- as.numeric(strftime(scenes.avail$Acquisition.Date, format='%j'))
cloud.col <- 1-(scenes.avail$Scene.Cloud.Cover/100)
cloud.col[which(cloud.col==0)] <- 0.01

## all scenes
pdf('/Users/Avril/Desktop/landsat5_data_across_years.pdf', width=20, height=7)
par(mar=c(5.1, 5.1, 6.1, 2.1))
plot(scenes.avail$doy, scenes.avail$year, pch=19, cex=3, xlab='Day of Year', ylab='Year',
     col='transparent', ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  points(scenes.avail[scenes.avail$WRS.Path==34, 'doy'], scenes.avail[scenes.avail$WRS.Path==34, 'year'], pch=21, cex=3, xlab='Day of Year', ylab='Year',
         col='black', bg=alpha('blue', alpha=cloud.col), ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  points(scenes.avail[scenes.avail$WRS.Path==35, 'doy'], scenes.avail[scenes.avail$WRS.Path==35, 'year'], pch=22, cex=3, xlab='Day of Year', ylab='Year',
         col='black', bg=alpha('blue', alpha=cloud.col), ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  lines(x=c(91,91), y=c(1980, 2006), lty=2, lwd=2) ## April 1
  lines(x=c(305,305), y=c(1980, 2006), lty=2, lwd=2) ## November 1
  text(c(91, 305), c(2006.5, 2006.5), labels=c('Apr 1', 'Nov 1'), cex=1.5)
  legend((365/3), 2012, xpd=TRUE, fill=c('blue', alpha('blue', alpha=0.75), alpha('blue', alpha=0.50), alpha('blue', alpha=0.25), alpha('blue', alpha=0.01)),
         legend=c('0%','25%','50%','75%','100%'), title='Scene Cloud Cover', horiz=TRUE, xjust=0.5, cex=1.5)
  legend((365*2/3), 2012, xpd=TRUE, pch=c(21,22), legend=c('34', '35'), title='Path Number', horiz=TRUE, xjust=0.5, 
         cex=1.5, pt.bg='black', pt.cex=3, text.width=12, x.intersp=1.8)
dev.off()

## scenes with 0% cloud cover
temp <- scenes.avail[which(scenes.avail$Land.Cloud.Cover == 0),]
cloud.col <- 1-(temp$Scene.Cloud.Cover/100)
cloud.col[which(cloud.col==0)] <- 0.01
pdf('/Users/Avril/Desktop/landsat5_data_across_years_zerocloudcover.pdf', width=20, height=7)
par(mar=c(5.1, 5.1, 6.1, 2.1))
plot(temp$doy, temp$year, pch=19, cex=3, xlab='Day of Year', ylab='Year',
     col='transparent', ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  points(temp[temp$WRS.Path==34, 'doy'], temp[temp$WRS.Path==34, 'year'], pch=21, cex=3, xlab='Day of Year', ylab='Year',
         col='black', bg=alpha('blue', alpha=cloud.col), ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  points(temp[temp$WRS.Path==35, 'doy'], temp[temp$WRS.Path==35, 'year'], pch=22, cex=3, xlab='Day of Year', ylab='Year',
         col='black', bg=alpha('blue', alpha=cloud.col), ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  lines(x=c(91,91), y=c(1980, 2006), lty=2, lwd=2) ## April 1
  lines(x=c(305,305), y=c(1980, 2006), lty=2, lwd=2) ## November 1
  text(c(91, 305), c(2006.5, 2006.5), labels=c('Apr 1', 'Nov 1'), cex=1.5)
  legend((365/3), 2012, xpd=TRUE, fill=c('blue', alpha('blue', alpha=0.75), alpha('blue', alpha=0.50), alpha('blue', alpha=0.25), alpha('blue', alpha=0.01)),
         legend=c('0%','25%','50%','75%','100%'), title='Scene Cloud Cover', horiz=TRUE, xjust=0.5, cex=1.5)
  legend((365*2/3), 2012, xpd=TRUE, pch=c(21,22), legend=c('34', '35'), title='Path Number', horiz=TRUE, xjust=0.5, 
         cex=1.5, pt.bg='black', pt.cex=3, text.width=12, x.intersp=1.8)
dev.off()

##### Try looking at intra-annual variation in Level 2 data for 1999 #####
## set wd for test directory - can run as loop over multiple directories later
setwd('/Users/Avril/Documents/krat_remote_sensing/landsat_5_downloads/LT05_L1TP_035038_20040611_20160914_01_T1/')

# setwd('/Users/Avril/Documents/krat_remote_sensing/archive/test_1999_C2_L1_data/')
# dirs <- list.files()

# pdf(file='/Users/Avril/Desktop/1999.pdf', width=6, height=6)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))

# for(i in dirs){
#   setwd(paste0('/Users/Avril/Documents/krat_remote_sensing/archive/test_1999_C2_L1_data/',i,'/'))
  ## read in data
  all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"), full.names = TRUE)
  if(length(all_landsat_bands) != 0){ ## if files haven't been renamed yet, rename them
    ## get new filenames and rename TIF files - filenames must == band names in metadata file or radCor() will fail
    new.names <- do.call(rbind, strsplit(all_landsat_bands, split='_', fixed=TRUE))[,8]
    new.names <- paste0(gsub('.TIF','',new.names),'_dn')
    file.rename(all_landsat_bands, new.names)
    ls5.stack <- stack(new.names)
  } else{ ## otherwise, they've already been renamed and can be read in with this loop
    new.names <- list.files(pattern = glob2rx("*_dn$"), full.names = TRUE)
    ls5.stack <- stack(new.names)
  }
  ## read in metadata
  md.file <- list.files(pattern=glob2rx("*MTL.txt"), full.names=TRUE)
  m.data <- readMeta(md.file) ## works for LS5 Collection 1 Level 1 data - errors with Collection 2 Level 1
  
  ## apply TOA correction - 'apref' = apparent reflectance (top-of-atmosphere reflectance)
  ls5.cor.stack <- radCor(ls5.stack, m.data, method='apref', verbose=TRUE, bandSet=m.data$DATA$BANDS)
  
  ## manually select the Landsat5TM bands you need for Tasseled Cap (1,2,3,4,5,7)
  tc.cor.stack <- raster::subset(ls5.cor.stack, subset=c('B1_tre','B2_tre','B3_tre','B4_tre','B5_tre','B7_tre'))
  ## apply the Tasseled Cap transformation
  ls5.tc.cor <- tasseledCap(tc.cor.stack, sat='Landsat5TM')
  # plot(ls5.tc.cor)
  
  # ## get a natural color image
  # raster::plotRGB(ls5.stack,r=3,g=2,b=1)
  
  ## set coordinates
  lo.x <- 663500
  hi.x <- 665600
  lo.y <- 3497000
  hi.y <- 3499750
  
  # par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
  # plot(ls5.tc.cor$wetness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n')
  # pdf(file='/Users/Avril/Desktop/test.pdf', width=6, height=6)
  # par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
  
  ## create consistent legend/color scale across figures within a measure (w/g/b)
  # pal <- viridis ## other options: viridis  magma   plasma  inferno  cividis
  # 
  # cuts <- seq(from=-1, to=1, by=.001) ## covers range of wetness values for 1999
  # plot(ls5.tc.cor$wetness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main=paste0('wetness\n',i),
  #      breaks=cuts, col=pal(length(cuts)))
  # 
  # cuts <- seq(from=-1, to=1, by=.001) ## covers range of greenness values for 1999
  # plot(ls5.tc.cor$greenness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main=paste0('greenness\n',i),
  #      breaks=cuts, col=pal(length(cuts)))
  # 
  # cuts <- seq(from=0, to=2, by=.001) ## covers range of brightness values for 1999
  # plot(ls5.tc.cor$brightness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main=paste0('brightness\n',i),
  #      breaks=cuts, col=pal(length(cuts)))

  plot(ls5.tc.cor$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', 
       xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='greenness')
  plot(ls5.tc.cor$brightness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n',
       xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='brightness')
  plot(ls5.tc.cor$wetness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n',
       xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='brightness')
  # dev.off()
  
  # print(i)
# }

dev.off()







