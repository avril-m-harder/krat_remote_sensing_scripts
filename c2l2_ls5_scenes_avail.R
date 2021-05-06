# install.packages('pROC')
# install.packages('raster')
# install.packages('RStoolbox')
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)

# ##### Read in mound waypoints for later extraction #####
# mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
# mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
# mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
# coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
# proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84") 
# mnd.locs <- spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

##### Look at distribution of data availability over timeframe of interest (1989-2005) #####
setwd('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/')
scenes.avail <- read.csv('landsat_tm_c2_l2_all_scenes_avail.csv')

## examine data attributes
par(mar=c(5.1,4.1,4.1,2.1))
scenes.avail$Date.Acquired <- as.Date(scenes.avail$Date.Acquired)
table(scenes.avail$Day.Night.Indicator) 
all(scenes.avail$Land.Cloud.Cover == scenes.avail$Scene.Cloud.Cover) ## same information
hist(scenes.avail$Land.Cloud.Cover, breaks=30, col='grey')
table(scenes.avail$Sensor.Mode) ## 46 in "BUMPER" mode (idk what this is)
table(scenes.avail$Data.Type.L2) ## all are TM_L2SP
hist(scenes.avail$Image.Quality, breaks=30, col='grey')
## plot availability of data over time (color by cloud cover)
scenes.avail$year <- as.numeric(do.call(rbind, strsplit(as.character(scenes.avail$Date.Acquired), split='-', fixed=TRUE))[,1])
scenes.avail$doy <- as.numeric(strftime(scenes.avail$Date.Acquired, format='%j'))
cloud.col <- 1-(scenes.avail$Scene.Cloud.Cover/100)
cloud.col[which(cloud.col==0)] <- 0.01

## write list of scenes with cloud cover <20%
write.table(scenes.avail[which(scenes.avail$Land.Cloud.Cover <= 20),2], quote=FALSE,
            '/Users/Avril/Desktop/C2L2_low_cloud_scenes.txt', sep='\t', row.names=FALSE, col.names=FALSE)
## write list of cloud-free scenes
write.table(scenes.avail[which(scenes.avail$Land.Cloud.Cover == 0),2], quote=FALSE,
            '/Users/Avril/Desktop/C2L2_cloud_free_scenes.txt', sep='\t', row.names=FALSE, col.names=FALSE)

## large plots showing scene availability across doy and years (20 x 7)
## all scenes
pdf('/Users/Avril/Desktop/C2L2_landsat5_data_across_years.pdf', width=20, height=7)
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
pdf('/Users/Avril/Desktop/C2L2_landsat5_data_across_years_zerocloudcover.pdf', width=20, height=7)
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
