# install.packages('pROC')
# install.packages('raster')
# install.packages('RStoolbox')
# install.packages('TrackReconstruction')
library(raster)
library(rgdal)
library(RStoolbox)
library(viridis)
library(sp)
library(TrackReconstruction)
`%notin%` <- Negate(`%in%`)

## Read in some Landsat 7 data
setwd('/Users/Avril/Documents/krat_remote_sensing/archive/proposal_sat_map_stuff/LE07_L1TP_035038_20020817_20160928_01_T1/')
all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"),
                                full.names = TRUE)

# ## Read in some Landsat 5 (C2 L1) data for visualizing data resolution on the ground  ------------------
# setwd('/Users/Avril/Desktop/remote_sensing_data/1999_C2_L1_data/LT05_L1TP_035038_19990105_20200908_02_T1/')
# all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"),
#                                 full.names = TRUE)

# test <- raster(all_landsat_bands[1])
# plot(test, col=gray(0:100 / 100)) ## plot it to see what it looks like in greyscale

## manually select the Landsat5ETM bands you need for Tasseled Cap (1,2,3,4,5,7)
crit_bands <- all_landsat_bands[c(1:5,7)]
## stack the images
ls5_stack <- stack(crit_bands)
# ## turn it into a brick
# ls5_brick <- brick(ls5_stack)

#### plot site
## set extent variables
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750
## make extent object
ext <- extent(lo.x, hi.x, lo.y, hi.y)
## crop image to extent
ls5_stack <- crop(ls5_stack, ext)
## get a natural color image at 30-m resolution
raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))

#
# ##### 1. Plotting points from the 2 waypoint files separately #####
# #### try adding waypoints to raster image
# ## read in smaller file of waypoints
# setwd('/Users/Avril/Desktop/remote_sensing_data/')
# pts <- read.csv('1-25-2021_Rucker_GPS.csv', header=FALSE)
# colnames(pts) <- c('lat','long','terr')
# pts <- pts[,c(2,1,3)]
# coordinates(pts) <- c('long','lat') ## converts to SpatialPointsDataFrame object
# proj4string(pts) <- CRS("+proj=longlat +datum=WGS84") 
# pts.plot <- spTransform(pts, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# ## plot
# # pdf('/Users/Avril/Desktop/1-25-2021_Rucker_GPS.pdf', width=8, height=8)
# par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
# raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
#   points(pts.plot, pch=19, col='yellow')
#   text(pts.plot, labels=pts.plot$terr, adj=c(0,1), col='white')
# # dev.off() ## matches up with Google Earth view!
# 
# #### try with larger file of waypoints
# l.pts <- read.csv('Rucker.csv', header=FALSE)
# colnames(l.pts) <- c('lat','long','terr')
# l.pts <- l.pts[,c(2,1,3)]
# coordinates(l.pts) <- c('long','lat') ## converts to SpatialPointsDataFrame object
# proj4string(l.pts) <- CRS("+proj=longlat +datum=WGS84") 
# gps.pts.plot <- spTransform(l.pts, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# ## plot
# # pdf('/Users/Avril/Desktop/Rucker.pdf', width=8, height=8)
# par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
# raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
#   points(gps.pts.plot, pch=19, cex=0.5, col='yellow')
#   # text(gps.pts.plot, labels=gps.pts.plot$terr, adj=c(0,1), col='white')
# # dev.off() ## also matches up with Google Earth view!
# 
# #### merge the two sets of points to see if the same mounds are marked > once
# raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
#   points(pts.plot[which(pts.plot$terr %in% gps.pts.plot$terr),], cex=0.5, col='green')
#   points(gps.pts.plot[which(gps.pts.plot$terr %in% pts.plot$terr),], cex=0.5, col='blue') ## overlay pretty well,
# ## but try to calculate distance between points in meters
# shared <- rbind(pts.plot[which(pts.plot$terr %in% gps.pts.plot$terr),],
#                 gps.pts.plot[which(gps.pts.plot$terr %in% pts.plot$terr),])
# spDistsN1(shared[1,], shared[3,]) ## 0
# spDistsN1(shared[2,], shared[4,]) ## 0
  
##### 2. Combining points in 2 waypoint files Peter sent to see how many mounds are un-GPS-marked #####
setwd('/Users/Avril/Documents/krat_remote_sensing/raw_data/')
pts <- read.csv('1-25-2021_Rucker_GPS.csv', header=FALSE) ## read in smaller file
l.pts <- read.csv('Rucker.csv', header=FALSE) ## read in larger file
gps.pts <- rbind(pts, l.pts) ## combine them
gps.pts <- gps.pts[!duplicated(gps.pts),] ## get rid of duplicate waypoints
colnames(gps.pts) <- c('lat','long','terr')
gps.pts <- gps.pts[,c(2,1,3)]
gps.pts.plot <- gps.pts ## save as new object prior to format conversions
coordinates(gps.pts.plot) <- c('long','lat') ## converts to SpatialPointsDataFrame object
proj4string(gps.pts.plot) <- CRS("+proj=longlat +datum=WGS84") 
gps.pts.plot <- spTransform(gps.pts.plot, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
## plot all known waypoints
pdf('/Users/Avril/Desktop/gps_locs_LARGE.pdf', width=200, height=200)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
  points(gps.pts.plot, pch=19, cex=2, col='yellow')
  text(gps.pts.plot, labels=gps.pts.plot$terr, col='white', adj=c(0,1.8)) ## useful when plotting large file (200 x 200)
  text(lo.x, hi.y, labels='All mounds with GPS locations', adj=c(0,1), col='yellow', cex=1.5)
dev.off()
## plot with cell numbers to assign mounds without GPS coordinates to cells (added 7/12/21)
temp.layer <- ls5_stack$LE07_L1TP_035038_20020817_20160928_01_T1_B1
temp.layer <- setValues(temp.layer, 1:ncell(temp.layer))
pdf('/Users/Avril/Desktop/gps_locs_LARGE.pdf', width=200, height=200)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
  points(gps.pts.plot, pch=19, cex=2, col='yellow')
  text(gps.pts.plot, labels=gps.pts.plot$terr, col='white', adj=c(0,1.8)) ## useful when plotting large file (200 x 200)
  text(lo.x, hi.y, labels='All mounds with GPS locations', adj=c(0,1), col='yellow', cex=1.5)
  text(temp.layer)
dev.off()
  
## read in individual krat data to ID mounds of interest
krats <- read.csv('/Users/Avril/Documents/krats/krat_data_and_paper2/summary_allstats2_withgen_zeroes.csv')
## latest year to examine would be 2005 (data run through 2007, so 2005 would catch MOST offspring of examined indivs)
krats <- krats[krats$birthyear <= 2005,]
# krats <- krats[krats$genback > 3,]
par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))
hist(krats$birthyear, col='grey') ## earliest year is 1989; could examine fitness x environment for that year, 
                 ## but without inbreeding data for indivs in early years; keeping for now
mounds <- as.character(unique(krats$terr)) ## list of mounds we need location info for (n=210; n=164 if genback>3)
length(mounds[mounds %notin% gps.pts$terr]) ## 132 mounds do not have GPS-marked waypoints (116 if genback>3)
unmarked <- mounds[mounds %notin% gps.pts$terr]
## write list of mounds without GPS waypoints
# write.csv(mounds[mounds %notin% gps.pts$terr], '/Users/Avril/Desktop/unmarked_mounds.csv', row.names=FALSE)
##### ! On closer inspection, it looks like "unmarked" mound names in the database file might actually be covered in the waypoint file, with subpopulation names appended to the end of the mound names #####
## plot locations of mounds of interest with known GPS locations (i.e., names match EXACTLY between waypoint and database files)
# pdf('/Users/Avril/Desktop/mound_gps_locs.pdf', width=8, height=8)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
  points(gps.pts.plot[gps.pts.plot$terr %in% mounds,], pch=19, cex=0.5, col='yellow')
  text(lo.x, hi.y, labels='Mounds of interest with GPS locations', adj=c(0,1), col='yellow', cex=1.5)
# dev.off()

##### 3. Dealing with database location measurements #####
##### 3A. Plot database meter values #####
mnd.locs <- read.csv('/Users/Avril/Documents/krats/krat_data_and_paper2/KRATP.csv')
mnd.locs <- mnd.locs[,c('lat','long','terr')] ## only keep mound location information
mnd.locs <- mnd.locs[!duplicated(mnd.locs),] ## keep unique rows only
colnames(mnd.locs) <- c('long','lat','terr') ## lat/long column headings reversed in original file
int.locs <- mnd.locs[mnd.locs$terr %in% mounds,]
# pdf('/Users/Avril/Desktop/database_locs.pdf', width=8, height=8)
par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))
plot(c(0, mnd.locs$long), c(0, mnd.locs$lat), pch=19, cex=0.5, col='blue', main='Mounds of interest',
     xlab='Database long position (m)', ylab='Database lat position (m)')
  polygon(x=c(-1500,300,300,-1500), y=c(-1500,-1500,200,200), col='#424148')
  points(0, 0, pch=13, cex=1, col='springgreen')
  points(int.locs, pch=19, col='lavender', cex=0.5) ## all points
  points(int.locs[int.locs$terr %in% gps.pts.plot$terr, 'long'], int.locs[int.locs$terr %in% gps.pts.plot$terr, 'lat'],
         pch=19, col='yellow', cex=0.5) ## points with GPS information
  legend('bottomright', legend=c('reference point','with GPS info','without GPS info'), box.col='white',
         col=c('springgreen','yellow','lavender'), pch=c(13,19,19), bg='#424148', text.col='white')
# dev.off()

#
# ##### 3B. Calculate GPS coords for database reference point #####
# db.locs <- mnd.locs[mnd.locs$terr %in% gps.pts.plot$terr,] ## narrow down to database mounds with known GPS locations
# db.locs <- db.locs[!duplicated(db.locs),] ## n=96 unique mounds
# gps.locs <- gps.pts.plot[gps.pts.plot$terr %in% db.locs$terr,] ## narrow down waypoints to database mounds
# 
# ## db.locs provides info on distance of each mound in meters from some reference point;
# ## gps.locs provides GPS coords for matching list of mounds;
# ## will try to use db.locs to identify reference point, obtain coords for that reference,
# ## and use that value to plot the rest of the mounds without GPS coords.
# 
# calc.locs <- as.data.frame(gps.locs@coords)
# calc.locs$terr <- as.character(gps.locs$terr) ## position (in meters) of GPS-located mounds
# colnames(db.locs) <- c('long.dist','lat.dist','terr')
# calc.locs <- merge(x=calc.locs, y=db.locs, by='terr') ## combine known locations with dist-to-ref information
# 
# ## calculate distance (hypotenuse) to reference point
# calc.locs$distance <- sqrt(calc.locs$long.dist^2 + calc.locs$lat.dist^2)
# ## calculate bearing in degrees (SOH, sin = opposite / hypotenuse)
# calc.locs$rad.bear <- asin(abs(calc.locs$long.dist / calc.locs$distance))
# calc.locs$deg.bear <- calc.locs$rad.bear * 180 / pi
# 
# ## use GPS coords (in degrees) to calculate coords of reference point
# l.pts <- read.csv('Rucker.csv', header=FALSE)
# colnames(l.pts) <- c('lat','long','terr')
# l.pts <- l.pts[,c(2,1,3)]
# l.pts <- l.pts[l.pts$terr %in% calc.locs$terr,] ## narrow down to mounds with GPS coords
# colnames(calc.locs)[2:3] <- c('long.m','lat.m')
# calc.locs <- merge(x=calc.locs, y=l.pts, by='terr')
# ## calculate reference point latitude
# lat.rad <- CalcLatitude(calc.locs$lat, distance=calc.locs$distance, bearing=calc.locs$rad.bear) ## result is in radians
# calc.locs$ref.lat <- (lat.rad*360)/(2*pi) ## convert to degrees
# ## calculate reference point longitude
# long.rad <- CalcLongitude(calc.locs$lat, calc.locs$ref.lat, calc.locs$long, distance=calc.locs$distance, bearing=calc.locs$rad.bear) ## result is in radians
# calc.locs$ref.long <- (long.rad*360)/(2*pi) ## convert to degrees
# ref.lat <- mean(calc.locs$ref.lat)
# ref.long <- mean(calc.locs$ref.long)
# 
# ##### 3C. Calculate GPS coords for database mounds without marked waypoints ##
# mnd.locs <- read.csv('/Users/Avril/Documents/krats/krat_data/KRATP.csv')
# mnd.locs <- mnd.locs[,c('lat','long','terr')] ## only keep mound location information
# colnames(mnd.locs) <- c('long','lat','terr')
# mnd.locs <- mnd.locs[!duplicated(mnd.locs),]
# mnd.locs$distance <- sqrt(mnd.locs$long^2 + mnd.locs$lat^2)
# ## calculate bearing from reference point to each mound in degrees (SOH, sin = opposite / hypotenuse)
# mnd.locs$rad.bear <- asin(abs(mnd.locs$long / mnd.locs$distance))
# mnd.locs$deg.bear <- mnd.locs$rad.bear * 180 / pi
# mnd.locs$deg.bear.rotate <- mnd.locs$deg.bear + 180
# mnd.locs$rad.bear.rotate <- mnd.locs$deg.bear.rotate * pi / 180
# 
# ## calculate mound latitude values using reference latitude
# lat.rad <- CalcLatitude(ref.lat, distance=mnd.locs$distance, bearing=mnd.locs$rad.bear.rotate)
# mnd.locs$lat.deg <- (lat.rad*360)/(2*pi)
# ## calculate mound longitude values using reference longitude
# long.rad <- CalcLongitude(ref.lat, mnd.locs$lat.deg, ref.long, distance=mnd.locs$distance, bearing=mnd.locs$rad.bear.rotate)
# mnd.locs$long.deg <- (long.rad*360)/(2*pi)
# ## get final list of mound IDs and long/lat in degrees
# mnd.locs <- mnd.locs[,c('long.deg','lat.deg','terr')]
# colnames(mnd.locs)[1:2] <- c('long','lat')
# 
# ## create mappable coordinates for reference point
# ref <- as.data.frame(cbind(ref.long, ref.lat))
# colnames(ref) <- c('long','lat')
# coordinates(ref) <- c('long','lat')
# proj4string(ref) <- CRS("+proj=longlat +datum=WGS84") 
# ref.plot <- spTransform(ref, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# 
# ### plot map of all mound locations with new calculated GPS coordinates
# coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object
# proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84") 
# mnd.locs.plot <- spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# ## plot
# # pdf('/Users/Avril/Desktop/Rucker_all_calc_points.pdf', width=8, height=8)
# par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,2,1))
# raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
#   points(mnd.locs.plot, pch=19, cex=0.1, col='yellow')
#   points(ref.plot, pch=19, cex=1, col='lightgreen')
#   # text(gps.pts.plot, labels=gps.pts.plot$terr, adj=c(0,1), col='white')
# dev.off() 
#
# ##### 3D. Plot calculated locations on high-res image background #####
# ## try importing high-res orthoimagery files
# setwd('/Users/Avril/Documents/krat_remote_sensing/site_hi_res_orthoimagery/')
# all_oi_files <- list.files(pattern = glob2rx("*tif$"),
#                                 full.names = TRUE)
# 
# block.5 <- raster(all_oi_files[5])
# block.4 <- raster(all_oi_files[4])
# right <- raster::merge(block.4, block.5)
# plot(right, col=gray(0:100 / 100))
# 
# block.1 <- raster(all_oi_files[1])
# block.2 <- raster(all_oi_files[2])
# left <- raster::merge(block.1, block.2)
# plot(left, col=gray(0:100 / 100))
# 
# ## merge left and right sides of image to plot extent of interest
# full <- raster::merge(left, right)
# 
# # pdf('/Users/Avril/Desktop/hires_calc_mound_locations.pdf', width=10, height=10)
# plot(full, col=gray(0:100 / 100), ext=ext)
#   # add calculated GPS locations for all mounds (yellow) and reference point (green)
#   points(mnd.locs.plot, pch=19, cex=0.5, col='yellow')
#   points(ref.plot, pch=13, cex=3, col='lightgreen')
# dev.off()

# ##### 3E. Compare locations of mounds plotting using GPS coordinates for mounds to locations backcalculated using distance to reference point in database #####
# head(gps.locs) ## GPS-marked waypoint coords for mound locations
# head(mnd.locs) ## back-calculated coordinates for mound locations based on reference distances in database
# temp.locs <- mnd.locs.plot[which(mnd.locs.plot$terr %in% gps.locs$terr),] ## narrow down to list of mounds that have both GPS-marked coords and back-calculated coords
# proj4string(temp.locs) <- CRS("+proj=longlat +datum=WGS84") 
# temp.locs.plot <- spTransform(temp.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# 
# par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,2,1))
# raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
#   points(ref.plot, pch=13, cex=3, col='lightgreen') ## plot reference point
#   points(gps.locs, col='yellow', pch=19, cex=0.5) ## GPS-marked points
#   points(temp.locs, col='blue', pch=19, cex=0.5) ## db-calculated points
# 
# ## convert m coords to decimal degrees and calculate distance discrepancy for each mound
# ## read in GPS waypoints
# setwd('/Users/Avril/Desktop/remote_sensing_data/')
# l.pts <- read.csv('Rucker.csv', header=FALSE)
# colnames(l.pts) <- c('lat','long','terr')
# l.pts <- l.pts[,c(2,1,3)]
# ## re-obtain GPS coords for mound db backcalculations
# mnd.locs <- read.csv('/Users/Avril/Documents/krats/krat_data/KRATP.csv')
# mnd.locs <- mnd.locs[,c('lat','long','terr')] ## only keep mound location information
# colnames(mnd.locs) <- c('long','lat','terr')
# mnd.locs <- mnd.locs[!duplicated(mnd.locs),]
# mnd.locs$distance <- sqrt(mnd.locs$long^2 + mnd.locs$lat^2)
# ## calculate bearing from reference point to each mound in degrees (SOH, sin = opposite / hypotenuse)
# mnd.locs$rad.bear <- asin(abs(mnd.locs$long / mnd.locs$distance))
# mnd.locs$deg.bear <- mnd.locs$rad.bear * 180 / pi
# mnd.locs$deg.bear.rotate <- mnd.locs$deg.bear + 180
# mnd.locs$rad.bear.rotate <- mnd.locs$deg.bear.rotate * pi / 180
# ## calculate mound latitude values using reference latitude
# lat.rad <- CalcLatitude(ref.lat, distance=mnd.locs$distance, bearing=mnd.locs$rad.bear.rotate)
# mnd.locs$lat.deg <- (lat.rad*360)/(2*pi)
# ## calculate mound longitude values using reference longitude
# long.rad <- CalcLongitude(ref.lat, mnd.locs$lat.deg, ref.long, distance=mnd.locs$distance, bearing=mnd.locs$rad.bear.rotate)
# mnd.locs$long.deg <- (long.rad*360)/(2*pi)
# ## get final list of mound IDs and long/lat in degrees
# mnd.locs <- mnd.locs[,c('long.deg','lat.deg','terr')]
# colnames(mnd.locs)[1:2] <- c('long','lat')
# 
# ## limit down to same list of mounds
# l.pts <- l.pts[which(l.pts$terr %in% mnd.locs$terr),]
# mnd.locs <- mnd.locs[which(mnd.locs$terr %in% l.pts$terr),]
# 
# ## for each mound, calculate the distance between location estimates
# OUT <- NULL
# for(i in unique(l.pts$terr)){
#   sub.gps <- l.pts[which(l.pts$terr == as.character(i)),]
#   sub.db <- mnd.locs[which(mnd.locs$terr == as.character(i)),]
#   dist <- CalcDistance(sub.gps$lat, sub.gps$long, sub.db$lat, sub.db$long) * 1000 ## in kilometers (* by 1000 for m)
#   save <- c(as.character(i), dist)
#   OUT <- rbind(OUT, save)
# }
# 
# loc.diffs <- as.data.frame(OUT, row.names=FALSE)
# loc.diffs$V2 <- as.numeric(paste(loc.diffs$V2))
# # pdf('/Users/Avril/Desktop/distance_discrepancies.pdf', width=6, height=6)
# par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,2,1))
# hist(loc.diffs$V2, col='grey', breaks=20, main='Discrepancy between GPS-marked and\ndatabase-calculated mound locations',
#      xlab='Distance (m)')
# # dev.off()

##### 4. Re-examine mound names in waypoint file and database to ID naming discrepancies and match up mounds with "missing" GPS data #####
head(gps.pts) ## known GPS coords for named mounds
head(unmarked) ## database mound names that don't EXACTLY match any mound names in waypoint files

OUT <- NULL
UNMARKED <- NULL
for(i in unmarked){ ## for each mound without a GPS waypoint,
  save <- gps.pts[grep(i, gps.pts$terr),] ## search for name matches in the waypoint file,
  if(nrow(save) == 0){
    UNMARKED <- c(UNMARKED, i)
  }
  if(nrow(save) > 0){
    save <- cbind(save, i) ## add the database mound name,
    OUT <- rbind(OUT, save) ## and save the match(es)
  }
} 
nrow(OUT)
length(unique(OUT$i)) ### 111 mounds of interest with 206 name matches
length(UNMARKED) ## 21 mounds of interest with no grep matches in the waypoint file -- examine manually
rm(unmarked) ## get rid of original list of 'unmarked' mounds - now separated into OUT and UNMARKED
# write.csv(OUT, '/Users/Avril/Desktop/list_of_all_grep_matches.csv', row.names=FALSE)

##### 4A. Make some plots for each unmarked mound to line it up with its potential GPS matches #####
## set coordinates
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750
## make extent object
ext <- extent(lo.x, hi.x, lo.y, hi.y)
# pdf('/Users/Avril/Desktop/comparisons.pdf', width=12, height=6)
# par(mfrow=c(1,2))
# for(i in 1:nrow(OUT)){
#   gps.name <- OUT$terr[i]
#   db.name <- OUT$i[i]
#   ## plot all GPS waypoints, highlighting point of interest
#   raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
#     points(gps.pts.plot, pch=19, cex=0.25, col='lavender')
#     points(gps.pts.plot[which(gps.pts.plot$terr == gps.name),], col='yellow', pch=19, cex=1)
#     text(lo.x, hi.y, labels=gps.name, adj=c(0,1), col='yellow', cex=1.5)
#   ## plot all database coords (in meters), highlighting point of interest for comparison with GPS patterns
#   par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))
#   plot(c(0, mnd.locs$long), c(0, mnd.locs$lat), pch=19, cex=0.5, col='transparent', main=db.name,
#          xlab='Database long position (m)', ylab='Database lat position (m)')
#     polygon(x=c(-1500,300,300,-1500), y=c(-1500,-1500,200,200), col='#424148')
#     points(0, 0, pch=13, cex=1, col='springgreen')
#     points(int.locs, pch=19, col='lavender', cex=0.25) ## all points
#     points(int.locs[which(int.locs$terr == as.character(db.name)),],
#            pch=19, col='yellow', cex=1) ## points with GPS information
# }
# dev.off()
### >> visually inspected plots, determining whether name matches corresponded to actual mound ID matches >>>
#### recorded in list_of_confirmed_grep_matches.csv

## read in list of manually verified mound locations and matches
chkd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/archive/sorting_out_mound_names/list_of_confirmed_grep_matches.csv') ## 109 mounds successfully matched after visual inspection
## combine with previously linked waypoint and database points
chkd.pts <- gps.pts[gps.pts$terr %in% int.locs$terr,] ## narrow down to mounds of interest
colnames(chkd.pts)[3] <- 'waypoint.name'
chkd.pts$database.name <- chkd.pts$waypoint.name ## for the mounds in gps.pts, waypoint and database names already matched exactly
chkd.pts <- rbind(chkd.pts, chkd.locs)
rm(chkd.locs)

##### Summarizing progress to here: #####
## There are 187 unique mounds with successfully matched GPS coordinates, based on (1) exact name matches between waypoint files and 
## mound database names and (2) visually inspected grep matches (stored in gps.pts). 25 database mounds remain to be connected to 
## GPS coords, if possible. Of these 25, 4 mounds returned grep matches, but did not spatially correspond to any of the returned
## matches:
OUT <- OUT[OUT$i %notin% chkd.pts$database.name,]
unique(OUT$i)
## The "R2" mound name shows up here quite a bit and is problematic because it's used to refer to 6 distinct locations in the mound
## database:
int.locs[int.locs$terr=='R2',]
## The remaining 21 un-mapped mound names are in 'unmarked' and require further examination
unmarked <- UNMARKED
rm(UNMARKED)
## Combining these 4 and 21 mounds --> list of 25 mound database names without successful GPS waypoint matches
unmapped <- c(unmarked, as.character(unique(OUT$i)))

##### 4B. Deal with unmatched/unmarked database points #####
## plot their locations first - compare this large plot to the GPS-based one produced at line 101
pdf('/Users/Avril/Desktop/database_locations_LARGE.pdf', width=200, height=200)
par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))
plot(c(0, mnd.locs$long), c(0, mnd.locs$lat), pch=19, cex=0.5, col='blue', main='Mounds of interest',
     xlab='Database long position (m)', ylab='Database lat position (m)')
  polygon(x=c(-1500,300,300,-1500), y=c(-1500,-1500,200,200), col='#424148')
  points(0, 0, pch=13, cex=1, col='springgreen')
  points(int.locs, pch=19, col='yellow', cex=5) ## all points
  points(int.locs[int.locs$terr %in% unmarked, 'long'], int.locs[int.locs$terr %in% unmarked, 'lat'],
         pch=19, col='lavender', cex=5) ## points with GPS information
  text(int.locs[int.locs$terr %in% unmarked, 'long'], int.locs[int.locs$terr %in% unmarked, 'lat'],
       labels=int.locs[int.locs$terr %in% unmarked, 'terr'], col='white', adj=c(0,3))
  text(int.locs[int.locs$terr %notin% unmarked, 'long'], int.locs[int.locs$terr %notin% unmarked, 'lat'],
       labels=int.locs[int.locs$terr %notin% unmarked, 'terr'], col='yellow', adj=c(0,3))
  ## add all R2 points and labels
  points(int.locs[int.locs$terr=='R2', 'long'], int.locs[int.locs$terr=='R2', 'lat'],
         pch=19, col='lavender', cex=5) 
  text(int.locs[int.locs$terr=='R2', 'long'], int.locs[int.locs$terr=='R2', 'lat'],
         labels=int.locs[int.locs$terr=='R2', 'terr'], col='white', adj=c(0,3)) 
  legend('bottomright', legend=c('reference point','with GPS info','without GPS info'), box.col='white',
         col=c('springgreen','blue','lavender'), pch=c(13,19,19), bg='#424148', text.col='white')
dev.off()
## something weird going on with mounds just labeled "R2" in database -- mark those points
par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))
plot(c(0, int.locs$long), c(0, int.locs$lat), pch=19, cex=0.5, col='blue', main='Mounds of interest',
       xlab='Database long position (m)', ylab='Database lat position (m)')
  polygon(x=c(-1500,300,300,-1500), y=c(-1500,-1500,200,200), col='#424148')
  points(0, 0, pch=13, cex=1, col='springgreen')
  points(int.locs, pch=19, col='blue', cex=0.5) ## all points
  points(int.locs[int.locs$terr=='R2', 'long'], int.locs[int.locs$terr=='R2', 'lat'],
         pch=19, col='lavender', cex=0.5) ## points with GPS information
  legend('bottomright', legend=c('reference point','all mounds not called R2','called R2 in database'), box.col='white',
         col=c('springgreen','blue','lavender'), pch=c(13,19,19), bg='#424148', text.col='white')
##### 4C. Determining GPS coordinates for 20 mounds without them ####
### >>> from examining the large plots of GPS and database points, the only unmarked/R2 mound with a corresponding
## GPS point is 'SHANGRLA'; for the rest, I will use the nearest 5 GPS points to calculate a new set of GPS coords
## for each unmarked mound. I'll plot the resulting points and compare visually to the database-based map.
## first, add 'SHANGRLA' to list of known coordinates, and remove it from 'unmarked'
save <- as.data.frame(c(gps.pts[gps.pts$terr=='Shangrila', c(1:2)], 'Shangrila', 'SHANGRLA'))
colnames(save) <- colnames(chkd.pts)
chkd.pts <- rbind(chkd.pts, save)
unmarked <- unmarked[-which(unmarked=='SHANGRLA')]

## write file of matched GPS points
# write.csv(chkd.pts, 'mound_GPS_coords_n188.csv', row.names=FALSE)

# ## next, calculate GPS coordinates for remaining 20 unmarked mounds using 5 nearest GPS coords in chkd.pts as references
# mkd <- mnd.locs[which(mnd.locs$terr %in% chkd.pts$database.name | mnd.locs$terr %in% unmarked),] ## get database meter coords for all mounds of interest with verified GPS coords and unmarked points of interest
# rownames(mkd) <- mkd$terr
# mkd <- mkd[,-3]
# mkd.dist <- as.matrix(dist(mkd, method='euclidean'))
# mkd.dist <- mkd.dist[,which(colnames(mkd.dist) %in% unmarked)]
# mkd.dist <- mkd.dist[which(rownames(mkd.dist) %notin% unmarked),] ## don't care about distance to other unmarked points
# ## identify 5 points nearest unmarked mounds
# OUT <- NULL
# for(i in 1:ncol(mkd.dist)){
#   ## ID and store 5 nearest points
#   save <- sort(mkd.dist[,i])[2:6]
#   save1 <- names(save)
#   save1 <- c(colnames(mkd.dist)[i], save1)
#   
#   # ## plot these points
#   # plot(mnd.locs[which(mnd.locs$terr %in% save1), 'long'], mnd.locs[which(mnd.locs$terr %in% save1), 'lat'], pch=19, main=colnames(mkd.dist)[i])
#   #   points(mnd.locs[which(mnd.locs$terr == colnames(mkd.dist)[i]), 'long'],
#   #          mnd.locs[which(mnd.locs$terr == colnames(mkd.dist)[i]), 'lat'], pch=19, col='blue')
#   
#   ## calculate bearing between reference and focal points
#   for(j in 2:6){
#     focal <- save1[1]
#     ref <- save1[j]
#     dist <- mkd.dist[which(rownames(mkd.dist) == ref), which(colnames(mkd.dist) == focal)] ## get distance in meters
#     focal.m <- mkd[which(rownames(mkd) == focal),]
#     ref.m <- mkd[which(rownames(mkd) == ref),]
#     ref.deg.lat <- chkd.pts[which(chkd.pts$database.name == ref), 'lat']
#     ref.deg.long <- chkd.pts[which(chkd.pts$database.name == ref), 'long']
#     #### bearing calculation will depend on which quadrant the focal is located in, relative to the reference;
#     #### calculate bearing from perspective of reference points; (SOH, sin = opposite / hypotenuse)
#     ## q1: ref.long < focal.long & ref.lat < focal.lat
#     if(ref.m$long < focal.m$long & ref.m$lat < focal.m$lat){
#       rad.bear <- asin(abs((focal.m$long - ref.m$long) / dist))
#       deg.bear <- rad.bear * 180 / pi
#       rad.bear.rotate <- deg.bear * pi / 180
#     }
#     ## q2: ref.long < focal.long & ref.lat > focal.lat
#     if(ref.m$long < focal.m$long & ref.m$lat > focal.m$lat){
#       rad.bear <- asin(abs((focal.m$lat - ref.m$lat) / dist))
#       deg.bear <- rad.bear * 180 / pi
#       deg.bear <- deg.bear + 90
#       rad.bear.rotate <- deg.bear * pi / 180
#     }
#     ## q3: ref.long < focal.long & ref.lat < focal.lat
#     if(ref.m$long < focal.m$long & ref.m$lat < focal.m$lat){
#       rad.bear <- asin(abs((focal.m$long - ref.m$long) / dist))
#       deg.bear <- rad.bear * 180 / pi
#       deg.bear <- deg.bear + 180
#       rad.bear.rotate <- deg.bear * pi / 180
#     }
#     ## q4: ref.long < focal.long & ref.lat < focal.lat
#     if(ref.m$long < focal.m$long & ref.m$lat < focal.m$lat){
#       rad.bear <- asin(abs((focal.m$lat - ref.m$lat) / dist))
#       deg.bear <- rad.bear * 180 / pi
#       deg.bear <- deg.bear + 270
#       rad.bear.rotate <- deg.bear * pi / 180
#     }
# 
#     ## calculate new coordinates for focal point
#     lat.rad <- CalcLatitude(ref.deg.lat, distance=dist, bearing=rad.bear.rotate)
#     lat.deg <- (lat.rad*360)/(2*pi)
#     long.rad <- CalcLongitude(ref.deg.lat, lat.deg, ref.deg.long, dist, rad.bear.rotate)
#     long.deg <- (long.rad*360)/(2*pi)
#     
#     ## save coord estimates
#     tosave1 <- c(focal, ref, 'focal', long.deg, lat.deg)
#     tosave2 <- c(focal, ref, 'ref', ref.deg.long, ref.deg.lat)
#     OUT <- rbind(OUT, tosave1, tosave2)
#   }
# }
# est.focal.coords <- as.data.frame(OUT)
# colnames(est.focal.coords) <- c('focal.db.name','ref.db.name','pt.type','long.deg','lat.deg')
# est.focal.coords$long.deg <- as.numeric(paste(est.focal.coords$long.deg))
# est.focal.coords$lat.deg <- as.numeric(paste(est.focal.coords$lat.deg))
# rownames(est.focal.coords) <- NULL
# focals.only <- est.focal.coords[est.focal.coords$pt.type=='focal',]
# ## convert to plottable coordinates and visually check variation in calculated coords for each mound
# focals.only.plot <- focals.only ## save as new object prior to format conversions
# coordinates(focals.only.plot) <- c('long.deg','lat.deg') ## converts to SpatialPointsDataFrame object
# proj4string(focals.only.plot) <- CRS("+proj=longlat +datum=WGS84") 
# focals.only.plot <- spTransform(focals.only.plot, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
# raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
#   points(focals.only.plot, pch=19, cex=0.5, col='yellow')
# # text(focals.only.plot, labels=focals.only.plot$focal.db.name, col='white', adj=c(0,1.8)) ## useful when plotting large file (200 x 200)
# # text(lo.x, hi.y, labels='All mounds with GPS locations', adj=c(0,1), col='yellow', cex=1.5)
# # dev.off()
# ### Okay, this approach won't work. Not enough agreement between coordinates calculated for unmarked mounds ###
  
##### 4D. Plot unmarked mounds still in need of GPS points #####
pdf('/Users/Avril/Desktop/mound_GPS_avail.pdf', width=7, height=7)
par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))
plot(c(0, int.locs$long), c(0, int.locs$lat), pch=19, cex=0.5, col='transparent',
       xlab='Database long position (m)', ylab='Database lat position (m)')
  points(0, 0, pch=13, cex=1, col='springgreen4')
  points(int.locs, pch=19, col='blue4', cex=0.5) ## all points
  points(int.locs[int.locs$terr %in% unmarked, 'long'], int.locs[int.locs$terr %in% unmarked, 'lat'],
         pch=21, col='black', bg='yellow', cex=1) ## points without GPS information
  points(int.locs[int.locs$terr=='R2', 'long'], int.locs[int.locs$terr=='R2', 'lat'],
         pch=21, col='black', bg='green', cex=1) ## all points labeled 'R2' in database 
  legend('bottomright', legend=c('reference point','GPS marked (n=188)','unmarked (n=20)','R2 in database (n=6)'), pt.cex=c(1,0.5,1,1),
         col=c('springgreen4','blue4','black','black'), pt.bg=c(NA,NA,'yellow','green'), pch=c(13,19,21,21), inset=c(0.025,0.025))
dev.off()

pdf('/Users/Avril/Desktop/mound_GPS_avail_info_LARGE.pdf', width=200, height=200)
par(mar=c(5.1,4.1,4.1,2.1), mgp=c(3,1,0))
plot(c(0, int.locs$long), c(0, int.locs$lat), pch=19, cex=0.5, col='transparent',
     xlab='Database long position (m)', ylab='Database lat position (m)')
  points(0, 0, pch=13, cex=5, col='springgreen4')
  points(int.locs, pch=19, col='blue4', cex=5) ## all points
  
  points(int.locs[int.locs$terr %in% unmarked, 'long'], int.locs[int.locs$terr %in% unmarked, 'lat'],
         pch=21, col='black', bg='yellow', cex=5) ## points without GPS information
  points(int.locs[int.locs$terr=='R2', 'long'], int.locs[int.locs$terr=='R2', 'lat'],
         pch=21, col='black', bg='green', cex=5) ## all points labeled 'R2' in database 
  ## label non-R2 mounds
  text(int.locs[int.locs$terr!='R2', 'long'], int.locs[int.locs$terr!='R2', 'lat'], labels=int.locs[int.locs$terr!='R2', 'terr'], adj=c(0,3))
  ## label R2 mounds with their database coordinates so they can be renamed and geo-matched
  text(int.locs[int.locs$terr=='R2', 'long'], int.locs[int.locs$terr=='R2', 'lat'], 
       labels=paste0(int.locs[int.locs$terr=='R2', 'terr'],'\n(',int.locs[int.locs$terr=='R2', 'long'],', ',int.locs[int.locs$terr=='R2', 'lat'],')'), adj=c(0,3))
  legend('bottomright', legend=c('reference point','GPS marked (n=188)','unmarked (n=20)','R2 in database (n=6)'), pt.cex=c(1,0.5,1,1),
         col=c('springgreen4','blue4','black','black'), pt.bg=c(NA,NA,'yellow','green'), pch=c(13,19,21,21), inset=c(0.025,0.025))
dev.off()

## write table of un-GPS-marked mounds + R2 mounds for manual assignment to cell #s
temp <- rbind(int.locs[int.locs$terr %in% unmarked,], int.locs[int.locs$terr=='R2',])
# write.csv(temp, '/Users/Avril/Desktop/mounds_to_be_assigned.csv', row.names=FALSE)

# ##### 4E. Read in results of manual mound:cell assignment process to get coordinates for centers of those cells #####
man.ass <- read.csv('/Users/Avril/Documents/krat_remote_sensing/archive/sorting_out_mound_names/manual_mound_cell_assignments.csv')
temp.coords <- as.data.frame(xyFromCell(ls5_stack, man.ass$cell))
temp.coords$terr <- man.ass$terr
coordinates(temp.coords) <- c('x','y')

raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
points(temp.coords, pch=19, col='green')

pdf('/Users/Avril/Desktop/gps_locs_LARGE_plus_assignments.pdf', width=200, height=200)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
  points(gps.pts.plot, pch=19, cex=2, col='yellow')
  text(gps.pts.plot, labels=gps.pts.plot$terr, col='white', adj=c(0,1.8)) ## useful when plotting large file (200 x 200)
  points(temp.coords, pch=19, col='green', cex=2)
  text(temp.coords, labels=temp.coords$terr, col='white', adj=c(0,1.8)) ## useful when plotting large file (200 x 200)
  text(temp.layer) ## add cell numbers to double-check assignments plot correctly
dev.off()



gps.pts.plot <- gps.pts ## save as new object prior to format conversions
coordinates(gps.pts.plot) <- c('long','lat') ## converts to SpatialPointsDataFrame object
proj4string(gps.pts.plot) <- CRS("+proj=longlat +datum=WGS84")
gps.pts.plot <- spTransform(gps.pts.plot, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

##### 5. Plot mounds with known GPS coords over high-res imagery #####
## read in mound waypoints
 
  