library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
library(rgdal)
`%notin%` <- Negate(`%in%`)
setwd('/Users/Avril/Documents/krat_remote_sensing/tc_output_tables/')

tc.fn <- 'tc_cloud_free' ## for TC data
## options: tc_initial_test   tc_cloud_free

mc.fn <- 'mnd_cloud_free' ## for mound/cell ID key
## options: mnd_key_initial_test   mnd_cloud_free

tc.dat <- read.csv(paste0(tc.fn,'.csv'))
mc.key <- read.csv(paste0(mc.fn,'.csv'))

# ## limit tc.dat/mc.key by scene cloud %
# low.cloud <- read.table('/Users/Avril/Documents/krat_remote_sensing/landsat_5_data_overviews/low_cloud_scenes.txt', sep='\t')
# cloud.free <- read.table('/Users/Avril/Documents/krat_remote_sensing/landsat_5_data_overviews/cloud_free_scenes.txt', sep='\t')
# 
# tc.dat <- tc.dat[which(tc.dat$scene.id %in% low.cloud$V1),]
# mc.key <- mc.key[which(mc.key$scene.id %in% low.cloud$V1),]

## plot mean values across extent for each scene/timepoint
mean.dat <- tc.dat[,-1] ## get rid of ID, because there are repeat cell values across mound IDs
mean.dat <- mean.dat[!duplicated(mean.dat),] ## get rid of duplicated rows (2,600,424 rows --> 383,616 rows)
mean.dat$year <- as.numeric(do.call(rbind, strsplit(as.character(mean.dat$acq.date), split='-', fixed=TRUE))[,1])
## before taking means, plot distributions of each day within each year
for(i in unique(mean.dat$year)){
  sub <- mean.dat[which(mean.dat$year == i),]
  sub <- sub[order(sub$doy),]
  sub$doy <- as.factor(sub$doy)
  print(ggplot(sub, aes(x=doy, y=greenness)) + 
    ggtitle(paste0(i, ' - greenness')) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_boxplot(colour='black', fill='grey'))
}

## loop over observations and take mean of all cell TC values for each timepoint
OUT <- NULL
for(i in unique(mean.dat$scene.id)){
  sub <- mean.dat[which(mean.dat$scene.id == i),]
  save <- c(mean(sub$greenness), mean(sub$wetness), mean(sub$brightness), sub$doy[1], sub$year[1], sub$path[1])
  OUT <- rbind(OUT, save)
}
out <- as.data.frame(OUT)
colnames(out) <- c('greenness','wetness','brightness','doy','year','path')
plot(out$doy, out$brightness, col=out$year, pch=19, cex=0.8)
plot(out$doy, out$greenness, col=out$year, pch=19, cex=0.8)
plot(out$doy, out$wetness, col=out$year, pch=19, cex=0.8)
## plot years separately
# pdf('/Users/Avril/Desktop/test.pdf', height=18, width=9)
# par(mfrow=c(9,2))
for(i in unique(out$year)){
  sub <- out[which(out$year == i),]
  plot(sub$doy, sub$wetness, col=sub$path, pch=19, cex=1, main=i)
}
# dev.off()

##### GIF attempt #####
## read in TC data
plot.dat <- tc.dat[,-1] ## get rid of ID, because there are repeat cell values across mound IDs
plot.dat <- plot.dat[!duplicated(plot.dat),] ## get rid of duplicated rows (2,600,424 rows --> 383,616 rows)
plot.dat$year <- as.numeric(do.call(rbind, strsplit(as.character(plot.dat$acq.date), split='-', fixed=TRUE))[,1])
plot.dat$dec <- plot.dat$year+(plot.dat$doy/365)

## set up year data and time portion of plot
days <- plot.dat[,c('year','doy','dec','scene.id')]
days <- days[!duplicated(days),]
days$yval <- 1
days <- days[order(days$dec),]
# pdf('/Users/Avril/Desktop/date_test.pdf', width=5, height=1)
# par(mar=c(2.1,0,0,0))
# for(i in 1:nrow(days)){
# plot(days$dec[i], days$yval[i], ylim=c(0,1.25), ylab='', xlab='', bty='n', yaxt='n', pch=19, cex=3,
#      xlim=c(1989,2005), xaxt='n')
#   axis(1, at=c(1989,2005), labels=TRUE)
#   lines(x=c(days$dec[i], days$dec[i]), y=c(-0.1, 1), lwd=6)
# }
# dev.off()

## set up TC portion of plot
## set tighter extent
lo.x <- 663500+350
hi.x <- 665600-400
lo.y <- 3497000+800
hi.y <- 3499750-400
ext <- extent(lo.x, hi.x, lo.y, hi.y)

## read in mound data
mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84") 
mnd.locs <- spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

## read in 1 cropped scene to get background image
ls5.stack <- brick('/Users/Avril/Documents/krat_remote_sensing/cropped_landsat45tm_scenes/LT05_L1TP_035038_20051121_20160911_01_T1_CROPPED.grd')
raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[1:3], margins=FALSE)

## read in high res background image
bg <- raster('/Users/Avril/Documents/krat_remote_sensing/site_hi_res_orthoimagery/cropped_highresortho.grd')
plot(bg, col=gray(0:100 / 100))
## memory issue with projection conversion -- split into 4 smaller images first?
xmin <- extent(bg)[1]
xmax <- extent(bg)[2]
ymin <- extent(bg)[3]
ymax <- extent(bg)[4]
## segment 1
ext.1 <- extent((xmin+xmax)/2, xmax, (ymin+ymax)/2, ymax)
bg.1 <- crop(bg, ext.1)
bg1.reproj <- projectRaster(bg.1, crs=crs(ls5.stack)) ## reproject bg image to match crs of ls5 data
## segment 2
ext.2 <- extent((xmin+xmax)/2, xmax, ymin, (ymin+ymax)/2)
bg.2 <- crop(bg, ext.2)
bg2.reproj <- projectRaster(bg.2, crs=crs(ls5.stack)) ## reproject bg image to match crs of ls5 data
## segment 3
ext.3 <- extent(xmin, (xmin+xmax)/2, ymin, (ymin+ymax)/2)
bg.3 <- crop(bg, ext.3)
bg3.reproj <- projectRaster(bg.3, crs=crs(ls5.stack)) ## reproject bg image to match crs of ls5 data
## segment 4
ext.4 <- extent(xmin, (xmin+xmax)/2, (ymin+ymax)/2, ymax)
bg.4 <- crop(bg, ext.4)
bg4.reproj <- projectRaster(bg.4, crs=crs(ls5.stack)) ## reproject bg image to match crs of ls5 data
bg.reproj <- raster::merge(bg1.reproj, bg2.reproj, bg3.reproj, bg4.reproj)
bg.reproj <- crop(bg.reproj, ext)

## make buffer outline - should work for all 3 metrics
plot(ls5.stack$B1_dn, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n',
     xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='Buffer extracted\n(greenness values)')
  points(mnd.locs, pch=19, cex=0.2)
  g.ness <- extract(ls5.stack$B1_dn, mnd.locs, method='simple', df=TRUE, cellnumbers=TRUE, buffer=78)
  r2 <- ls5.stack$B1_dn
  r2[setdiff(seq_len(ncell(r2)), unique(g.ness[,2]))] <- NA
  r2[!is.na(r2)] <- 1
  plot(rasterToPolygons(r2, dissolve=TRUE), add=TRUE, border='black', lwd=2)

## final test plot
sub <- plot.dat[which(plot.dat$scene.id==unique(plot.dat$scene.id)[1]),]
pic <- unique(sub$cells) ## get list of cell nums to visualize
all.cells <- seq(from=1, to=ncell(ls5.stack$B1_dn)) ## get list of all cell nums in image
no.pic <- all.cells[all.cells %notin% pic] ## get list of cell IDs to not visualize

## set color values
pal <- plasma ## other options: viridis  magma   plasma  inferno  cividis   mako    turbo   rocket
w.cuts <- seq(from=min(plot.dat$wetness, na.rm=TRUE), to=max(plot.dat$wetness, na.rm=TRUE), length.out=100)

## set all cells that will never have values to NA
temp <- ls5.stack$B1_dn
for(j in no.pic){
  temp[j] <- NA
}

##### Plotting greenness map + time plot #####
boxplot(plot.dat$greenness)
## get rid of extreme greenness value
ex <- plot.dat[which(plot.dat$greenness == max(plot.dat$greenness, na.rm=TRUE)), 'scene.id']
plot.g <- plot.dat[which(plot.dat$scene.id != ex),]
g.cuts <- seq(from=min(plot.g$greenness, na.rm=TRUE), to=max(plot.g$greenness, na.rm=TRUE), length.out=100)
plot.g <- plot.g[order(plot.g$dec),]
# ## identify a year that looks pretty good across months
# plot.g <- plot.g[order(plot.g$year),]
# for(i in unique(plot.g$year)){
#   temp <- plot.g[which(plot.g$year == i),]
#   plot(temp$doy, temp$greenness, pch=19, col='black', main=i)
# }
# ## 1998 looks kinda good
# plot.g <- plot.g[which(plot.g$year == '1998'),]

pdf('/Users/Avril/Desktop/greenness_test.pdf', width=6, height=8)
for(j in 1:length(unique(plot.g$scene.id))){
  sub <- plot.g[which(plot.g$scene.id==unique(plot.g$scene.id)[j]),]
  vals <- plot.g[which(plot.g$year == sub$year[1]), 'greenness']
  ## set color breaks for year-specific values
  g.cuts <- seq(from=min(vals, na.rm=TRUE), to=max(vals, na.rm=TRUE), length.out=100)
  for(i in 1:nrow(sub)){
    temp[sub$cells[i]] <- sub$greenness[i] 
  }
  raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[1:3], margins=FALSE, ext=ext)
    raster::plot(bg.reproj$layer, col=gray(0:100 / 100), yaxt='n', xaxt='n', legend=FALSE, ext=ext, add=TRUE)
    plot(temp, add=TRUE, legend=FALSE, col='transparent', breaks=g.cuts)
    plot(temp, add=TRUE, legend=FALSE, col=pal(length(g.cuts)), breaks=g.cuts, ext=ext)
    plot(rasterToPolygons(r2, dissolve=TRUE), add=TRUE, border='white', lwd=0.25) ## buffer outline
    points(mnd.locs, pch=19, cex=0.4, col='white')
  print(j/length(unique(plot.dat$scene.id)))
}
dev.off()

##### Plotting brightness map + time plot #####
boxplot(plot.dat$brightness)
## get rid of bightness values > 0.5
ex <- plot.dat[which(plot.dat$brightness > 0.5), 'scene.id']
plot.b <- plot.dat[which(plot.dat$scene.id %notin% ex),]
b.cuts <- seq(from=min(plot.dat$brightness, na.rm=TRUE), to=max(plot.dat$brightness, na.rm=TRUE), length.out=100)

pdf('/Users/Avril/Desktop/brightness_test.pdf', width=6, height=8)
for(j in 1:length(unique(plot.b$scene.id))){
  sub <- plot.b[which(plot.b$scene.id==unique(plot.b$scene.id)[j]),]
  vals <- plot.b[which(plot.b$year == sub$year[1]), 'brightness']
  ## set color breaks for year-specific values
  g.cuts <- seq(from=min(vals, na.rm=TRUE), to=max(vals, na.rm=TRUE), length.out=100)
  for(i in 1:nrow(sub)){
    temp[sub$cells[i]] <- sub$brightness[i] 
  }
  raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[1:3], col='transparent', margins=FALSE, ext=ext)
    raster::plot(bg.reproj$layer, col=gray(0:100 / 100), yaxt='n', xaxt='n', legend=FALSE, ext=ext, add=TRUE)
    plot(temp, add=TRUE, legend=FALSE, col='transparent', breaks=b.cuts)
    plot(temp, add=TRUE, legend=FALSE, col=pal(length(b.cuts)), breaks=b.cuts, ext=ext)
    plot(rasterToPolygons(r2, dissolve=TRUE), add=TRUE, border='white', lwd=0.25) ## buffer outline
    points(mnd.locs, pch=19, cex=0.4, col='white')
  print(j/length(unique(plot.b$scene.id)))
}
dev.off()
# ## set up year data and time plot to go with brightness
# days <- plot.dat[,c('year','doy','dec','scene.id')]
# days <- days[!duplicated(days),]
# days$yval <- 1
# days <- days[order(days$dec),]
# days <- days[which(days$scene.id %notin% ex),]
# pdf('/Users/Avril/Desktop/brightness_date.pdf', width=5, height=1)
# par(mar=c(2.1,0,0,0))
# for(i in 1:nrow(days)){
# plot(days$dec[i], days$yval[i], ylim=c(0,1.25), ylab='', xlab='', bty='n', yaxt='n', pch=19, cex=3,
#      xlim=c(1989,2005), xaxt='n')
#   axis(1, at=c(1989,2005), labels=TRUE)
#   lines(x=c(days$dec[i], days$dec[i]), y=c(-0.1, 1), lwd=6)
# }
# dev.off()

# ##### set up image formatting and put 2 plots together - format squashes TC data layer #####
# ## set up year data 
# days <- plot.dat[,c('year','doy','dec','scene.id')]
# days <- days[!duplicated(days),]
# days$yval <- 1
# days <- days[order(days$dec),]
# ## set up TC data 
# 
# pdf('/Users/Avril/Desktop/comb_test.pdf', width=(10/2), height=(11.5/2))
# # layout.show(nf)
# for(i in 1:nrow(days)){
#   layout(matrix(c(1,2), nrow=2,ncol=1), widths=c(8.5,8.5), heights=c(2.5,8.5), respect=TRUE)
#   par(mar=c(2.1,0,1.1,0))
#   scene <- days$scene.id[i] ## save scene ID
#   sub <- plot.dat[which(plot.dat$scene.id==scene),]
#   if(nrow(sub) == nrow(sub[complete.cases(sub),])){
#     # print('TRUE')
#     ## plot date info
#     plot(days$dec[i], days$yval[i], ylim=c(0,1.25), ylab='', xlab='', bty='n', yaxt='n', pch=19, cex=3,
#          xlim=c(1989,2005), xaxt='n')
#     axis(1, at=c(1989,2005), labels=TRUE)
#     lines(x=c(days$dec[i], days$dec[i]), y=c(-0.1, 1), lwd=6)
#     
#     ## plot TC info
#     vals <- plot.dat[which(plot.dat$year == sub$year[1]), 'greenness']
#     ## set color breaks for year-specific values
#     g.cuts <- seq(from=min(vals, na.rm=TRUE), to=max(vals, na.rm=TRUE), length.out=100)
#     for(j in 1:nrow(sub)){
#       temp[sub$cells[j]] <- sub$greenness[j] 
#     }
#     raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[1:3], margins=FALSE, ext=ext, alpha=1)
#     raster::plot(bg.reproj$layer, col=gray(0:100 / 100), yaxt='n', xaxt='n', legend=FALSE, ext=ext, add=TRUE)
#       plot(temp, add=TRUE, legend=FALSE, col=pal(length(g.cuts)), breaks=g.cuts, ext=ext)
#   }
#   print(i/nrow(days))
# }
# dev.off()
