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

##### Read in mound waypoints for later extraction #####
mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84") 
mnd.locs <- sp::spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

#
##### Loop over directories of raw data and write cropped raster files #####
# ### Only need to run this loop once ###
# ## loop over downloads in scratch space (n=648) 
# setwd('/Volumes/avril_data/krat_remote_sensing/raw_landsat45tm_scene_downloads/')
# dirs <- list.files()
# 
# ## set extent for all analyses
# lo.x <- 663500
# hi.x <- 665600
# lo.y <- 3497000
# hi.y <- 3499750
# ext <- extent(lo.x, hi.x, lo.y, hi.y)
# 
# for(i in dirs){
#   setwd(paste0('/Volumes/avril_data/krat_remote_sensing/raw_landsat45tm_scene_downloads/',i,'/'))
#   ## read in data
#   all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"), full.names = TRUE)
#   if(length(all_landsat_bands) != 0){ ## if files haven't been renamed yet, rename them
#     ## get new filenames and rename TIF files - filenames must == band names in metadata file or radCor() will fail
#     new.names <- do.call(rbind, strsplit(all_landsat_bands, split='_', fixed=TRUE))[,8]
#     new.names <- paste0(gsub('.TIF','',new.names),'_dn')
#     file.rename(all_landsat_bands, new.names)
#     ls5.stack <- stack(new.names)
#   } else{ ## otherwise, they've already been renamed and can be read in with this loop
#     new.names <- list.files(pattern = glob2rx("*_dn$"), full.names = TRUE)
#     ls5.stack <- stack(new.names)
#   }
#   ## read in metadata
#   md.file <- list.files(pattern=glob2rx("*MTL.txt"), full.names=TRUE)
#   m.data <- readMeta(md.file) ## works for LS5 Collection 1 Level 1 data - errors with Collection 2 Level 1
#   
#   ## crop raster data by extent to reduce processing times and write to file
#   ls5.stack <- crop(ls5.stack, ext)
#   
#   writeRaster(ls5.stack, filename=paste0('/Volumes/avril_data/krat_remote_sensing/cropped_landsat45tm_scenes/',i,'_CROPPED.grd'), progress='text', overwrite=TRUE)
#   ls5.stack <- brick(paste0('/Volumes/avril_data/krat_remote_sensing/cropped_landsat45tm_scenes/',i,'_CROPPED.grd'))
#   
#   print(i)
# }


##### loop over cropped scenes, process, and write TC output #####
### Initial test run of this loop = 5 minutes ###
## set file names for run
tc.fn <- 'tc_initial_test' ## for TC data
mc.fn <- 'mnd_key_initial_test' ## for mound/cell ID key

## set extent variables
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750

# setwd('/Volumes/avril_data/krat_remote_sensing/cropped_landsat45tm_scenes/')
setwd('/Users/Avril/Documents/krat_remote_sensing/cropped_landsat45tm_scenes/')
files <- list.files(pattern="*.grd")

pdf(file='/Users/Avril/Desktop/test.pdf', width=8, height=8)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))

OUT <- NULL
HI.COV <- NULL

for(i in 1:length(files)){
  ls5.stack <- brick(files[i])
  ## calculate cloud mask using QA band
  try(raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[1:3]), silent=TRUE)
    graphics::text(x=lo.x, y=hi.y, labels=paste0('pre-mask\n',files[i]), col='yellow', adj=c(0,1))
  qa.test <- classifyQA(ls5.stack$BQA_dn, type=c('cloud'), sensor='TM', confLayers=TRUE) ## create cloud mask with classifyQA
  plot(qa.test) ## plot cloud mask
    graphics::text(x=lo.x, y=hi.y, labels=paste0('QA cloud result\n',files[i]), col='black', adj=c(0,1))
  qa.test[qa.test > 1] <- NA ## if confLayers==TRUE
  ## apply mask to scene
  msk.ls5.stack <- mask(ls5.stack, mask=qa.test)
  try(plotRGB(msk.ls5.stack, r=3, g=2, b=1, scale=(msk.ls5.stack@data@max[1:3])), silent=TRUE)
    graphics::text(x=lo.x, y=hi.y, labels=paste0('post-mask\n',files[i]), col='yellow', adj=c(0,1))
  
  ## calculate proportion of cloud-masked cells
  qa.freq <- freq(qa.test, value=NA) ## number of cells ID'ed w/ cloud cover
  n.cells <- dim(ls5.stack)[1]*dim(ls5.stack)[2] ## number of cells in cropped scene
  ## If cloud cover is >10%, save name of file for later visualization
  scene <- gsub('_CROPPED.grd', '', files[i])
  if(qa.freq/n.cells >= 0.1){
    # raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[1:3])
    #   graphics::text(x=lo.x, y=hi.y, labels=paste0('pre-mask - FAIL\n',files[i]), col='yellow', adj=c(0,1))
    HI.COV <- c(HI.COV, scene)
    # plot(qa.test)
    #   graphics::text(x=lo.x, y=hi.y, labels=paste0('QA cloud result - FAIL\n',files[i]), col='black', adj=c(0,1))
      save <- c(scene, qa.freq/n.cells)
      OUT <- rbind(OUT, save)
  }
  
  ## If cloud cover is <10%, proceed with processing
  if(qa.freq/n.cells < 0.1){
    ## read in metadata for original scene
    d <- gsub('_CROPPED.grd', '', files[i])
    setwd(paste0('/Volumes/avril_data/krat_remote_sensing/raw_landsat45tm_scene_downloads/',d,'/'))
    md.file <- list.files(pattern=glob2rx('*MTL.txt'), full.names=TRUE)
    m.data <- readMeta(md.file) ## works for LS5 Collection 1 Level 1 data - errors with Collection 2 Level 1
    # setwd('/Volumes/avril_data/krat_remote_sensing/cropped_landsat45tm_scenes/')
    setwd('/Users/Avril/Documents/krat_remote_sensing/cropped_landsat45tm_scenes/')
    
    ## apply TOA correction - 'apref' = apparent reflectance (top-of-atmosphere reflectance)
    ls5.cor.stack <- radCor(msk.ls5.stack, m.data, method='apref', verbose=FALSE, bandSet=m.data$DATA$BANDS)
    raster::plotRGB(ls5.cor.stack, r=3, g=2, b=1, scale=max(ls5.cor.stack@layers[[1]]@data@max,
                                ls5.cor.stack@layers[[2]]@data@max, ls5.cor.stack@layers[[3]]@data@max))
      graphics::text(x=lo.x, y=hi.y, labels=paste0('post-TOA\n',files[i]), col='black', adj=c(0,1))
    
    ## apply atmospheric correction (haze removal)
    haze <- estimateHaze(msk.ls5.stack, darkProp = 0.01, hazeBand = c("B1_dn", "B2_dn", "B3_dn", "B4_dn"), plot=FALSE)
    haz.msk.ls5.stack <- radCor(msk.ls5.stack, metaData = m.data, hazeValues = haze,
                                hazeBands = c("B1_dn", "B2_dn", "B3_dn", "B4_dn"), method = "sdos")
    raster::plotRGB(haz.msk.ls5.stack, r=3, g=2, b=1, scale=max(haz.msk.ls5.stack@layers[[1]]@data@max,
                                 haz.msk.ls5.stack@layers[[2]]@data@max, haz.msk.ls5.stack@layers[[3]]@data@max))
      graphics::text(x=lo.x, y=hi.y, labels=paste0('post-TOA, post-haze\n',files[i]), col='black', adj=c(0,1))
    
    
    ## manually select the Landsat5TM bands you need for Tasseled Cap (1,2,3,4,5,7)
    tc.cor.stack <- raster::subset(haz.msk.ls5.stack, subset=c('B1_sre','B2_sre','B3_sre','B4_sre','B5_sre','B7_sre'))
    ## apply the Tasseled Cap transformation
    ls5.tc.cor <- tasseledCap(tc.cor.stack, sat='Landsat5TM')
    par(mar=c(5.1,4.1,4.1,2.1))
    plot(ls5.tc.cor$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(files[i],'\ngreenness'))
      points(mnd.locs, pch=19, cex=0.2)
    plot(ls5.tc.cor$brightness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(files[i],'\nbrightness'))
      points(mnd.locs, pch=19, cex=0.2)
    plot(ls5.tc.cor$wetness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(files[i],'\nwetness'))
      points(mnd.locs, pch=19, cex=0.2)
    
    ## extract TC pixel data for mounds & surrounding pixels
    ## for pixel mound is located in (buffer = ## meters);
    ## median dispersal distances from birth to reproductive mound = 77.5 m (females) and 40 m (males)
    g.ness <- extract(ls5.tc.cor$greenness, mnd.locs, method='simple', df=TRUE, cellnumbers=TRUE, buffer=78)
    w.ness <- extract(ls5.tc.cor$wetness, mnd.locs, method='simple', df=TRUE, cellnumbers=TRUE, buffer=78)
    b.ness <- extract(ls5.tc.cor$brightness, mnd.locs, method='simple', df=TRUE, cellnumbers=TRUE, buffer=78)
    ## check TC output, just to be safe
    stopifnot(all(g.ness[,c(1,2)] == w.ness[,c(1,2)]), all(g.ness[,c(1,2)] == b.ness[,c(1,2)]))
    
    # ## plot what this buffer would extract
    # plot(ls5.tc.cor$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n',
    #      xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='Buffer extracted\n(greenness values)')
    #   points(mnd.locs, pch=19, cex=0.2)
    #   r2 <- ls5.tc.cor$greenness
    #   r2[setdiff(seq_len(ncell(r2)), unique(g.ness[,2]))] <- NA
    #   r2[!is.na(r2)] <- 1
    #   plot(rasterToPolygons(r2, dissolve=TRUE), add=TRUE, border='red', lwd=2)
    # ## maybe save data for focal pixels, then figure out which pixels are in the buffer zone, and save values for those,
    # ## with key linking cell numbers in buffer zone to corresponding focal pixels?
  
    ## define data to be saved and format for writing output
    scene.id <- m.data$SCENE_ID
    acq.date <- m.data$ACQUISITION_DATE
    path <- m.data$PATH_ROW[1]
    row <- m.data$PATH_ROW[2]
    doy <- as.numeric(strftime(acq.date, format='%j'))
    all.tc <- merge(x=g.ness, y=w.ness, by=c('ID','cells'))
    all.tc <- merge(x=all.tc, y=b.ness, by=c('ID','cells'))
    all.tc$acq.date <- acq.date
    all.tc$doy <- doy
    all.tc$scene.id <- scene.id
    all.tc$path <- path
    all.tc$row <- row
    all.tc$nrows <- ls5.tc.cor$brightness@nrows
    all.tc$ncols <- ls5.tc.cor$brightness@ncols

    ## -- write to a file continuously to free up memory
    write.table(all.tc, file=paste0('../tc_output_tables/',tc.fn,'.csv'), quote=FALSE, append=TRUE,
                row.names=FALSE, sep=',', col.names=!file.exists(paste0('../tc_output_tables/',tc.fn,'.csv')))

    ## get cell number for each mound and save scene information
    mnd.cell.dat <- cbind(mnd.cells, (cellFromXY(ls5.tc.cor, mnd.locs)))
    colnames(mnd.cell.dat)[3] <- 'cell.num'
    mnd.cell.dat$acq.date <- acq.date
    mnd.cell.dat$doy <- doy
    mnd.cell.dat$scene.id <- scene.id
    mnd.cell.dat$path <- path
    mnd.cell.dat$row <- row

    ## -- write to a file continuously to free up memory
    write.table(mnd.cell.dat, paste0('../tc_output_tables/',mc.fn,'.csv'), quote=FALSE, append=TRUE,
                row.names=FALSE, sep=',', col.names=!file.exists(paste0('../tc_output_tables/',mc.fn,'.csv')))
  }
  print(i/length(files))
}
dev.off()

## plot scenes with problematic cloud cover, according to QA band
pdf('/Users/Avril/Desktop/cloud_qa_fails.pdf', width=8, height=8)
for(j in HI.COV){
  ls5.stack <- brick(paste0(j,'_CROPPED.grd'))
  raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[1:3])
    graphics::text(x=lo.x, y=hi.y, labels=paste0('pre-mask - FAIL\n',files[i]), col='yellow', adj=c(0,1))
  plot(qa.test)
    graphics::text(x=lo.x, y=hi.y, labels=paste0('QA cloud result - FAIL\n',files[i]), col='black', adj=c(0,1))
}
dev.off()
#

##### Need to manually review both PDFs (cloud_qa_fails.pdf and test.pdf) to make sure that QA didn't misclassify images #####





##### Do some data viz for current run #####
setwd('/Volumes/avril_data/krat_remote_sensing/tc_output_tables/')

tc.dat <- read.csv(paste0(tc.fn,'.csv'))
mc.key <- read.csv(paste0(mc.fn,'.csv'))

## for a single cell, plot trends in TC metrics over year
temp <- tc.dat[which(tc.dat$cells == unique(tc.dat$cells)[1]),]
temp$year <- as.numeric(do.call(rbind, strsplit(as.character(temp$acq.date), split='-', fixed=TRUE))[,1])
plot(temp$doy, temp$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$brightness, pch=19, col=temp$year, cex=0.8)
## compare with another cell
temp1 <- tc.dat[which(tc.dat$cells == unique(tc.dat$cell)[300]),]
temp1$year <- as.numeric(do.call(rbind, strsplit(as.character(temp1$acq.date), split='-', fixed=TRUE))[,1])
plot(temp1$doy, temp1$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$brightness, pch=19, col=temp$year, cex=0.8)

par(mfrow=c(1,2))
plot(temp$doy, temp$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$brightness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$brightness, pch=19, col=temp$year, cex=0.8)

## plot lines by year within a cell
palette.pals()
cols <- palette.colors(n=length(unique(temp$year)), palette='Polychrome36', recycle=FALSE)
plot(temp$doy, temp$greenness, col='transparent')
temp <- temp[complete.cases(temp),]
for(i in 1:length(unique(temp$year))){
  sub <- temp[which(temp$year == unique(temp$year)[i]),]
  sub <- sub[order(sub$doy),]
  lines(x=sub$doy, y=sub$greenness, col=cols[i])
}
