# install.packages('pROC')
# install.packages('raster')
# install.packages('RStoolbox')
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)

##### Read in mound waypoints for later extraction #####
mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84") 
mnd.locs <- spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

##### Loop over directories, processing scenes and saving TC data #####
# ## set wd for test directory - can run as loop over multiple directories later
# setwd('/Users/Avril/Documents/krat_remote_sensing/landsat_5_downloads/LT05_L1TP_035038_20040611_20160914_01_T1/')

setwd('/Users/Avril/Documents/krat_remote_sensing/landsat_5_downloads/')
dirs <- list.files()

pdf(file='/Users/Avril/Desktop/test_run.pdf', width=6, height=6)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))

CHECK.TC <- NULL
TC.DATA <- NULL
MND.CELLS <- NULL

## set extent for all analyses
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750
ext <- extent(lo.x, hi.x, lo.y, hi.y)

for(i in dirs){
  setwd(paste0('/Users/Avril/Documents/krat_remote_sensing/landsat_5_downloads/',i,'/'))
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
  
  ## crop raster data by extent to reduce processing times
  ls5.stack <- crop(ls5.stack, ext)
  
  # ## calculate cloud mask -- best way to do this? 
  # cld.msk <- cloudMask(ls5.stack, blue='B1_dn', tir='B6_dn', buffer=NULL, plot=TRUE)
  # ggR(cld.msk, 2, geom_raster=TRUE)
  
  ## apply TOA correction - 'apref' = apparent reflectance (top-of-atmosphere reflectance)
  ls5.cor.stack <- radCor(ls5.stack, m.data, method='apref', verbose=TRUE, bandSet=m.data$DATA$BANDS)
  
  ## manually select the Landsat5TM bands you need for Tasseled Cap (1,2,3,4,5,7)
  tc.cor.stack <- raster::subset(ls5.cor.stack, subset=c('B1_tre','B2_tre','B3_tre','B4_tre','B5_tre','B7_tre'))
  ## apply the Tasseled Cap transformation
  ls5.tc.cor <- tasseledCap(tc.cor.stack, sat='Landsat5TM')
  
  plot(ls5.tc.cor$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(i,'\ngreenness'))
    points(mnd.locs, pch=19, cex=0.2)

  plot(ls5.tc.cor$brightness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(i,'\nbrightness'))
    points(mnd.locs, pch=19, cex=0.2)

  plot(ls5.tc.cor$wetness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(i,'\nwetness'))
    points(mnd.locs, pch=19, cex=0.2)
  # dev.off()
  
  ## extract TC pixel data for mounds & surrounding pixels
  ## for pixel mound is located in (buffer = ## meters);
  ## median dispersal distances from birth to reproductive mound = 77.5 m (females) and 40 m (males)
  g.ness <- extract(ls5.tc.cor$greenness, mnd.locs, method='simple', df=TRUE, cellnumbers=TRUE, buffer=78)
  w.ness <- extract(ls5.tc.cor$wetness, mnd.locs, method='simple', df=TRUE, cellnumbers=TRUE, buffer=78)
  b.ness <- extract(ls5.tc.cor$brightness, mnd.locs, method='simple', df=TRUE, cellnumbers=TRUE, buffer=78)
  ## check TC output, just to be safe
  scene.id <- m.data$SCENE_ID
  if(all(g.ness[,c(1,2)] == w.ness[,c(1,2)]) & all(g.ness[,c(1,2)] == b.ness[,c(1,2)])){
    tc.check <- c(scene.id, 'PASS')
    CHECK.TC <- rbind(CHECK.TC, tc.check)
  }
  
  ## plot what this buffer would extract
  plot(ls5.tc.cor$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n',
       xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='Buffer extracted\n(greenness values)')
    points(mnd.locs, pch=19, cex=0.2)
    r2 <- ls5.tc.cor$greenness
    r2[setdiff(seq_len(ncell(r2)), unique(g.ness[,2]))] <- NA
    r2[!is.na(r2)] <- 1
    plot(rasterToPolygons(r2, dissolve=TRUE), add=TRUE, border='red', lwd=2)
  ## maybe save data for focal pixels, then figure out which pixels are in the buffer zone, and save values for those,
  ## with key linking cell numbers in buffer zone to corresponding focal pixels?
    
  ## define data to be saved and format for writing output
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
  ## save all focal + buffer cell TC values
  TC.DATA <- rbind(TC.DATA, all.tc)

  ## get cell number for each mound and save scene information
  mnd.cell.dat <- cbind(mnd.cells, (cellFromXY(ls5.tc.cor, mnd.locs)))
  colnames(mnd.cell.dat)[3] <- 'cell.num'
  mnd.cell.dat$acq.date <- acq.date
  mnd.cell.dat$doy <- doy
  mnd.cell.dat$scene.id <- scene.id
  mnd.cell.dat$path <- path
  mnd.cell.dat$row <- row
  ## save mound/cell ID information
  MND.CELLS <- rbind(MND.CELLS, mnd.cell.dat)
  
  print(i)
}
dev.off()

head(CHECK.TC)
head(TC.DATA)
head(MND.CELLS)







