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
source('/Users/Avril/Documents/krat_remote_sensing/krat_remote_sensing_scripts/c2l2readMeta.R')
`%notin%` <- Negate(`%in%`)

g.col <- 'forestgreen'
w.col <- 'deepskyblue2'
b.col <- 'tan4'
n.col <- 'chartreuse3'

##### 1. Read in mound waypoints for later extraction + manual assignments #####
mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
mnd.locs <- mnd.locs[c('long','lat','database.name')] ## rearrange/rename to match man.locs below
colnames(mnd.locs)[3] <- 'terr'
coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84") ## still in degrees
mnd.locs <- sp::spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")) ## now in meters

### manual mound:cell assignments
man.ass <- read.csv('/Users/Avril/Documents/krat_remote_sensing/archive/sorting_out_mound_names/manual_mound_cell_assignments.csv')
man.cells <- man.ass[,c('terr','new.db.name','cell')]
man.cells[!is.na(man.cells$new.db.name), 'terr'] <- man.cells[!is.na(man.cells$new.db.name), 'new.db.name']
man.cells$ID <- c((max(mnd.cells$ID)+1):(max(mnd.cells$ID)+nrow(man.cells)))
colnames(man.cells)[1] <- 'database.name'
man.cells <- man.cells[,c('database.name','ID')]

## read in an example raster to get coordinates
setwd('/Users/Avril/Documents/krat_remote_sensing/archive/proposal_sat_map_stuff/LE07_L1TP_035038_20020817_20160928_01_T1/')
all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"),
                                full.names = TRUE)
crit_bands <- all_landsat_bands[c(1:5,7)]
ls5_stack <- stack(crit_bands)
## set extent for all analyses + crop 
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750
ext <- extent(lo.x, hi.x, lo.y, hi.y)
ls5_stack <- crop(ls5_stack, ext)
man.locs <- as.data.frame(xyFromCell(ls5_stack, man.ass$cell))
man.locs$terr <- man.ass$terr
coordinates(man.locs) <- c('x','y') ## already in meters
proj4string(man.locs)<- CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

## confirm proper coordinate loading for both sets -- checks out
raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
  points(mnd.locs, cex=0.5, col='yellow', pch=19)
  points(man.locs, cex=0.5, col='green', pch=19)

## combine two sets of points, plot to confirm
all.cells <- rbind(mnd.cells, man.cells)
all.locs <- rbind(mnd.locs, man.locs)
raster::plotRGB(ls5_stack, r=3,g=2,b=1, ext=ext)
  points(all.locs, cex=0.5, col='blue', pch=19) ## looks good
  
rm(ext, all_landsat_bands, crit_bands, 
   ls5_stack, mnd.locs, man.locs, man.ass, mnd.cells, man.cells) ## cleanup before looping

# ##### 2. Loop over directories of raw data and write cropped raster files (output already saved) #####
# ### Only need to run this loop once ###
# ## loop over downloads in scratch space (n=647)
# setwd('/Volumes/avril_data/krat_remote_sensing/C2L2_raw_landsat45tm_scene_downloads/')
# dirs <- list.files()
#
# # set extent for all analyses
# lo.x <- 663500
# hi.x <- 665600
# lo.y <- 3497000
# hi.y <- 3499750
# ext <- extent(lo.x, hi.x, lo.y, hi.y)
#
# for(i in 1:length(dirs)){
#   setwd(paste0('/Volumes/avril_data/krat_remote_sensing/C2L2_raw_landsat45tm_scene_downloads/',dirs[i],'/'))
#   ## read in data
#   all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"), full.names = TRUE) ## get all bands, including QA and CLOUD_QA
#   # ls5.stack <- stack(all_landsat_bands)
#
#   if(length(all_landsat_bands) != 0){ ## if files haven't been renamed yet, rename them
#     ## get new filenames and rename TIF files
#     new.names <- gsub('CLOUD_QA', 'CLOUD-QA', all_landsat_bands)
#     strsplit(new.names[grep('CLOUD_QA', new.names)], split='_', fixed=TRUE)
#     new.names <- do.call(rbind, strsplit(new.names, split='_', fixed=TRUE))[,9]
#     new.names <- paste0(gsub('.TIF','',new.names),'_sr')
#     file.rename(all_landsat_bands, new.names)
#     ls5.stack <- stack(new.names)
#   } else{ ## otherwise, they've already been renamed and can be read in with this loop
#     new.names <- list.files(pattern = glob2rx("*_sr$"), full.names = TRUE)
#     ls5.stack <- stack(new.names)
#   }
#
#   ## read in metadata
#   md.file <- list.files(pattern=glob2rx("*MTL.txt"), full.names=TRUE)
#   m.data <- c2l2readMeta(md.file) ## uses an edited version of readMeta to read in metadata from C2L2-formatted *_MTL.txt files
#
#   ## crop raster data by extent to reduce processing times and write to file
#   ls5.stack <- crop(ls5.stack, ext)
#
#   writeRaster(ls5.stack, filename=paste0('/Volumes/avril_data/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/',dirs[i],'_CROPPED.grd'), progress='text', overwrite=TRUE)
#   # ls5.stack <- brick(paste0('/Volumes/avril_data/krat_remote_sensing/C2L2_croppped_landsat45tm_scenes/',i,'_CROPPED.grd'))
#
#   print(i)
# }
#
#
# ## for scenes of interest, print cropped image for manual cloud-cover check and write information
# ## for taking notes on image quality in .csv file
# setwd('/Users/Avril/Documents/krat_remote_sensing/')
# files <- read.table('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/C2L2_low_cloud_scenes.txt') ## scenes with <= 20% cloud cover (n=460)
# raw.files <- files$V1
# files$V1 <- paste0(files$V1,'_CROPPED.grd')
# files <- as.vector(files$V1)
# scenes.avail <- read.csv('C2L2_landsat_5_data_overviews/landsat_tm_c2_l2_all_scenes_avail.csv') ## read in info to get cloud cover data for whole scenes
#
# setwd('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/')
# pdf('../C2L2_landsat_5_data_overviews/C2L2_low_cloud_scenes_raw_images.pdf', width=8, height=8)
# par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
# OUT <- NULL
# for(i in 1:length(files)){
#   print(i)
#   ls5.stack <- brick(files[i])
#   plot.new()
#   try(raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)]), silent=TRUE)
#   # try(raster::plotRGB(ls5.stack, r=3, g=2, b=1), silent=FALSE)
#   graphics::legend('topleft', paste0(i,' - ',files[i]), bty='n', text.col='darkgreen')
#   save <- c(i, files[i], scenes.avail[which(scenes.avail$Landsat.Product.Identifier.L2==raw.files[i]), 'Land.Cloud.Cover'])
#   OUT <- rbind(OUT, save)
# }
# dev.off()
# colnames(OUT) <- c('index','filename','cover')
# write.csv(OUT, '../C2L2_landsat_5_data_overviews/C2L2_low_cloud_scenes_cloud_cover_notes.csv',
#           row.names=FALSE)
#
# ## because limiting to just Path 35 (not 34), reviewing additional scenes to try to increase sample size (cover > 20%) (Jan. 27, 2022)
# setwd('/Users/Avril/Documents/krat_remote_sensing/')
# files <- read.table('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/C2L2_high_cloud_scenes.txt') ## scenes with > 20% cloud cover (n=101)
# raw.files <- files$V1
# files$V1 <- paste0(files$V1,'_CROPPED.grd')
# files <- as.vector(files$V1)
# scenes.avail <- read.csv('C2L2_landsat_5_data_overviews/landsat_tm_c2_l2_all_scenes_avail.csv') ## read in info to get cloud cover data for whole scenes
#
# setwd('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/')
# pdf('../C2L2_landsat_5_data_overviews/C2L2_high_cloud_scenes.pdf', width=8, height=8)
# par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
# OUT <- NULL
# for(i in 1:length(files)){
#   print(i)
#   ls5.stack <- brick(files[i])
#   plot.new()
#   try(raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)]), silent=TRUE)
#   # try(raster::plotRGB(ls5.stack, r=3, g=2, b=1), silent=FALSE)
#     graphics::legend('topleft', paste0(i,' - ',files[i]), bty='n', text.col='darkgreen')
#   save <- c(i, files[i], scenes.avail[which(scenes.avail$Landsat.Product.Identifier.L2==raw.files[i]), 'Land.Cloud.Cover'])
#   OUT <- rbind(OUT, save)
# }
# dev.off()
# colnames(OUT) <- c('index','filename','cover')
# write.csv(OUT, '../C2L2_landsat_5_data_overviews/C2L2_high_cloud_scenes_cloud_cover_notes.csv',
#           row.names=FALSE)

##### 3. Loop over cropped scenes, process, and write TC output #####
### ** Requires external drive for reading in metadata files ** ###
## set file names for run
tc.fn <- 'tc_manual_cloudcheck' ## for TC data -- tc_manual_cloudcheck
mc.fn <- 'mnd_manual_cloudcheck' ## for mound/cell ID key -- mnd_manual_cloudcheck

setwd('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/')

## read in results of manual cloud checking to get list of scenes to process
## read in results of checking scenes with <= 20% cloud cover
res <- read.csv('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/C2L2_low_cloud_scenes_cloud_cover_notes.csv')
table(res$usable) ## 365 / 460 scenes checked are usable (i.e., no clouds over extent or processing errors)
res <- res[which(res$usable == 1),]
res$path <- do.call(rbind, strsplit(res$filename, split='_'))[,3]
res <- res[res$path == '035038',] ## only keep scenes from Path 35
files1 <- as.vector(res$filename)
## read in results of checking scenes with > 20% cloud cover
res <- read.csv('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/C2L2_high_cloud_scenes_cloud_cover_notes.csv')
table(res$usable) ## 20 / 62 scenes checked are usable (i.e., no clouds over extent or processing errors)
res <- res[which(res$usable == 1),]
files2 <- as.vector(res$filename)
## combine lists of files
files <- c(files1, files2)

# pdf(file=paste0('/Users/Avril/Desktop/',tc.fn,'.pdf'), width=8, height=8)
# par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))

# ## temp, 2/3/22, just to write some series images
# years <- do.call(rbind, strsplit(files, split='_', fixed=TRUE))[,4]
# years <- substr(years, 1,4)
# files <- files[which(years == 1998)]

# OUT <- NULL
# for(i in 1:length(files)){
#   ls5.stack <- brick(files[i])
#   # plot.new()
#   # try(raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)]), silent=TRUE)
#   # # try(raster::plotRGB(ls5.stack, r=3, g=2, b=1), silent=FALSE)
#   #   graphics::legend('topleft', paste0(i,' - ',files[i]), bty='n', text.col='darkgreen')
# 
#   ## masking clouds using CLOUD_QA or QA band isn't very accurate;
#   ## just manually review plots to check for cloud cover
#   scene <- gsub('_CROPPED.grd', '', files[i])
#   ## read in metadata for original scene
#   d <- gsub('_CROPPED.grd', '', files[i])
#   setwd(paste0('/Volumes/avril_data/nsf_postdoc_data/krat_remote_sensing/C2L2_raw_landsat45tm_scene_downloads/',d,'/'))
#   md.file <- list.files(pattern=glob2rx('*MTL.txt'), full.names=TRUE)
#   m.data <- c2l2readMeta(md.file)
#   setwd('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/')
# 
#   ## apply scaling factors appropriate for C2L2 data
#   ## surface reflectance data
#   for(l in c(1:5,7)){
#     vals <- getValues(ls5.stack[[l]])
#     vals <- 0.0000275 * vals - 0.2 ## convert DN to surface reflectance
#     ls5.stack[[l]] <- setValues(ls5.stack[[l]], vals)
#   }
#   ## surface temperature data
#   vals <- getValues(ls5.stack[[6]])
#   vals <- 0.00341802 * vals + 149.0 ## convert DN to degrees Kelvin
#   ls5.stack[[6]] <- vals
# 
#   ## manually select the Landsat5TM bands you need for Tasseled Cap (1,2,3,4,5,7)
#   tc.cor.stack <- raster::subset(ls5.stack, subset=c('B1_sr','B2_sr','B3_sr','B4_sr','B5_sr','B7_sr'))
# 
#   ## apply the Tasseled Cap transformation
#   ls5.tc.cor <- tasseledCap(tc.cor.stack, sat='Landsat5TM')
#   # par(mar=c(5.1,4.1,4.1,2.1))
#   # plot(ls5.tc.cor$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(files[i],'\ngreenness'),
#   #      col=viridis(100),zlim=c(-0.1, 0.2)) ## turn this on when plotting and need same scale across images
#   #   points(all.locs, pch=19, cex=0.2, col='white')
#   # plot(ls5.tc.cor$brightness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(files[i],'\nbrightness'),
#   #      col=viridis(100),zlim=c(0.1, 0.55)) ## turn this on when plotting and need same scale across images
#   #   points(all.locs, pch=19, cex=0.2, col='white')
#   # plot(ls5.tc.cor$wetness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', main=paste0(files[i],'\nwetness'),
#   #        col=viridis(100),zlim=c(0.05, 0.25)) ## turn this on when plotting and need same scale across images
#   #   points(all.locs, pch=19, cex=0.2, col='white')
#   
#   ## keep TC values for all cells in case you wanna use lag years (and there would be different data availability
#   ## based on buffers in that case) -- buffer code available in archived version
#   nc <- ncell(ls5.tc.cor)
#   g.ness <- cbind(c(1:nc), as.data.frame(ls5.tc.cor$greenness))
#   w.ness <- cbind(c(1:nc), as.data.frame(ls5.tc.cor$wetness))
#   b.ness <- cbind(c(1:nc), as.data.frame(ls5.tc.cor$brightness))
#   ## keep surface temperature data, too
#   temp.k <- cbind(c(1:nc), as.data.frame(ls5.stack$B6_sr))
#   colnames(temp.k)[2] <- 'temp.k'
#   
#   ## just calculate all the indices supported by this function
#   oth.ind <- spectralIndices(ls5.stack, blue='B1_sr', green='B2_sr', red='B3_sr', nir='B4_sr', swir2='B5_sr', swir3='B7_sr', tir2='B6_sr')
#   
#   ## define data to be saved and format for writing output
#   scene.id <- m.data$SCENE_ID
#   prod.id <- gsub('_CROPPED.grd', '', files[i])
#   acq.date <- m.data$ACQUISITION_DATE
#   path <- m.data$PATH_ROW[1]
#   row <- m.data$PATH_ROW[2]
#   doy <- as.numeric(strftime(acq.date, format='%j'))
#   all.tc <- merge(x=g.ness, y=w.ness, by=c('c(1:nc)'))
#   all.tc <- merge(x=all.tc, y=b.ness, by=c('c(1:nc)'))
#   all.tc <- merge(x=all.tc, y=temp.k, by=c('c(1:nc)'))
#   for(l in 1:dim(oth.ind)[3]){
#     temp <- cbind(c(1:nc), as.data.frame(oth.ind[[l]]))
#     all.tc <- merge(x=all.tc, y=temp, by=c('c(1:nc)'))
#   }
#   all.tc$acq.date <- acq.date
#   all.tc$doy <- doy
#   all.tc$scene.id <- scene.id
#   all.tc$prod.id <- prod.id
#   all.tc$path <- path
#   all.tc$row <- row
#   all.tc$nrows <- ls5.tc.cor$brightness@nrows
#   all.tc$ncols <- ls5.tc.cor$brightness@ncols
#   colnames(all.tc)[1] <- 'cell.num'
#   
#   ## -- write to a file continuously to free up memory
#   write.table(all.tc, file=paste0('../C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'), quote=FALSE, append=TRUE,
#               row.names=FALSE, sep=',', col.names=!file.exists(paste0('../C2L2_tc_output_tables/C2L2_',tc.fn,'.csv')))
#   
#   ## get cell number for each mound and save scene information
#   mnd.cell.dat <- cbind(all.cells, (cellFromXY(ls5.tc.cor, all.locs)))
#   colnames(mnd.cell.dat)[3] <- 'cell.num'
#   mnd.cell.dat$acq.date <- acq.date
#   mnd.cell.dat$doy <- doy
#   mnd.cell.dat$scene.id <- scene.id
#   mnd.cell.dat$prod.id <- prod.id
#   mnd.cell.dat$path <- path
#   mnd.cell.dat$row <- row
#   
#   ## -- write to a file continuously to free up memory
#   write.table(mnd.cell.dat, paste0('../C2L2_tc_output_tables/C2L2_',mc.fn,'.csv'), quote=FALSE, append=TRUE,
#               row.names=FALSE, sep=',', col.names=!file.exists(paste0('../C2L2_tc_output_tables/C2L2_',mc.fn,'.csv')))
#   
#   print(i/length(files))
# }
# # dev.off()

##### 4. Do some basic data viz and correlation checks for current run to check for issues #####
setwd('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/')

tc.fn <- 'tc_manual_cloudcheck' ## for TC data -- tc_manual_cloudcheck
mc.fn <- 'mnd_manual_cloudcheck' ## for mound/cell ID key -- mnd_manual_cloudcheck

tc.dat <- read.csv(paste0('C2L2_',tc.fn,'.csv'))
mc.key <- read.csv(paste0('C2L2_',mc.fn,'.csv'))

## look for correlations among TC metrics and other indices
## for a single cell in a single scene
temp <- tc.dat[which(tc.dat$cell.num == unique(tc.dat$cell.num)[1]),]
# pdf('/Users/Avril/Desktop/single_scene_index_correlations.pdf', width=15, height=15)
pairs(temp[,c(2:27)], col=alpha('dodgerblue3', 0.5))
# dev.off()

## a more comprehensive check with multiple scenes and cells, randomly sampled from all
## data
size <- 1000     ## number of cells to sample
samps <- sample(1:nrow(tc.dat), size, replace=FALSE)
temp <- tc.dat[samps,]
# pdf(paste0('/Users/Avril/Desktop/',size,'_randomcells_index_correlations.pdf'), width=15, height=15)
pairs(temp[,c(2:22)], col=alpha('dodgerblue3', 0.2))
# dev.off()

## quantify correlations among indices
cors <- abs(cor(temp[,c(2:22)]))

### Plot information on final set of scenes retained
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,1]
scenes.avail <- tc.dat[,c('year','doy','path')]
scenes.avail <- scenes.avail[!duplicated(scenes.avail),]
cloud.col <- 0.9

pdf('/Users/Avril/Desktop/C2L2_landsat5_data_across_years_manual_cloudcheck.pdf', width=20, height=7)
par(mar=c(5.1, 5.1, 6.1, 2.1))
plot(scenes.avail$doy, scenes.avail$year, pch=19, cex=3, xlab='Day of Year', ylab='Year',
     col='transparent', ylim=c(1989, 2005), cex.axis=1.5, cex.lab=1.5)
  ## add polygons for weather-based seasons
  polygon(c(182,243,243,182), c(1985,1985,2007,2007), border=NA, col='grey85') ## July 1 (day 182) - Aug 31 (day 243)
  polygon(c(335,365,365,335), c(1985,1985,2007,2007), border=NA, col='grey85') ## Dec 1 (day 335) - March (day 90)
  polygon(c(0,90,90,0), c(1985,1985,2007,2007), border=NA, col='grey85') ## Dec 1 (day 335) - March (day 90)
  points(scenes.avail[scenes.avail$path==34, 'doy'], scenes.avail[scenes.avail$path==34, 'year'], pch=21, cex=3, xlab='Day of Year', ylab='Year',
         col='black', bg=alpha('dodgerblue2', alpha=cloud.col), ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  points(scenes.avail[scenes.avail$path==35, 'doy'], scenes.avail[scenes.avail$path==35, 'year'], pch=22, cex=3, xlab='Day of Year', ylab='Year',
         col='black', bg=alpha('dodgerblue4', alpha=cloud.col), ylim=c(1989, 2007), cex.axis=1.5, cex.lab=1.5)
  # lines(x=c(91,91), y=c(1980, 2006), lty=2, lwd=2) ## April 1
  # lines(x=c(305,305), y=c(1980, 2006), lty=2, lwd=2) ## November 1
  # text(c(91, 305), c(2006.5, 2006.5), labels=c('Apr 1', 'Nov 1'), cex=1.5)
  legend((365/2), 2009.5, xpd=TRUE, pch=c(21,22), legend=c('34', '35'), title='Path Number', horiz=TRUE, xjust=0.5,
         cex=1.5, col='black', pt.cex=3, text.width=12, x.intersp=1.8, pt.bg=c(alpha('dodgerblue3', alpha=cloud.col), alpha('dodgerblue4', alpha=cloud.col)))
dev.off()
