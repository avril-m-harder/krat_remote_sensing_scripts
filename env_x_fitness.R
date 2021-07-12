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

##### Read in mound waypoints #####
mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84")
mnd.locs <- sp::spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

##### Read in population (Peter's) data #####
pop.dat <- read.csv('/Users/Avril/Documents/krat_genetics/preseq_sample_information/KRATP.csv')

##### Read in TC and other index results #####
## set file names to be read in 
tc.fn <- 'tc_cloud_free' ## for TC data -- tc_initial_test    tc_cloud_free
mc.fn <- 'mnd_key_cloud_free' ## for mound/cell ID key -- mnd_key_initial_test    mnd_key_cloud_free

## read in list of scenes of interest (e.g., cloud-free)
# files <- list.files(pattern="*.grd") ## to run with ALL scenes (causes some issues due to cloud masking step)
# files <- read.table('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/C2L2_intermediate_cloud_scenes.txt') ## scenes with <=60% cloud cover (n=574)
files <- read.table('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/C2L2_cloud_free_scenes.txt') ## scenes with 0% cloud cover (n=233)
files$V1 <- paste0(files$V1,'_CROPPED.grd')
files <- as.vector(files$V1)

## results of manual checks: for cloud-free scenes, corrupted info in...
## 90, 92, 93, 103, 105, 107, 109, 110, 112, 113, 116, 119, 122, 156, 159, 166
## 156, 159, 166 really weird - like they're not the correct location?
remove <- c(90, 92, 93, 103, 105, 107, 109, 110, 112, 113, 116, 119, 122, 156, 159, 166)
scenes.rm <- gsub('_CROPPED.grd', '', files[remove])

tc.dat <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'))
mc.key <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',mc.fn,'.csv'))

## remove scenes with weird data (for cloud-free scenes, this really improves the outlier situation)
tc.dat <- tc.dat[which(tc.dat$prod.id %notin% scenes.rm),]
mc.key <- mc.key[which(mc.key$prod.id %notin% scenes.rm),]

##### Define seasons #####
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,1]
## define brood-year lag months (e.g., for indivs born in 2000, lag.year would be set to 2000 for June 1999-May 2000)
## i.e., abiotic data from the months of June - May are predicting survival/fitness for indivs presumably born between those months
tc.dat$year <- as.numeric(tc.dat$year)
tc.dat$lag.year <- as.numeric(tc.dat$year)
tc.dat[which(tc.dat$month %in% c(6:12)), 'lag.year'] <- tc.dat[which(tc.dat$month %in% c(6:12)), 'year'] + 1 
tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,2]
tc.dat$month <- as.numeric(tc.dat$month)
### two seasonal options:
# ## (1) define seasons by quarters (1=spring, 2=summer, 3=fall, 4=winter)
# tc.dat[which(tc.dat$month %in% c(1,2,3)), 'szn'] <- 1
# tc.dat[which(tc.dat$month %in% c(4,5,6)), 'szn'] <- 2
# tc.dat[which(tc.dat$month %in% c(7,8,9)), 'szn'] <- 3
# tc.dat[which(tc.dat$month %in% c(10,11,12)), 'szn'] <- 4

## (2) define seasons by expected precip (1=June-August, 2=Dec-March, 0=outside these months)
tc.dat[which(tc.dat$month %in% c(6,7,8)), 'szn'] <- 1
tc.dat[which(tc.dat$month %in% c(12,1,2,3)), 'szn'] <- 2
tc.dat[which(tc.dat$month %in% c(4,5,9,10,11)), 'szn'] <- 0

## read in 1 cropped scene to get background image and cell #s (rows, columns)
ls5.stack <- brick('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/LT05_L2SP_035038_20020622_20200905_02_T1_CROPPED.grd')
raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)

##### Get number of offspring recorded per mound per year and assign to cells in the raster #####
OUT <- NULL
for(y in unique(pop.dat$year)){
  sub <- pop.dat[pop.dat$year == y,] ## subset to year
  off <- sub[sub$offspring == 1,] ## get information for all offspring first recorded in that year
  off <- as.data.frame(table(off$terr)) ## get tally of offspring per mound
  off$year <- y
  OUT <- rbind(OUT, off)
}
off <- OUT
colnames(off) <- c('database.name','num.off','year')
temp <- mc.key[,c('database.name','cell.num')]
temp <- temp[!duplicated(temp),] ## only 188 entries, confirming mounds are always assigned to the same cell # across scenes
off <- merge(off, temp, by='database.name') ## combine mound/offspring information with cell IDs
off <- off[order(off$year),]

##### !!! have to figure out how to incorporate concept of lag year data #####
##### Check for relationships between environmental data for buffer zone around cells with offspring in a year
nc <- ncol(ls5.stack)
nr <- nrow(ls5.stack)
loop.tc <- tc.dat[,-1]
OUT <- NULL
for(y in unique(off$year)){
  temp <- off[off$year == y,] ## subset offspring data to year
  cs <- unique(temp$cell.num) ## get list of all cells of interest for that year
  adj <- c(cs-nc-1, cs-nc, cs-nc+1, cs-1, cs+1, cs+nc-1, cs+nc, cs+nc+1) ## expand list of cells to include cells adjacent to cells of interest (center + 8 cells surrounding)
  cs <- unique(c(cs, adj))
  r <- ls5.stack
  r[setdiff(seq_len(ncell(r)), cs)] <- NA
  r[!is.na(r)] <- 1
  raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)
    plot(rasterToPolygons(r, dissolve=TRUE), add=TRUE, border='red', lwd=2) ## plot outline of cells to be included in analyses
    legend('topleft', legend=y, bty='n', inset=c(0.15, 0.02), cex=1.5)
  tc.temp <- loop.tc[which(loop.tc$lag.year == y),] ## get lag year environmental data that corresponds to year preceeding offspring recorded for that year
  tc.temp <- tc.temp[which(tc.temp$cells %in% cs),]
  tc.temp <- tc.temp[!duplicated(tc.temp),]
  save <- c(y, mean(tc.temp$greenness, na.rm=TRUE), mean(tc.temp[tc.temp$szn == 0, 'greenness'], na.rm=TRUE), ## get average TC values across all cells within a year
            mean(tc.temp[tc.temp$szn == 1, 'greenness'], na.rm=TRUE), mean(tc.temp[tc.temp$szn == 2, 'greenness'], na.rm=TRUE),
            mean(tc.temp$brightness, na.rm=TRUE), mean(tc.temp[tc.temp$szn == 0, 'brightness'], na.rm=TRUE), 
            mean(tc.temp[tc.temp$szn == 1, 'brightness'], na.rm=TRUE), mean(tc.temp[tc.temp$szn == 2, 'brightness'], na.rm=TRUE),
            mean(tc.temp$wetness, na.rm=TRUE), mean(tc.temp[tc.temp$szn == 0, 'wetness'], na.rm=TRUE), 
            mean(tc.temp[tc.temp$szn == 1, 'wetness'], na.rm=TRUE), mean(tc.temp[tc.temp$szn == 2, 'wetness'], na.rm=TRUE))
  OUT <- rbind(OUT, save)
}
year.dat <- as.data.frame(OUT)
colnames(year.dat) <- c('year','mean.g','mean.offs.g','mean.mon.g','mean.win.g','mean.b','mean.offs.b','mean.mon.b','mean.win.b','mean.w','mean.offs.w','mean.mon.w','mean.win.w')

## get annual offspring totals
OUT <- NULL
for(i in unique(off$year)){
  temp <- off[off$year == i,]
  save <- c(i, sum(temp$num.off))
  OUT <- rbind(OUT, save)
}
off.tot <- as.data.frame(OUT)
colnames(off.tot) <- c('year','total.off')
year.dat <- merge(year.dat, off.tot, by='year')
year.dat <- year.dat[-c(17,18),]

## plot some relationships between TC mean values and year offspring totals
plot(year.dat$mean.g, year.dat$total.off, pch=19, col='springgreen4')
plot(year.dat$mean.offs.g, year.dat$total.off, pch=19, col='springgreen4')
plot(year.dat$mean.mon.g, year.dat$total.off, pch=19, col='springgreen4')
plot(year.dat$mean.win.g, year.dat$total.off, pch=19, col='springgreen4')
plot(year.dat$mean.b, year.dat$total.off, pch=19, col='tan4')
plot(year.dat$mean.offs.b, year.dat$total.off, pch=19, col='tan4')
plot(year.dat$mean.mon.b, year.dat$total.off, pch=19, col='tan4')
plot(year.dat$mean.win.b, year.dat$total.off, pch=19, col='tan4')
plot(year.dat$mean.w, year.dat$total.off, pch=19, col='dodgerblue')
plot(year.dat$mean.offs.w, year.dat$total.off, pch=19, col='dodgerblue')
plot(year.dat$mean.mon.w, year.dat$total.off, pch=19, col='dodgerblue')
plot(year.dat$mean.win.w, year.dat$total.off, pch=19, col='dodgerblue')
summary(lm(year.dat$total.off ~ year.dat$mean.win.w))

