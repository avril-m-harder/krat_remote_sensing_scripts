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

##### Read in mound waypoints - I don't think I need these data? just mound:cell assignments #####
# mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
# mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
# mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
# coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
# proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84")
# mnd.locs <- sp::spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

##### Read in population (Peter's) data #####
pop.dat <- read.csv('/Users/Avril/Documents/krat_genetics/preseq_sample_information/KRATP.csv')
## lat/long headings reversed in original file
colnames(pop.dat)[7:8] <- c('long','lat')

##### Read in more population (Janna's) data #####
j.dat <- read.csv('/Users/Avril/Documents/krats/krat_data_and_paper2/summary_allstats2_withgen_zeroes.csv')
ages <- j.dat[,c('id','birthyear','deathyear')]
ages$age <- ages$deathyear - ages$birthyear
ages <- ages[,c(1,4)]

##### Read in manual mound:cell assignments (n=26) #####
### >> also needed to re-name unique mounds that were all labeled 'R2' in the original database 
man.ass <- read.csv('/Users/Avril/Documents/krat_remote_sensing/archive/sorting_out_mound_names/manual_mound_cell_assignments.csv')
## rename 'R2' mounds to match their actual locations (unique names R2.1-R2.6)
r2 <- man.ass[man.ass$terr=='R2',]
for(i in 1:nrow(r2)){
  pop.dat[which(pop.dat$lat == r2$lat[i] & pop.dat$long == r2$long[i]), 'terr'] <- r2$new.db.name[i]
}

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
mounds.cells.only <- mc.key[,c('database.name','cell.num')]
mounds.cells.only <- mounds.cells.only[!duplicated(mounds.cells.only),]

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
## (1) define seasons by quarters (1=spring, 2=summer, 3=fall, 4=winter)
tc.dat[which(tc.dat$month %in% c(1,2,3)), 'month.szn'] <- 1
tc.dat[which(tc.dat$month %in% c(4,5,6)), 'month.szn'] <- 2
tc.dat[which(tc.dat$month %in% c(7,8,9)), 'month.szn'] <- 3
tc.dat[which(tc.dat$month %in% c(10,11,12)), 'month.szn'] <- 4

## (2) define seasons by expected precip (1=June-August, 2=Dec-March, 0=outside these months)
tc.dat[which(tc.dat$month %in% c(6,7,8)), 'weather.szn'] <- 1
tc.dat[which(tc.dat$month %in% c(12,1,2,3)), 'weather.szn'] <- 2
tc.dat[which(tc.dat$month %in% c(4,5,9,10,11)), 'weather.szn'] <- 0

## examine variation in TC metrics across months and different seasonal options -- not sure that this was very helpful?
# pdf('/Users/Avril/Desktop/tc_metric_variation_across_intervals.pdf', width=6, height=15)
# par(mfrow=c(3,1))
# for(i in unique(tc.dat$year)){
#   temp <- tc.dat[tc.dat$year == i,]
#   boxplot(temp$greenness ~ temp$month, main=paste0(i,'\ngreenness'), xlab='Month', ylab='Greenness', pch=19, cex=0.5)
#   boxplot(temp$greenness ~ temp$month.szn, main=paste0(i,'\ngreenness'), xlab='Quarterly seasons', ylab='Greenness', pch=19, cex=0.5)
#   boxplot(temp$greenness ~ temp$weather.szn, main=paste0(i,'\ngreenness'), xlab='Weather-defined seasons', ylab='Greenness', pch=19, cex=0.5)
#   boxplot(temp$wetness ~ temp$month, main=paste0(i,'\nwetness'), xlab='Month', ylab='Wetness', pch=19, cex=0.5)
#   boxplot(temp$wetness ~ temp$month.szn, main=paste0(i,'\nwetness'), xlab='Quarterly seasons', ylab='Wetness', pch=19, cex=0.5)
#   boxplot(temp$wetness ~ temp$weather.szn, main=paste0(i,'\nwetness'), xlab='Weather-defined seasons', ylab='Wetness', pch=19, cex=0.5)
#   boxplot(temp$brightness ~ temp$month, main=paste0(i,'\nbrightness'), xlab='Month', ylab='Brightness', pch=19, cex=0.5)
#   boxplot(temp$brightness ~ temp$month.szn, main=paste0(i,'\nbrightness'), xlab='Quarterly seasons', ylab='Brightness', pch=19, cex=0.5)
#   boxplot(temp$brightness ~ temp$weather.szn, main=paste0(i,'\nbrightness'), xlab='Weather-defined seasons', ylab='Brightness', pch=19, cex=0.5)
# }
# dev.off()

## read in 1 cropped scene to get background image and cell #s (rows, columns)
ls5.stack <- brick('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/LT05_L2SP_035038_20020622_20200905_02_T1_CROPPED.grd')
raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)

##### Get number of offspring recorded per mound per year and assign to cells in the raster #####
## >> relies on 'offspring' == 1 designation to assign offspring to the mound where recorded;
## >> does not consider any pedigree information (e.g., where individual ID'ed as mom was sampled)
OUT <- NULL
for(y in unique(pop.dat$year)){
  sub <- pop.dat[pop.dat$year == y,] ## subset to year
  off <- sub[sub$offspring == 1,] ## get information for all offspring first recorded in that year
  off.ages <- ages[which(ages$id %in% off$id),]
  off <- merge(off, off.ages, by='id')
  OUT <- rbind(OUT, off)
}
off <- OUT
off <- off[which(off$year <= 2005),]
colnames(off)[6] <- 'database.name'
temp <- mc.key[,c('database.name','cell.num')]
temp <- temp[!duplicated(temp),] ## 214 entries, confirming mounds are always assigned to the same cell # across scenes
off <- merge(off, temp, by='database.name') ## combine mound/offspring information with cell IDs
# hist(off[which(off$database.name %notin% temp$database.name), 'year'], xlab='Year', main='Mounds unassigned to cells')
# unique(off[which(off$database.name %notin% temp$database.name), 'database.name'])
# unique(pop.dat[which(pop.dat$terr %notin% temp$database.name), 'terr'])
off <- off[order(off$year),]




#

# ##### !!! have to figure out how to incorporate concept of lag year data #####
# ##### Check for relationships between environmental data for buffer zone around cells with offspring in a year
# nc <- ncol(ls5.stack) ## number of columns in raster
# nr <- nrow(ls5.stack) ## number of rows in raster
# loop.tc <- tc.dat
# OUT <- NULL
# # pdf('/Users/Avril/Desktop/mounds_with_offspring_in_analysis.pdf', width=6, height=8)
# for(y in unique(off$year)){
#   temp <- off[off$year == y,] ## subset offspring data to year
#   cs <- unique(temp$cell.num) ## get list of all cells of interest for that year
#   adj <- c(cs-nc-1, cs-nc, cs-nc+1, cs-1, cs+1, cs+nc-1, cs+nc, cs+nc+1) ## expand list of cells to include cells adjacent to cells of interest (center + 8 cells surrounding)
#   cs <- unique(c(cs, adj)) ## get list of unique cell #s
#   r <- ls5.stack ## create buffer polygon
#   r[setdiff(seq_len(ncell(r)), cs)] <- NA
#   r[!is.na(r)] <- 1
#   raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)
#     plot(rasterToPolygons(r, dissolve=TRUE), add=TRUE, border='red', lwd=2) ## plot outline of cells to be included in analyses
#     legend('topleft', legend=y, bty='n', inset=c(0.15, 0.02), cex=1.5)
#   tc.temp <- loop.tc[which(loop.tc$lag.year == y),] ## get lag year environmental data that corresponds to year preceeding offspring recorded for that year
#   tc.temp <- tc.temp[which(tc.temp$cell.num %in% cs),]
#   tc.temp <- tc.temp[!duplicated(tc.temp),]
#   save <- c(y, mean(tc.temp$greenness, na.rm=TRUE), mean(tc.temp[tc.temp$weather.szn == 0, 'greenness'], na.rm=TRUE), ## get average TC values across all cells within a year
#             mean(tc.temp[tc.temp$weather.szn == 1, 'greenness'], na.rm=TRUE), mean(tc.temp[tc.temp$weather.szn == 2, 'greenness'], na.rm=TRUE),
#             mean(tc.temp$brightness, na.rm=TRUE), mean(tc.temp[tc.temp$weather.szn == 0, 'brightness'], na.rm=TRUE), 
#             mean(tc.temp[tc.temp$weather.szn == 1, 'brightness'], na.rm=TRUE), mean(tc.temp[tc.temp$weather.szn == 2, 'brightness'], na.rm=TRUE),
#             mean(tc.temp$wetness, na.rm=TRUE), mean(tc.temp[tc.temp$weather.szn == 0, 'wetness'], na.rm=TRUE), 
#             mean(tc.temp[tc.temp$weather.szn == 1, 'wetness'], na.rm=TRUE), mean(tc.temp[tc.temp$weather.szn == 2, 'wetness'], na.rm=TRUE))
#   OUT <- rbind(OUT, save)
# }
# # dev.off()
# year.dat <- as.data.frame(OUT)
# colnames(year.dat) <- c('year','mean.g','mean.offs.g','mean.mon.g','mean.win.g','mean.b','mean.offs.b','mean.mon.b','mean.win.b','mean.w','mean.offs.w','mean.mon.w','mean.win.w')
# 
# ## make sure that all mounds with new offspring are being accounted for in TC analysis -- looks good
# # pop.dat <- pop.dat[order(pop.dat$year),]
# # pdf('/Users/Avril/Desktop/new_offspring_in_KRATP.pdf', width=6, height=6.5)
# # for(i in unique(pop.dat$year)){
# #   temp <- pop.dat[which(pop.dat$year == i & pop.dat$offspring == 1),]
# #   plot(temp$long, temp$lat, pch=19, col=alpha('dodgerblue', 0.3), main=i, xlab='Longitude', ylab='Latitude',
# #        xlim=c(min(pop.dat$long)-10, max(pop.dat$long)+10), ylim=c(min(pop.dat$lat)-10, max(pop.dat$lat)+10))
# # }
# # dev.off()
# 
# ## get annual offspring totals
# OUT <- NULL
# for(i in unique(off$year)){
#   temp <- off[off$year == i,]
#   save <- c(i, sum(temp$num.off))
#   OUT <- rbind(OUT, save)
# }
# off.tot <- as.data.frame(OUT)
# colnames(off.tot) <- c('year','total.off')
# year.dat <- merge(year.dat, off.tot, by='year')
# 
# ## plot some relationships between TC mean values and year offspring totals
# ## no obvious relationships between TC metrics averaged over the landscape seasonally/annually and total # of offspring
# ## produced in the year at all mounds
# plot(year.dat$mean.g, year.dat$total.off, pch=19, col='springgreen4')
# plot(year.dat$mean.offs.g, year.dat$total.off, pch=19, col='springgreen4')
# plot(year.dat$mean.mon.g, year.dat$total.off, pch=19, col='springgreen4')
# plot(year.dat$mean.win.g, year.dat$total.off, pch=19, col='springgreen4')
# plot(year.dat$mean.b, year.dat$total.off, pch=19, col='tan4')
# plot(year.dat$mean.offs.b, year.dat$total.off, pch=19, col='tan4')
# plot(year.dat$mean.mon.b, year.dat$total.off, pch=19, col='tan4')
# plot(year.dat$mean.win.b, year.dat$total.off, pch=19, col='tan4')
# plot(year.dat$mean.w, year.dat$total.off, pch=19, col='dodgerblue')
# plot(year.dat$mean.offs.w, year.dat$total.off, pch=19, col='dodgerblue')
# plot(year.dat$mean.mon.w, year.dat$total.off, pch=19, col='dodgerblue')
# plot(year.dat$mean.win.w, year.dat$total.off, pch=19, col='dodgerblue')
# summary(lm(year.dat$total.off ~ year.dat$mean.win.w))
# 
# ## try zooming in to more local relationships:
# ## (1) annual and seasonal mean TC metrics within 1 cell : # offspring produced in that cell
# ## (2) annual and seasonal mean TC metrics for 1 cell + adjacent cells : # offspring produced in the focal cell
# ## collapse offspring count info by cell # only (get rid of mound info)
# ##### !!! also need to do something with TC values for cells with mounds that produced no offspring. idk how !!! #####
# OUT <- NULL
# for(i in unique(off$year)){
#   temp <- off[off$year == i,]
#   for(j in unique(temp$cell.num)){
#     save <- c(i, j, sum(temp[which(temp$cell.num == j), 'num.off']))
#     OUT <- rbind(OUT, save)
#   }
# }
# cell.offs <- as.data.frame(OUT)
# colnames(cell.offs) <- c('year','cell.num','num.off')
# cell.offs <- merge(cell.offs, tc.dat, by=c('year','cell.num'))
# OUT <- NULL
# for(i in unique(cell.offs$year)){
#   temp <- cell.offs[cell.offs$year == i,]
#   for(j in unique(temp$cell.num)){
#     sub <- temp[temp$cell.num == j,]
#     ## save max and min TC metric values for each cell with offspring in each year
#     save <- c(i, j, sub$num.off[1], min(sub$greenness), max(sub$greenness), 
#               min(sub$wetness), max(sub$wetness),
#               min(sub$brightness), max(sub$brightness))
#     OUT <- rbind(OUT, save)
#   }
# }
# tc.cell.offs <- as.data.frame(OUT)
# colnames(tc.cell.offs) <- c('year','cell.num','num.off','min.g','max.g','min.w','max.w','min.b','max.b')
# 
# plot(tc.cell.offs$min.g, tc.cell.offs$num.off, pch=19, col=alpha('darkgreen', 0.4))
# plot(tc.cell.offs$max.g, tc.cell.offs$num.off, pch=19, col=alpha('darkgreen', 0.4))
# plot(tc.cell.offs$min.w, tc.cell.offs$num.off, pch=19, col=alpha('blue', 0.4))
# plot(tc.cell.offs$max.w, tc.cell.offs$num.off, pch=19, col=alpha('blue', 0.4))
# plot(tc.cell.offs$min.b, tc.cell.offs$num.off, pch=19, col=alpha('sienna', 0.4))
# plot(tc.cell.offs$max.b, tc.cell.offs$num.off, pch=19, col=alpha('sienna', 0.4))