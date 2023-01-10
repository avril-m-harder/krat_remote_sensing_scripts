

### COPIED IN FROM env_x_fitness.R -- NEEDS TO BE 100% REWORKED ###


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
n.col <- 'turquoise4'

##### Read in population (Peter's) data #####
pop.dat <- read.csv('/Users/Avril/Documents/krat_genetics/preseq_sample_information/KRATP.csv')
## lat/long headings reversed in original file
colnames(pop.dat)[7:8] <- c('long','lat')

##### Read in more population (Janna's) data #####
j.dat <- read.csv('/Users/Avril/Documents/krats/krat_data_and_paper2/summary_allstats2_withgen_zeroes.csv')
ages <- j.dat[,c('id','birthyear','deathyear')]
ages$age <- ages$deathyear - ages$birthyear
ages <- ages[,c(1,4)]

## read in 1 cropped scene to get background image and cell #s (rows, columns)
ls5.stack <- brick('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/LT05_L2SP_035038_20020622_20200905_02_T1_CROPPED.grd')
raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)

##### Read in manual mound:cell assignments (n=26) #####
### >> also needed to re-name unique mounds that were all labeled 'R2' in the original database 
man.ass <- read.csv('/Users/Avril/Documents/krat_remote_sensing/archive/sorting_out_mound_names/manual_mound_cell_assignments.csv')
## rename 'R2' mounds to match their actual locations (unique names R2.1-R2.6)
r2 <- man.ass[man.ass$terr=='R2',]
for(i in 1:nrow(r2)){
  pop.dat[which(pop.dat$lat == r2$lat[i] & pop.dat$long == r2$long[i]), 'terr'] <- r2$new.db.name[i]
}

##### Read in mound coordinates #####
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
man.locs <- as.data.frame(xyFromCell(ls5.stack, man.ass$cell))
man.locs$terr <- man.ass$terr
coordinates(man.locs) <- c('x','y') ## already in meters
proj4string(man.locs)<- CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
all.locs <- rbind(mnd.locs, man.locs)

## set extent for all analyses
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750
ext <- extent(lo.x, hi.x, lo.y, hi.y)

##### Read in TC and other index results #####
## set file names to be read in 
tc.fn <- 'tc_manual_cloudcheck' ## for TC data -- tc_manual_cloudcheck
mc.fn <- 'mnd_manual_cloudcheck' ## for mound/cell ID key -- mnd_manual_cloudcheck

## read in results of manual cloud checking to get list of scenes to process (results of manual cloud checking)
res <- read.csv('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/C2L2_low_cloud_scenes_cloud_cover_notes.csv')
table(res$usable) ## 365 / 460 scenes checked are usable (i.e., no clouds over extent or processing errors)
res <- res[which(res$usable == 1),]
files <- as.vector(res$filename)

tc.dat <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'))
## get rid of indices ID'ed as having correlation issues
tc.dat <- tc.dat[,-c(5:7)]
mc.key <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',mc.fn,'.csv'))

## scale and center TC + NDWI
tc.dat$z.g <- scale(tc.dat$greenness, scale=TRUE, center=TRUE)
tc.dat$z.b <- scale(tc.dat$brightness, scale=TRUE, center=TRUE)
tc.dat$z.w <- scale(tc.dat$wetness, scale=TRUE, center=TRUE)
tc.dat$z.n <- scale(tc.dat$NDWI, scale=TRUE, center=TRUE)

## get list of mound:cell associations
mounds.cells.only <- mc.key[,c('database.name','cell.num')]
mounds.cells.only <- mounds.cells.only[!duplicated(mounds.cells.only),]

##### Define seasons #####
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,1]
## define brood-year lag months (e.g., for indivs born in 2000, lag.year would be set to 2000 for July 1999-June 2000)
## i.e., abiotic data from the months of June - May are predicting survival/fitness for indivs presumably born between those months
tc.dat$year <- as.numeric(tc.dat$year)
tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,2]
tc.dat$month <- as.numeric(tc.dat$month)
tc.dat[which(tc.dat$month %in% c(7:12)), 'half.year.lag'] <- tc.dat[which(tc.dat$month %in% c(7:12)), 'year'] + 1
tc.dat[which(tc.dat$month %in% c(1:6)), 'half.year.lag'] <- tc.dat[which(tc.dat$month %in% c(1:6)), 'year']
tc.dat$one.year.lag <- tc.dat$year+1


### two seasonal options:
## (1) define seasons by quarters (1=spring, 2=summer, 3=fall, 4=winter)
tc.dat[which(tc.dat$month %in% c(1,2,3)), 'month.szn'] <- 1
tc.dat[which(tc.dat$month %in% c(4,5,6)), 'month.szn'] <- 2
tc.dat[which(tc.dat$month %in% c(7,8,9)), 'month.szn'] <- 3
tc.dat[which(tc.dat$month %in% c(10,11,12)), 'month.szn'] <- 4

## (2) define seasons by expected precip (1=July-August, 2=Dec-March, 0=outside these months)
tc.dat[which(tc.dat$month %in% c(7,8)), 'weather.szn'] <- 1
tc.dat[which(tc.dat$month %in% c(12,1,2,3)), 'weather.szn'] <- 2
tc.dat[which(tc.dat$month %in% c(4,5,6,9,10,11)), 'weather.szn'] <- 0

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

##### Get number of offspring recorded per mound per year and assign to cells in the raster #####
## >> relies on 'offspring' == 1 designation in Peter's data to assign offspring to the mound where 
## >> recorded; does not consider any pedigree information (e.g., where individual ID'ed as mom was sampled)
## 
## >> pop.dat              == Peter's data
## >> j.dat                == J's data
## >> tc.dat               == environmental data
## >> mounds.cells.only    == mound:cell assignments (1:1)

OUT <- NULL
for(y in unique(pop.dat$year)){
  sub <- pop.dat[pop.dat$year == y,]              ## subset to year
  off <- sub[sub$offspring == 1,]                 ## get information for all offspring first recorded in that year
  off.ages <- ages[which(ages$id %in% off$id),]   ## get lifespan of offspring
  off <- merge(off, off.ages, by='id')            ## add lifespan data to dataframe
  OUT <- rbind(OUT, off)
}
off <- OUT
off <- off[which(off$year <= 2005),] ## only keep offspring born in or before 2005
colnames(off)[6] <- 'database.name'
temp <- mc.key[,c('database.name','cell.num')]
temp <- temp[!duplicated(temp),] ## 214 entries, confirming mounds are always assigned to the same cell # across scenes
off <- merge(off, temp, by='database.name') ## combine mound/offspring information with cell IDs
off <- off[order(off$year),] ## order by year of birth

##### For each year and each cell, summarize mound and offspring information for later random sampling
## (# observed at that mound, # surviving > 0 years, average age)
OUT <- NULL
for(y in unique(off$year)){                ## for each year,
  sub <- off[off$year == y,]
  for(i in unique(sub$cell.num)){          ## and for each cell with offspring recorded in that year,
    temp <- sub[sub$cell.num == i,]
    for(j in unique(temp$database.name)){  ## and for each mound in that cell,
      temp1 <- temp[temp$database.name == j,]
      stopifnot(nrow(temp1) == length(unique(temp1$id))) ## make sure no IDs show up twice
      num.off <- nrow(temp1)                             ## number of offspring recorded at that mound in that year
      num.surv <- nrow(temp1[which(temp1$age > 0),])     ## number of offspring "" surviving to 1 year
      avg.age <- mean(temp1$age)                         ## average age at death for offspring recorded at that mound in that year
      save <- c(i, j, y, num.off, num.surv, avg.age)
      OUT <- rbind(OUT, save)
    }
  }
}
## mnd.offs contains summary data for each mound in each year with cell #s saved
mnd.offs <- as.data.frame(OUT, row.names = NA)
colnames(mnd.offs) <- c('cell.num','database.name','year','num.off','num.surv','avg.age')
mnd.offs$num.off <- as.numeric(mnd.offs$num.off)
mnd.offs$num.surv <- as.numeric(mnd.offs$num.surv)
mnd.offs$avg.age <- as.numeric(mnd.offs$avg.age)
## calculate how many cells had multiple mounds with offspring in each year
CT <- NULL
for(y in unique(mnd.offs$year)){
  sub <- mnd.offs[mnd.offs$year == y,]
  single.mnds <- length(unique(names(table(sub$cell.num)[which(table(sub$cell.num) == 1)])))
  mult.mnds <- length(unique(names(table(sub$cell.num)[which(table(sub$cell.num) > 1)])))
  save <- c(as.numeric(y), single.mnds, mult.mnds)
  CT <- rbind(CT, save)
}
sum(CT[,2]) ## number of cells where only 1 mound was recorded with offspring in exactly 1 year
sum(CT[,3]) ## number of cells where only 1 mound was recorded with offspring in at least 1 year (may be recorded for multiple years)

##### For each year and each cell, randomly select one mound with offspring recorded,
## add TC information for focal cell
sub.tc.dat <- tc.dat[,c(1:5,7,10,14:23)] ## subset TC information to just keep relevant bits:
## cell # / TC + NDWI / doy / path / year / month / 6-month lag year / 1-year lag year / month season / weather season