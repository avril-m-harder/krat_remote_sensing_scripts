library(sp)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(TeachingDemos)
library(Hmisc)
source('/Users/Avril/Documents/krat_remote_sensing/krat_remote_sensing_scripts/03b_c2l2readMeta.R')
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
man.ass <- read.csv('/Users/Avril/Documents/krat_remote_sensing/sorting_out_mound_names/2021_first_round/manual_mound_cell_assignments.csv')

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
man.ass <- read.csv('/Users/Avril/Documents/krat_remote_sensing/sorting_out_mound_names/2021_first_round/manual_mound_cell_assignments.csv')
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

tc.dat <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'))
mc.key <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',mc.fn,'.csv'))

## identify other indices not super correlated with TC indices
samps <- sample(c(1:nrow(tc.dat)), size = 1000, replace = FALSE)  ## sample 1000 random cells across all scenes
temp <- tc.dat[samps, c(2:27)]
res <- rcorr(as.matrix(temp))
round(res$P, 3) ## all of the other indices (besides TC and temp.k) are significantly correlated with greenness. proceeding with just TC and temp.k.

# ## scale and center TC
tc.dat$z.g <- scale(tc.dat$greenness, scale=TRUE, center=TRUE)
tc.dat$z.b <- scale(tc.dat$brightness, scale=TRUE, center=TRUE)
tc.dat$z.w <- scale(tc.dat$wetness, scale=TRUE, center=TRUE)

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

##### Viz #####

## visualize some results - occupied mounds only -- annual means
pdf('/Users/Avril/Desktop/annual_TC_means_vs_fitnesses.pdf', width=12, height=4)
par(mfrow=c(1,3))
i <- 1
while(i == 1){
plot(random.mnd.nolag$ann.mean.b,   random.mnd.nolag$num.off,    col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Number of offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.b,  random.mnd.6molag$num.off,   col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Number of offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.b,  random.mnd.1yrlag$num.off,   col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Number of offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.w,   random.mnd.nolag$num.off,    col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Number of offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.w,  random.mnd.6molag$num.off,   col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Number of offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.w,  random.mnd.1yrlag$num.off,   col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Number of offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.g,   random.mnd.nolag$num.off,    col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Number of offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.g,  random.mnd.6molag$num.off,   col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Number of offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.g,  random.mnd.1yrlag$num.off,   col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Number of offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.n,   random.mnd.nolag$num.off,    col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Number of offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.n,  random.mnd.6molag$num.off,   col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Number of offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.n,  random.mnd.1yrlag$num.off,   col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Number of offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.b,   random.mnd.nolag$num.surv,    col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Number of surviving offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.b,  random.mnd.6molag$num.surv,   col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Number of surviving offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.b,  random.mnd.1yrlag$num.surv,   col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Number of surviving offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.w,   random.mnd.nolag$num.surv,    col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Number of surviving offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.w,  random.mnd.6molag$num.surv,   col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Number of surviving offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.w,  random.mnd.1yrlag$num.surv,   col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Number of surviving offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.g,   random.mnd.nolag$num.surv,    col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Number of surviving offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.g,  random.mnd.6molag$num.surv,   col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Number of surviving offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.g,  random.mnd.1yrlag$num.surv,   col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Number of surviving offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.n,   random.mnd.nolag$num.surv,    col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Number of surviving offspring', main='No lag')
plot(random.mnd.6molag$ann.mean.n,  random.mnd.6molag$num.surv,   col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Number of surviving offspring', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.n,  random.mnd.1yrlag$num.surv,   col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Number of surviving offspring', main='1-year lag')

plot(random.mnd.nolag$ann.mean.b,   random.mnd.nolag$avg.age,    col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Average age at death', main='No lag')
plot(random.mnd.6molag$ann.mean.b,  random.mnd.6molag$avg.age,   col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Average age at death', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.b,  random.mnd.1yrlag$avg.age,   col=alpha(b.col, 0.4), pch=19, xlab='Mean annual brightness', ylab='Average age at death', main='1-year lag')

plot(random.mnd.nolag$ann.mean.w,   random.mnd.nolag$avg.age,    col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Average age at death', main='No lag')
plot(random.mnd.6molag$ann.mean.w,  random.mnd.6molag$avg.age,   col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Average age at death', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.w,  random.mnd.1yrlag$avg.age,   col=alpha(w.col, 0.4), pch=19, xlab='Mean annual wetness', ylab='Average age at death', main='1-year lag')

plot(random.mnd.nolag$ann.mean.g,   random.mnd.nolag$avg.age,    col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Average age at death', main='No lag')
plot(random.mnd.6molag$ann.mean.g,  random.mnd.6molag$avg.age,   col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Average age at death', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.g,  random.mnd.1yrlag$avg.age,   col=alpha(g.col, 0.4), pch=19, xlab='Mean annual greenness', ylab='Average age at death', main='1-year lag')

plot(random.mnd.nolag$ann.mean.n,   random.mnd.nolag$avg.age,    col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Average age at death', main='No lag')
plot(random.mnd.6molag$ann.mean.n,  random.mnd.6molag$avg.age,   col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Average age at death', main='6-month lag')
plot(random.mnd.1yrlag$ann.mean.n,  random.mnd.1yrlag$avg.age,   col=alpha(n.col, 0.4), pch=19, xlab='Mean annual NDWI', ylab='Average age at death', main='1-year lag')
i <- i+1
}
dev.off()

## visualize some results - occupied mounds only -- weather season 1 (summer rainy season) means
pdf('/Users/Avril/Desktop/summer_rainy_season_TC_means_vs_fitnesses.pdf', width=12, height=4)
par(mfrow=c(1,3))
i <- 1
while(i == 1){
  plot(random.mnd.nolag$weather.szn.1.b,   random.mnd.nolag$num.off,    col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.b,  random.mnd.6molag$num.off,   col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.b,  random.mnd.1yrlag$num.off,   col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.w,   random.mnd.nolag$num.off,    col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.w,  random.mnd.6molag$num.off,   col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.w,  random.mnd.1yrlag$num.off,   col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.g,   random.mnd.nolag$num.off,    col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.g,  random.mnd.6molag$num.off,   col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.g,  random.mnd.1yrlag$num.off,   col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.n,   random.mnd.nolag$num.off,    col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.n,  random.mnd.6molag$num.off,   col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.n,  random.mnd.1yrlag$num.off,   col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.b,   random.mnd.nolag$num.surv,    col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.b,  random.mnd.6molag$num.surv,   col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.b,  random.mnd.1yrlag$num.surv,   col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.w,   random.mnd.nolag$num.surv,    col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.w,  random.mnd.6molag$num.surv,   col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.w,  random.mnd.1yrlag$num.surv,   col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.g,   random.mnd.nolag$num.surv,    col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.g,  random.mnd.6molag$num.surv,   col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.g,  random.mnd.1yrlag$num.surv,   col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.n,   random.mnd.nolag$num.surv,    col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.n,  random.mnd.6molag$num.surv,   col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.n,  random.mnd.1yrlag$num.surv,   col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.b,   random.mnd.nolag$avg.age,    col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.b,  random.mnd.6molag$avg.age,   col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.b,  random.mnd.1yrlag$avg.age,   col=alpha(b.col, 0.4), pch=19, xlab='Mean summer rainy season brightness', ylab='Average age at death', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.w,   random.mnd.nolag$avg.age,    col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.w,  random.mnd.6molag$avg.age,   col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.w,  random.mnd.1yrlag$avg.age,   col=alpha(w.col, 0.4), pch=19, xlab='Mean summer rainy season wetness', ylab='Average age at death', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.g,   random.mnd.nolag$avg.age,    col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.g,  random.mnd.6molag$avg.age,   col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.g,  random.mnd.1yrlag$avg.age,   col=alpha(g.col, 0.4), pch=19, xlab='Mean summer rainy season greenness', ylab='Average age at death', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.1.n,   random.mnd.nolag$avg.age,    col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.1.n,  random.mnd.6molag$avg.age,   col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.1.n,  random.mnd.1yrlag$avg.age,   col=alpha(n.col, 0.4), pch=19, xlab='Mean summer rainy season NDWI', ylab='Average age at death', main='1-year lag')
  i <- i+1
}
dev.off()

## visualize some results - occupied mounds only -- weather season 2 (winter rainy season) means
pdf('/Users/Avril/Desktop/winter_rainy_season_TC_means_vs_fitnesses.pdf', width=12, height=4)
par(mfrow=c(1,3))
i <- 1
while(i == 1){
  plot(random.mnd.nolag$weather.szn.2.b,   random.mnd.nolag$num.off,    col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.b,  random.mnd.6molag$num.off,   col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.b,  random.mnd.1yrlag$num.off,   col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.w,   random.mnd.nolag$num.off,    col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.w,  random.mnd.6molag$num.off,   col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.w,  random.mnd.1yrlag$num.off,   col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.g,   random.mnd.nolag$num.off,    col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.g,  random.mnd.6molag$num.off,   col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.g,  random.mnd.1yrlag$num.off,   col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.n,   random.mnd.nolag$num.off,    col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Number of offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.n,  random.mnd.6molag$num.off,   col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Number of offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.n,  random.mnd.1yrlag$num.off,   col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Number of offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.b,   random.mnd.nolag$num.surv,    col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.b,  random.mnd.6molag$num.surv,   col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.b,  random.mnd.1yrlag$num.surv,   col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.w,   random.mnd.nolag$num.surv,    col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.w,  random.mnd.6molag$num.surv,   col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.w,  random.mnd.1yrlag$num.surv,   col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.g,   random.mnd.nolag$num.surv,    col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.g,  random.mnd.6molag$num.surv,   col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.g,  random.mnd.1yrlag$num.surv,   col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.n,   random.mnd.nolag$num.surv,    col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Number of surviving offspring', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.n,  random.mnd.6molag$num.surv,   col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Number of surviving offspring', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.n,  random.mnd.1yrlag$num.surv,   col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Number of surviving offspring', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.b,   random.mnd.nolag$avg.age,    col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.b,  random.mnd.6molag$avg.age,   col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.b,  random.mnd.1yrlag$avg.age,   col=alpha(b.col, 0.4), pch=19, xlab='Mean winter rainy season brightness', ylab='Average age at death', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.w,   random.mnd.nolag$avg.age,    col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.w,  random.mnd.6molag$avg.age,   col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.w,  random.mnd.1yrlag$avg.age,   col=alpha(w.col, 0.4), pch=19, xlab='Mean winter rainy season wetness', ylab='Average age at death', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.g,   random.mnd.nolag$avg.age,    col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.g,  random.mnd.6molag$avg.age,   col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.g,  random.mnd.1yrlag$avg.age,   col=alpha(g.col, 0.4), pch=19, xlab='Mean winter rainy season greenness', ylab='Average age at death', main='1-year lag')
  
  plot(random.mnd.nolag$weather.szn.2.n,   random.mnd.nolag$avg.age,    col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Average age at death', main='No lag')
  plot(random.mnd.6molag$weather.szn.2.n,  random.mnd.6molag$avg.age,   col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Average age at death', main='6-month lag')
  plot(random.mnd.1yrlag$weather.szn.2.n,  random.mnd.1yrlag$avg.age,   col=alpha(n.col, 0.4), pch=19, xlab='Mean winter rainy season NDWI', ylab='Average age at death', main='1-year lag')
  i <- i+1
}
dev.off()

#### value distributions for occupied, buffer unoccupied, and non-buffer
pdf('/Users/Avril/Desktop/values_distributions_for_cell_types.pdf', height=12, width=6)
par(mfrow=c(4,1), mar=c(2.6,2.6,1.6,1.1))
## across all years, cells
## brightness
plot(density(c(random.mnd.nolag$ann.mean.b, unocc.nolag$ann.mean.b, buffer.nolag$ann.mean.b)), col='transparent', main='All years',
     ylim=c(0,max(c(density(random.mnd.nolag$ann.mean.b)$y, density(unocc.nolag$ann.mean.b)$y, density(buffer.nolag$ann.mean.b)$y))), 
     xlim=c(min(c(random.mnd.nolag$ann.mean.b, buffer.nolag$ann.mean.b, unocc.nolag$ann.mean.b))-0.75,
            max(c(random.mnd.nolag$ann.mean.b, buffer.nolag$ann.mean.b, unocc.nolag$ann.mean.b))+0.75), xlab='z(Brightness)')
  lines(density(random.mnd.nolag$ann.mean.b), col=alpha(b.col, 1))
  polygon(density(random.mnd.nolag$ann.mean.b), col=alpha(b.col, 1), border='black')
  lines(density(buffer.nolag$ann.mean.b), col=alpha(b.col, 0.67))
  polygon(density(buffer.nolag$ann.mean.b), col=alpha(b.col, 0.67), border='black')
  lines(density(unocc.nolag$ann.mean.b), col=alpha(b.col, 0.33))
  polygon(density(unocc.nolag$ann.mean.b), col=alpha(b.col, 0.33), border='black')
  legend('topright', fill=c(b.col, alpha(b.col, 0.67), alpha(b.col, 0.33)), title='Brightness',
         legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)
## greenness
plot(density(c(random.mnd.nolag$ann.mean.g, unocc.nolag$ann.mean.g, buffer.nolag$ann.mean.g)), col='transparent', main='',
     ylim=c(0,max(c(density(random.mnd.nolag$ann.mean.g)$y, density(unocc.nolag$ann.mean.g)$y, density(buffer.nolag$ann.mean.g)$y))), 
     xlim=c(min(c(random.mnd.nolag$ann.mean.g, buffer.nolag$ann.mean.g, unocc.nolag$ann.mean.g))-0.75,
            max(c(random.mnd.nolag$ann.mean.g, buffer.nolag$ann.mean.g, unocc.nolag$ann.mean.g))+0.75), xlab='z(Greenness)')
  lines(density(random.mnd.nolag$ann.mean.g), col=alpha(g.col, 1))
  polygon(density(random.mnd.nolag$ann.mean.g), col=alpha(g.col, 1), border='black')
  lines(density(buffer.nolag$ann.mean.g), col=alpha(g.col, 0.67))
  polygon(density(buffer.nolag$ann.mean.g), col=alpha(g.col, 0.67), border='black')
  lines(density(unocc.nolag$ann.mean.g), col=alpha(g.col, 0.33))
  polygon(density(unocc.nolag$ann.mean.g), col=alpha(g.col, 0.33), border='black')
  legend('topright', fill=c(g.col, alpha(g.col, 0.67), alpha(g.col, 0.33)), title='Greenness',
         legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)
## wetness
plot(density(c(random.mnd.nolag$ann.mean.w, unocc.nolag$ann.mean.w, buffer.nolag$ann.mean.w)), col='transparent', main='',
     ylim=c(0,max(c(density(random.mnd.nolag$ann.mean.w)$y, density(unocc.nolag$ann.mean.w)$y, density(buffer.nolag$ann.mean.w)$y))), 
     xlim=c(min(c(random.mnd.nolag$ann.mean.w, buffer.nolag$ann.mean.w, unocc.nolag$ann.mean.w))-0.75,
            max(c(random.mnd.nolag$ann.mean.w, buffer.nolag$ann.mean.w, unocc.nolag$ann.mean.w))+0.75), xlab='z(Wetness)')
  lines(density(random.mnd.nolag$ann.mean.w), col=alpha(w.col, 1))
  polygon(density(random.mnd.nolag$ann.mean.w), col=alpha(w.col, 1), border='black')
  lines(density(buffer.nolag$ann.mean.w), col=alpha(w.col, 0.67))
  polygon(density(buffer.nolag$ann.mean.w), col=alpha(w.col, 0.67), border='black')
  lines(density(unocc.nolag$ann.mean.w), col=alpha(w.col, 0.33))
  polygon(density(unocc.nolag$ann.mean.w), col=alpha(w.col, 0.33), border='black')
  legend('topright', fill=c(w.col, alpha(w.col, 0.67), alpha(w.col, 0.33)), title='Wetness',
         legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)
## NDWI
plot(density(c(random.mnd.nolag$ann.mean.n, unocc.nolag$ann.mean.n, buffer.nolag$ann.mean.n)), col='transparent', main='',
     ylim=c(0,max(c(density(random.mnd.nolag$ann.mean.n)$y, density(unocc.nolag$ann.mean.n)$y, density(buffer.nolag$ann.mean.n)$y))), 
     xlim=c(min(c(random.mnd.nolag$ann.mean.n, buffer.nolag$ann.mean.n, unocc.nolag$ann.mean.n))-0.75,
            max(c(random.mnd.nolag$ann.mean.n, buffer.nolag$ann.mean.n, unocc.nolag$ann.mean.n))+0.75), xlab='z(NDWI)')
  lines(density(random.mnd.nolag$ann.mean.n), col=alpha(n.col, 1))
  polygon(density(random.mnd.nolag$ann.mean.n), col=alpha(n.col, 1), border='black')
  lines(density(buffer.nolag$ann.mean.n), col=alpha(n.col, 0.67))
  polygon(density(buffer.nolag$ann.mean.n), col=alpha(n.col, 0.67), border='black')
  lines(density(unocc.nolag$ann.mean.n), col=alpha(n.col, 0.33))
  polygon(density(unocc.nolag$ann.mean.n), col=alpha(n.col, 0.33), border='black')
  legend('topright', fill=c(n.col, alpha(n.col, 0.67), alpha(n.col, 0.33)), title='NDWI',
         legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)

## by year
for(y in unique(random.mnd.nolag$year)){
  mnds <- random.mnd.nolag[random.mnd.nolag$year == y,]
  unoc <- unocc.nolag[unocc.nolag$year == y,]
  buff <- buffer.nolag[buffer.nolag$year == y,]
  ## brightness
  plot(density(c(mnds$ann.mean.b, unoc$ann.mean.b, buff$ann.mean.b)), col='transparent', main=y,
       ylim=c(0,max(c(density(mnds$ann.mean.b)$y, density(unoc$ann.mean.b)$y, density(buff$ann.mean.b)$y))), 
       xlim=c(min(c(random.mnd.nolag$ann.mean.b, buffer.nolag$ann.mean.b, unocc.nolag$ann.mean.b))-0.75,
              max(c(random.mnd.nolag$ann.mean.b, buffer.nolag$ann.mean.b, unocc.nolag$ann.mean.b))+0.75), xlab='z(Brightness)')
    lines(density(mnds$ann.mean.b), col=alpha(b.col, 1))
    polygon(density(mnds$ann.mean.b), col=alpha(b.col, 1), border='black')
    lines(density(buff$ann.mean.b), col=alpha(b.col, 0.67))
    polygon(density(buff$ann.mean.b), col=alpha(b.col, 0.67), border='black')
    lines(density(unoc$ann.mean.b), col=alpha(b.col, 0.33))
    polygon(density(unoc$ann.mean.b), col=alpha(b.col, 0.33), border='black')
    legend('topright', fill=c(b.col, alpha(b.col, 0.67), alpha(b.col, 0.33)), title='Brightness',
           legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)
  ## greenness
  plot(density(c(mnds$ann.mean.g, unoc$ann.mean.g, buff$ann.mean.g)), col='transparent', main='',
       ylim=c(0,max(c(density(mnds$ann.mean.g)$y, density(unoc$ann.mean.g)$y, density(buff$ann.mean.g)$y))), 
       xlim=c(min(c(random.mnd.nolag$ann.mean.g, buffer.nolag$ann.mean.g, unocc.nolag$ann.mean.g))-0.75,
              max(c(random.mnd.nolag$ann.mean.g, buffer.nolag$ann.mean.g, unocc.nolag$ann.mean.g))+0.75), xlab='z(Greenness)')
    lines(density(mnds$ann.mean.g), col=alpha(g.col, 1))
    polygon(density(mnds$ann.mean.g), col=alpha(g.col, 1), border='black')
    lines(density(buff$ann.mean.g), col=alpha(g.col, 0.67))
    polygon(density(buff$ann.mean.g), col=alpha(g.col, 0.67), border='black')
    lines(density(unoc$ann.mean.g), col=alpha(g.col, 0.33))
    polygon(density(unoc$ann.mean.g), col=alpha(g.col, 0.33), border='black')
    legend('topright', fill=c(g.col, alpha(g.col, 0.67), alpha(g.col, 0.33)), title='Greenness',
           legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)
  ## wetness
  plot(density(c(mnds$ann.mean.w, unoc$ann.mean.w, buff$ann.mean.w)), col='transparent', main='',
       ylim=c(0,max(c(density(mnds$ann.mean.w)$y, density(unoc$ann.mean.w)$y, density(buff$ann.mean.w)$y))), 
       xlim=c(min(c(random.mnd.nolag$ann.mean.w, buffer.nolag$ann.mean.w, unocc.nolag$ann.mean.w))-0.75,
              max(c(random.mnd.nolag$ann.mean.w, buffer.nolag$ann.mean.w, unocc.nolag$ann.mean.w))+0.75), xlab='z(Wetness)')
    lines(density(mnds$ann.mean.w), col=alpha(w.col, 1))
    polygon(density(mnds$ann.mean.w), col=alpha(w.col, 1), border='black')
    lines(density(buff$ann.mean.w), col=alpha(w.col, 0.67))
    polygon(density(buff$ann.mean.w), col=alpha(w.col, 0.67), border='black')
    lines(density(unoc$ann.mean.w), col=alpha(w.col, 0.33))
    polygon(density(unoc$ann.mean.w), col=alpha(w.col, 0.33), border='black')
    legend('topright', fill=c(w.col, alpha(w.col, 0.67), alpha(w.col, 0.33)), title='Wetness',
           legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)
  ## NDWI
  plot(density(c(mnds$ann.mean.n, unoc$ann.mean.n, buff$ann.mean.n)), col='transparent', main='',
       ylim=c(0,max(c(density(mnds$ann.mean.n)$y, density(unoc$ann.mean.n)$y, density(buff$ann.mean.n)$y))), 
       xlim=c(min(c(random.mnd.nolag$ann.mean.n, buffer.nolag$ann.mean.n, unocc.nolag$ann.mean.n))-0.75,
              max(c(random.mnd.nolag$ann.mean.n, buffer.nolag$ann.mean.n, unocc.nolag$ann.mean.n))+0.75), xlab='z(NDWI)')
    lines(density(mnds$ann.mean.n), col=alpha(n.col, 1))
    polygon(density(mnds$ann.mean.n), col=alpha(n.col, 1), border='black')
    lines(density(buff$ann.mean.n), col=alpha(n.col, 0.67))
    polygon(density(buff$ann.mean.n), col=alpha(n.col, 0.67), border='black')
    lines(density(unoc$ann.mean.n), col=alpha(n.col, 0.33))
    polygon(density(unoc$ann.mean.n), col=alpha(n.col, 0.33), border='black')
    legend('topright', fill=c(n.col, alpha(n.col, 0.67), alpha(n.col, 0.33)), title='NDWI',
           legend=c('Mound cells','Buffer cells','Unoccupied cells'), bty='n', inset=0.02)
}
dev.off()

# ## Boxplots of independent variable values for each dependent variable -- not as helpful as scatterplots
# pdf('/Users/Avril/Desktop/boxplots.pdf', width=10, height=10)
# par(mfrow=c(2,2))
# for(y in unique(random.mnd.nolag$year)){     ## for each year
#   sub <- random.mnd.nolag[random.mnd.nolag$year == y,]
#   for(c in 4:5){                              ## and for each column of dependent variables
#     boxplot(sub$weather.szn.1.g ~ sub[,c], col=g.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - No lag'), ylab='Greenness') ## plot boxplots for each metric
#     boxplot(sub$weather.szn.1.b ~ sub[,c], col=b.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - No lag'), ylab='Brightness')
#     boxplot(sub$weather.szn.1.w ~ sub[,c], col=w.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - No lag'), ylab='Wetness')
#     boxplot(sub$weather.szn.1.n ~ sub[,c], col=n.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - No lag'), ylab='NDWI')
#   }
#   sub <- random.mnd.6molag[random.mnd.6molag$year == y,]
#   for(c in 4:5){
#     boxplot(sub$weather.szn.1.g ~ sub[,c], col=g.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 6-month lag'), ylab='Greenness')
#     boxplot(sub$weather.szn.1.b ~ sub[,c], col=b.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 6-month lag'), ylab='Brightness')
#     boxplot(sub$weather.szn.1.w ~ sub[,c], col=w.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 6-month lag'), ylab='Wetness')
#     boxplot(sub$weather.szn.1.n ~ sub[,c], col=n.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 6-month lag'), ylab='NDWI')
#   }
#   sub <- random.mnd.1yrlag[random.mnd.1yrlag$year == y,]
#   for(c in 4:5){
#     boxplot(sub$weather.szn.1.g ~ sub[,c], col=g.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 1-year lag'), ylab='Greenness')
#     boxplot(sub$weather.szn.1.b ~ sub[,c], col=b.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 1-year lag'), ylab='Brightness')
#     boxplot(sub$weather.szn.1.w ~ sub[,c], col=w.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 1-year lag'), ylab='Wetness')
#     boxplot(sub$weather.szn.1.n ~ sub[,c], col=n.col, pch=19, xlab=colnames(sub)[c], main=paste0(y,' - 1-year lag'), ylab='NDWI')
#   }
# }
# dev.off()


# ## Annual PCAs -- not super interesting
# for(y in unique(random.mnd.nolag$year)){
#   mnds <- random.mnd.nolag[random.mnd.nolag$year == y, c(1,8:11)]
#   mnds$type <- 'mnds'
#   unoc <- unocc.nolag[unocc.nolag$year == y, c(1,4:7)]
#   unoc$type <- 'unoc'
#   buff <- buffer.nolag[buffer.nolag$year == y, c(1, 4:7)]
#   buff$type <- 'buff'
#   all <- rbind(mnds, unoc, buff)
#   ## create groups for plotting colors
#   col.group <- c(rep('red', times=nrow(all[which(all$type == 'mnds'),])),
#                  rep('orchid3', times=nrow(all[which(all$type == 'buff'),])),
#                  rep('blue', times=nrow(all[which(all$type == 'unoc'),])))
#   pca <- prcomp(all[,c(2:5)], scale=TRUE, center=TRUE)
#   plot(pca$x[,1], pca$x[,2], pch=19, col=alpha(col.group, 0.8), xlab='PC1', ylab='PC2', main=y)
# }

