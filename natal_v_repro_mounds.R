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


##### 1. Read in data #####
##### 1A. Read in population (Peter's) data #####
pop.dat <- read.csv('/Users/Avril/Documents/krat_genetics/preseq_sample_information/KRATP.csv')
## lat/long headings reversed in original file
colnames(pop.dat)[7:8] <- c('long','lat')

##### 1B. Read in more population (Janna's) data #####
j.dat <- read.csv('/Users/Avril/Documents/krats/krat_data_and_paper2/summary_allstats2_withgen_zeroes.csv')
ages <- j.dat[,c('id','birthyear','deathyear')]
ages$age <- ages$deathyear - ages$birthyear
ages <- ages[,c(1,4)]

## read in 1 cropped scene to get background image and cell #s (rows, columns)
ls5.stack <- brick('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/LT05_L2SP_035038_20020622_20200905_02_T1_CROPPED.grd')
raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)

##### 1C. Read in manual mound:cell assignments (n=26) #####
### >> also needed to re-name unique mounds that were all labeled 'R2' in the original database 
man.ass <- read.csv('/Users/Avril/Documents/krat_remote_sensing/archive/sorting_out_mound_names/manual_mound_cell_assignments.csv')
## rename 'R2' mounds to match their actual locations (unique names R2.1-R2.6)
r2 <- man.ass[man.ass$terr=='R2',]
for(i in 1:nrow(r2)){
  pop.dat[which(pop.dat$lat == r2$lat[i] & pop.dat$long == r2$long[i]), 'terr'] <- r2$new.db.name[i]
}

##### 1D. Read in mound coordinates #####
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

##### 1E. Read in mound:cell key #####
mc.key <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_mnd_manual_cloudcheck.csv'))
mc.key <- mc.key[,c('database.name','cell.num')]
mc.key <- mc.key[!duplicated(mc.key),]
colnames(mc.key)[1] <- 'terr'

## set extent for all analyses
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750
ext <- extent(lo.x, hi.x, lo.y, hi.y)


##### 2. Adult location:reproduction #####
## For each adult individual, record mound location and fitness measures for
## each year of that individual's life. Also record juvenile mound location.
## Assumptions:
#### (1) Juvenile (offspring==1) location is natal mound.
#### (2) Adult (offspring==0) location(s) are reproductive mounds for their
####      respective years.
### Requires that juvenile mound != adult mounds.
## ** Not sure that this is any better than my original approach, just based
## ** on assigning individual to mounds as juveniles and nothing else.
OUT <- NULL
for(i in unique(pop.dat$id)){
  sub <- pop.dat[pop.dat$id == i,]                     ## subset to individual,
  sub <- sub[,c(2,3,6,11)]                             ## keep ID, lifestage, mound, year,
  sub$sex <- j.dat[j.dat$id == i, 'sex']               ## add sex information,
  juv.mnd <- unique(sub[sub$offspring == 1, 'terr'])   ## get list of juvenile mound(s),
  adt.mnd <- unique(sub[sub$offspring != 1, 'terr'])   ## get list of adult mound(s).
  if(length(unique(c(juv.mnd, adt.mnd))) > 1 &         ## if there are at least 2 unique mounds (juv + adult),
     length(juv.mnd) > 0 & length(adt.mnd) > 0 &       ## and >=1 in each category,
     all(juv.mnd %in% mc.key$terr) & all(adt.mnd %in% mc.key$terr)){     ## and both are in the mound:cell key,
    offs <- j.dat[which(j.dat$momid == i | j.dat$dadid == i),]           ## get pedigree-ID'ed offspring information.
    if(nrow(offs) > 0){
      for(y in unique(sub$year)){                                                      ## for each year with data for the focal individual,
        sub[sub$year == y, 'num.offs'] <- nrow(offs[offs$birthyear == y,])             ## record number of offspring 'born' in that year,
        sub[sub$year == y, 'num.surv.offs'] <- nrow(offs[which(offs$birthyear == y &   ## and number of surviving offspring born in that year.
                                                    (offs$deathyear - offs$birthyear) >= 1),])
      }
      OUT <- rbind(OUT, sub)
    }
  }
}

fem.juv <- OUT[OUT$sex == 'Female' & OUT$offspring == 1,]  ## data for mounds where females were first observed
fem.adt <- OUT[OUT$sex == 'Female' & OUT$offspring == 0,]  ## data for mounds where females were located after the first observation
mal.juv <- OUT[OUT$sex == 'Male' & OUT$offspring == 1,]    ## data for mounds where males were first observed
mal.adt <- OUT[OUT$sex == 'Male' & OUT$offspring == 0,]    ## data for mounds where males were located after the first observation

## add cell numbers to data
fem.juv <- merge(fem.juv, mc.key, by='terr')
fem.adt <- merge(fem.adt, mc.key, by='terr')
mal.juv <- merge(mal.juv, mc.key, by='terr')
mal.adt <- merge(mal.adt, mc.key, by='terr')

## add lifetime offspring numbers to data (totals in j.dat inaccurate? see id == 916 for an example;
## probably because one of the offspring did not have a dad id'ed)
for(i in unique(fem.juv$id)){
  sub <- fem.adt[fem.adt$id == i,]
  fem.juv[fem.juv$id == i, 'lifetime.off'] <- sum(sub$num.offs)
  fem.juv[fem.juv$id == i, 'lifetime.off.surv'] <- sum(sub$num.surv.offs)
  fem.adt[fem.adt$id == i, 'lifetime.off'] <- sum(sub$num.offs)
  fem.adt[fem.adt$id == i, 'lifetime.off.surv'] <- sum(sub$num.surv.offs)
}
for(i in unique(mal.juv$id)){
  sub <- mal.adt[mal.adt$id == i,]
  mal.juv[mal.juv$id == i, 'lifetime.off'] <- sum(sub$num.offs)
  mal.juv[mal.juv$id == i, 'lifetime.off.surv'] <- sum(sub$num.surv.offs)
  mal.adt[mal.adt$id == i, 'lifetime.off'] <- sum(sub$num.offs)
  mal.adt[mal.adt$id == i, 'lifetime.off.surv'] <- sum(sub$num.surv.offs)
}

all.fit.data <- rbind(mal.adt, fem.adt, mal.juv, fem.juv)

# ### write these to files
# write.csv(fem.juv, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/fem_juv_occ_and_fitness.csv', row.names=FALSE)
# write.csv(fem.adt, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/fem_adt_occ_and_fitness.csv', row.names=FALSE)
# write.csv(mal.juv, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mal_juv_occ_and_fitness.csv', row.names=FALSE)
# write.csv(mal.adt, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mal_adt_occ_and_fitness.csv', row.names=FALSE)
  
##### 3. Check for relationships between natal and adult mound locations and fitness measures #####
##### 3A. Read in environmental data #####
## read in remote sensing predictor variables
tc.fn <- 'tc_manual_cloudcheck' ## for TC data -- tc_manual_cloudcheck
tc.dat <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'))
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,1]
tc.dat$year <- as.numeric(tc.dat$year)
tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,2]
tc.dat$month <- as.numeric(tc.dat$month)
## create month index for easier identification of lag periods
tc.dat$month.index <- ((tc.dat$year-1) * 12) + tc.dat$month

## get index names
inds <- c(2:(grep('acq.date', colnames(tc.dat))-1)) ## get column numbers for remote sensing index data
## NA values for EVI2 and TVI, so get rid of those
inds <- inds[inds %notin% c(which(colnames(tc.dat) == 'EVI2'), which(colnames(tc.dat) == 'TVI'))]
## remove EVI2 and TVI from data set
tc.dat <- tc.dat[,-c(which(colnames(tc.dat) == 'EVI2'), which(colnames(tc.dat) == 'TVI'))]
## get column indices for final set of remote sensing data
inds <- c(2:(grep('acq.date', colnames(tc.dat))-1))


##### 3B. Calculate summary statistics for occupied cells #####
# ## get list of cells from fitness data
# cells <- all.fit.data[,c('year','terr')]
# cells <- cells[!duplicated(cells),]
# cells <- merge(cells, mc.key, by='terr')
# cells <- cells[order(cells$year),]
# 
# OUT <- NULL
# for(r in 1:nrow(cells)){          ## for each cell we need stats for,
#   print(r/nrow(cells))
#   y <- cells$year[r]              ## set the year,
#   c <- cells$cell.num[r]          ## set the cell,
#   save <- c(y, c)
#   ## collect data for 4 6-month time intervals covering years t-1 and t
#   i1 <- (y*12)-23                 ## set first month of first 6-month interval (Jan-June, year t-1),
#   i2 <- (y*12)-17                 ## set first month of second 6-month interval (July-Dec, year t-1),
#   i3 <- (y*12)-11                 ## set first month of third 6-month interval (Jan-June, year t),
#   i4 <- (y*12)-5                  ## set first month of fourth 6-month interval (July-Dec, year t).
#   for(i in c(i1, i2, i3, i4)){    ## for each 6-month interval,
#     sub <- tc.dat[which(tc.dat$month.index >= i & tc.dat$month.index <= i+5 & tc.dat$cell.num == c), ]
#     # sub <- sub[!is.na(sub),]
#     if(nrow(sub) > 0){              ## if there's at least 1 observation in that interval,
#       n.obs <- nrow(sub)            ## get number of remote sensing observations,
#       save <- c(save, n.obs)        ## and save.
#       for(col in inds){          ## for each remote sensing measure,
#         temp <- sub[,col]           ## get all values,
#         avg <- mean(temp)           ## calculate mean,
#         med <- median(temp)         ## and median.
#         save <- c(save, avg, med)   ## save the information.
#       }
#     }else{
#       save <- c(save, 0, rep(NA, length(inds)*2))  ## if 0 observations, add information to save.
#     }
#   }
#   ## collect data for weather-based seasons (summer and winter rainy) in years t-1 and t
#   s1 <- c(((y*12)-17), ((y*12)-16))  ## summer rainy, July-Aug year t-1
#   s2 <- c(((y*12)-24), ((y*12)-21))  ## winter rainy, Dec-March year t-1
#   s3 <- c(((y*12)-5), ((y*12)-4))    ## summer rainy, July-Aug year t
#   s4 <- c(((y*12)-12), ((y*12)-9))   ## winter rainy, Dec-March year t
#   szns <- rbind(s1, s2, s3, s4)
#   save1 <- NULL
#   for(s in 1:4){    ## for each of the four weather-defined seasons,
#     sub <- tc.dat[which(tc.dat$month.index >= szns[s,1] & tc.dat$month.index <= szns[s,2] & tc.dat$cell.num == c),]
#     # sub <- sub[!is.na(sub),]
#     if(nrow(sub) > 0){              ## if there's at least 1 observation in that interval,
#       n.obs <- nrow(sub)            ## get number of remote sensing observations,
#       save1 <- c(save1, n.obs)        ## and save.
#       for(col in inds){          ## for each remote sensing measure,
#         temp <- sub[,col]           ## get all values,
#         avg <- mean(temp)           ## calculate mean,
#         med <- median(temp)         ## and median.
#         save1 <- c(save1, avg, med)   ## save the information.
#       }
#     }else{
#       save1 <- c(save1, 0, rep(NA, length(inds)*2))  ## if 0 observations, add information to save.
#     }
#   }
#   save <- c(save, save1)
#   OUT <- rbind(OUT, save)
#   if(r %% 10 == 0){
#     write.table(OUT, file=paste0('../intermediate_data/cells_w_adts_juvs_sumstats.csv'), quote=FALSE, append=TRUE,
#                 row.names=FALSE, sep=',', col.names=!file.exists(paste0('../intermediate_data/cells_w_adts_juvs_sumstats.csv')))
#     OUT <- NULL
#   }
# }
# write.table(OUT, file=paste0('../intermediate_data/cells_w_adts_juvs_sumstats.csv'), quote=FALSE, append=TRUE,
#             row.names=FALSE, sep=',', col.names=!file.exists(paste0('../intermediate_data/cells_w_adts_juvs_sumstats.csv')))

## should be 314 columns

##### 3C. Apply filters to data #####
cell.dat <- read.csv('../intermediate_data/cells_w_adts_juvs_sumstats.csv')

## create a vector of column names
indices <- colnames(tc.dat)[inds]
intervs <- c('month.1','month.2','month.3','month.4','weather.1','weather.2','weather.3','weather.4')
colnames(cell.dat)[c(1,2)] <- c('year','cell.num')
c <- 3
while(c <= ncol(cell.dat)){
  for(int in intervs){
    colnames(cell.dat)[c] <- paste0(int,'.n.obs')
    c <- c+1
    for(ind in indices){
      colnames(cell.dat)[c] <- paste0(int,'.',ind,'.mean')
      c <- c+1
      colnames(cell.dat)[c] <- paste0(int,'.',ind,'.median')
      c <- c+1
    }
  }
}

## separate into mean and median data
mean.cols <- c(1,2,grep('n.obs', colnames(cell.dat)), grep('mean', colnames(cell.dat)))
mean.cols <- mean.cols[order(mean.cols)]
mean.data <- cell.dat[,c(mean.cols)]
median.cols <- c(1,2,grep('n.obs', colnames(cell.dat)), grep('median', colnames(cell.dat)))
median.cols <- median.cols[order(median.cols)]
median.data <- cell.dat[,c(median.cols)]

## combine remote sensing and fitness data
all.dat <- merge(all.fit.data, mean.data, by=c('cell.num','year'))
## plot correlations for each time interval separately
for(i in intervs){
  cols <- grep(i, colnames(all.dat))                   ## get column indices for specific interval data
  temp <- all.dat[,c(1:10,cols[2:length(cols)])]       ## save fitness + interval data
  
  ## adult mounds - same-year stats (i.e., only evaluating fitness in year t)
  pdf(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/all_variable_correlations_by_interval/all_years_repro_mounds_',i,'_cors.pdf'),
      width=30, height=30)
  pairs(temp[temp$offspring == 0,c(7,8,11:ncol(temp))], pch=19, cex=0.5, col=alpha('blue', 0.4))
  dev.off()
  cor(temp[,c(7,8,11:ncol(temp))])
  
  ## natal mounds - weather at first recorded location vs. lifetime fitness
  pdf(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/all_variable_correlations_by_interval/all_years_natal_mounds_',i,'_cors.pdf'),
      width=30, height=30)
  pairs(temp[temp$offspring == 1,c(9:ncol(temp))], pch=19, cex=0.5, col=alpha('blue', 0.4))
  dev.off()
  cor(temp[,c(9:ncol(temp))])
}

#













##### 7. How far do individuals go from their natal --> repro mounds? #####
juv <- rbind(fem.juv, mal.juv)
adt <- rbind(fem.adt, mal.adt)
OUT <- NULL
OUT1 <- NULL
for(i in unique(juv$id)){
  sub.adt <- adt[adt$id == i,]
  mds <- unique(c(sub.adt$terr, juv[juv$id==i, 'terr']))
  dists <- spDists(all.locs[all.locs$terr %in% mds,])     ## calculate distances between all mounds where individual was observed
  dists <- unique(c(dists))                               ## only keep the unique values
  dists <- dists[dists!=0]                                ## get rid of diagonal values
  save <- cbind(rep(i, length(dists)), dists)             ## save the raw values
  save1 <- c(i, juv[juv$id == i, 'sex'], mean(dists), max(dists))                  ## save some basic summary stats
  OUT <- rbind(OUT, save)
  OUT1 <- rbind(OUT1, save1)
}

move.stats <- as.data.frame(OUT1)
colnames(move.stats) <- c('id','sex','mean.dist','max.dist')
move.stats$mean.dist <- as.numeric(move.stats$mean.dist)
move.stats$max.dist <- as.numeric(move.stats$max.dist)
hist(move.stats$mean.dist)
hist(move.stats$max.dist)

## 2 individuals moved really far?
## > 900m
i <- 4982  ## female
i <- 5036  ## male
raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)
  mds <- unique(c(adt[adt$id==i, 'terr'], juv[juv$id==i, 'terr']))
  pts <- all.locs[all.locs$terr %in% mds,]
  points(pts, pch=19, col='dodgerblue', cex=0.7)
## > 500m & < 900m
for(i in move.stats[move.stats$max.dist > 500 & move.stats$max.dist < 1000, 'id']){
  raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)
  mds <- unique(c(adt[adt$id==i, 'terr'], juv[juv$id==i, 'terr']))
  pts <- all.locs[all.locs$terr %in% mds,]
  points(pts, pch=19, col='dodgerblue', cex=0.7) 
}

### any relationship between dispersal distance and fitness? (no)
temp <- merge(juv, move.stats, by=c('id','sex'))
plot(temp$max.dist, temp$lifetime.off.surv)
