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

##### Read in mound:cell information #####
mc.fn <- 'mnd_manual_cloudcheck' ## for mound/cell ID key -- mnd_manual_cloudcheck
mc.key <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',mc.fn,'.csv'))
## get list of unique mound:cell associations
mounds.cells.only <- mc.key[,c('database.name','cell.num')]
mounds.cells.only <- mounds.cells.only[!duplicated(mounds.cells.only),]

##### Get number of offspring recorded per mound per year and assign to cells in the raster #####
## >> relies on 'offspring' == 1 designation in Peter's data to assign offspring to the mound where 
## >> recorded; does not consider any pedigree information (e.g., where individual ID'ed as mom was sampled)
## 
## >> pop.dat              == Peter's data
## >> j.dat                == J's data
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
      max.age <- max(temp1$age)
      save <- c(i, j, y, num.off, num.surv, avg.age, max.age)
      OUT <- rbind(OUT, save)
    }
  }
}
## mnd.offs contains summary data for each mound in each year with cell #s saved
mnd.offs <- as.data.frame(OUT, row.names = NA)
colnames(mnd.offs) <- c('cell.num','database.name','year','num.off','num.surv','avg.age','max.age')
mnd.offs$num.off <- as.numeric(mnd.offs$num.off)
mnd.offs$num.surv <- as.numeric(mnd.offs$num.surv)
mnd.offs$avg.age <- as.numeric(mnd.offs$avg.age)
mnd.offs$prop.surv <- mnd.offs$num.surv/mnd.offs$num.off
mnd.offs$max.age <- as.numeric(mnd.offs$max.age)

## add consideration of mounds that were occupied by an adult female but not by juveniles in each year
## (i.e., unproductive)
OUT <- NULL
for(y in unique(pop.dat$year)){
  sub <- pop.dat[pop.dat$year == y,]                                     ## subset to year
  adt <- sub[sub$offspring == 0 & sub$sex == 'Female' & sub$year == y,]  ## keep adult female records
  juv.mnds <- unique(mnd.offs[mnd.offs$year == y, 'database.name'])      ## get mounds with juvenile observations
  adt.only <- adt[adt$terr %notin% juv.mnds,]                            ## exclude these juvenile-occupied mounds
  colnames(adt.only)[6] <- 'database.name'
  adt.only <- merge(adt.only, mounds.cells.only, by='database.name')     ## add cell number information
  ## format to combine with juvenile-occupied mound information
  adt.only <- adt.only[,c('cell.num','database.name','year')]
  OUT <- rbind(OUT, adt.only)
}
OUT <- OUT[OUT$year <= 2005,]
OUT$num.off <- 0
OUT$num.surv <- 0
OUT$avg.age <- NA
OUT$max.age <- NA
OUT$prop.surv <- NA

## combine info for productive and non-productive mounds
fitness <- rbind(mnd.offs, OUT)
fitness <- fitness[order(fitness$year),]
hist(fitness$num.off)

write.csv(fitness, '../intermediate_data/fitness_variables.csv', row.names=FALSE)

# ## calculate how many cells had multiple mounds with offspring in each year
# CT <- NULL
# for(y in unique(mnd.offs$year)){
#   sub <- mnd.offs[mnd.offs$year == y,]
#   single.mnds <- length(unique(names(table(sub$cell.num)[which(table(sub$cell.num) == 1)])))
#   mult.mnds <- length(unique(names(table(sub$cell.num)[which(table(sub$cell.num) > 1)])))
#   save <- c(as.numeric(y), single.mnds, mult.mnds)
#   CT <- rbind(CT, save)
# }
# sum(CT[,2]) ## number of cells where only 1 mound was recorded with offspring in exactly 1 year
# sum(CT[,3]) ## number of cells where only 1 mound was recorded with offspring in at least 1 year (may be recorded for multiple years)