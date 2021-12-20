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