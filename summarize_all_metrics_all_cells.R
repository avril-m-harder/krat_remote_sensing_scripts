##### !! Add in consideration of all dependent variables and plot correlations #####

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

##### Read in TC and other index results #####
## set file names to be read in 
tc.fn <- 'tc_manual_cloudcheck' ## for TC data -- tc_manual_cloudcheck
tc.dat <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'))
## get rid of indices ID'ed as having correlation issues
tc.dat <- tc.dat[,-c(5:7)]

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


##### Set up for 3 different lags and summarize data for all cells and all years #####
OUT1 <- NULL ## no lag
OUT2 <- NULL ## 6-month lag
OUT3 <- NULL ## 1-year lag

years <- unique(tc.dat$year)
cells <- unique(tc.dat$cell.num)
tot <- length(years)*length(cells) ## total number of year*cell combinations = 109,480
n <- 1

for(y in years){
  for(c in cells){
    print(n/tot)
    save <- tc.dat[tc.dat$cell.num == c & tc.dat$year == y, c(1,14)][1,]
    save.1 <- save
    save.2 <- save
    save.3 <- save
    ## save relevant environmental information for each lag approach
    tc.temp.nolag <- tc.dat[which(tc.dat$cell.num == c & tc.dat$year == y),]
    tc.temp.6molag <- tc.dat[which(tc.dat$cell.num == c & tc.dat$half.year.lag == y),]
    tc.temp.1yearlag <- tc.dat[which(tc.dat$cell.num == c & tc.dat$one.year.lag == y),]

    ## No lag
    ## save annual information (number of scenes/observations, annual averages)
    save.1$num.obs <- nrow(tc.temp.nolag)
    save.1$ann.mean.g <- mean(tc.temp.nolag$greenness, na.rm=TRUE)
    save.1$ann.mean.w <- mean(tc.temp.nolag$wetness, na.rm=TRUE)
    save.1$ann.mean.b <- mean(tc.temp.nolag$brightness, na.rm=TRUE)
    save.1$ann.mean.n <- mean(tc.temp.nolag$NDWI, na.rm=TRUE)
    save.1$ann.sd.g <- sd(tc.temp.nolag$greenness, na.rm=TRUE)
    save.1$ann.sd.w <- sd(tc.temp.nolag$wetness, na.rm=TRUE)
    save.1$ann.sd.b <- sd(tc.temp.nolag$brightness, na.rm=TRUE)
    save.1$ann.sd.n <- sd(tc.temp.nolag$NDWI, na.rm=TRUE)
    save.1$ann.max.g <- max(tc.temp.nolag$greenness, na.rm=TRUE)
    save.1$ann.max.w <- max(tc.temp.nolag$wetness, na.rm=TRUE)
    save.1$ann.max.b <- max(tc.temp.nolag$brightness, na.rm=TRUE)
    save.1$ann.max.n <- max(tc.temp.nolag$NDWI, na.rm=TRUE)
    save.1$ann.min.g <- min(tc.temp.nolag$greenness, na.rm=TRUE)
    save.1$ann.min.w <- min(tc.temp.nolag$wetness, na.rm=TRUE)
    save.1$ann.min.b <- min(tc.temp.nolag$brightness, na.rm=TRUE)
    save.1$ann.min.n <- min(tc.temp.nolag$NDWI, na.rm=TRUE)
    ## save month-based season data for each of 4 seasons
    save.1$month.szn.1.obs <- nrow(tc.temp.nolag[which(tc.temp.nolag$month.szn == 1),])
    save.1$month.szn.1.g <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 1), 'greenness'], na.rm=TRUE)
    save.1$month.szn.1.w <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 1), 'wetness'], na.rm=TRUE)
    save.1$month.szn.1.b <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 1), 'brightness'], na.rm=TRUE)
    save.1$month.szn.1.n <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 1), 'NDWI'], na.rm=TRUE)
    save.1$month.szn.2.obs <- nrow(tc.temp.nolag[which(tc.temp.nolag$month.szn == 2),])
    save.1$month.szn.2.g <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 2), 'greenness'], na.rm=TRUE)
    save.1$month.szn.2.w <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 2), 'wetness'], na.rm=TRUE)
    save.1$month.szn.2.b <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 2), 'brightness'], na.rm=TRUE)
    save.1$month.szn.2.n <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 2), 'NDWI'], na.rm=TRUE)
    save.1$month.szn.3.obs <- nrow(tc.temp.nolag[which(tc.temp.nolag$month.szn == 3),])
    save.1$month.szn.3.g <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 3), 'greenness'], na.rm=TRUE)
    save.1$month.szn.3.w <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 3), 'wetness'], na.rm=TRUE)
    save.1$month.szn.3.b <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 3), 'brightness'], na.rm=TRUE)
    save.1$month.szn.3.n <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 3), 'NDWI'], na.rm=TRUE)
    save.1$month.szn.4.obs <- nrow(tc.temp.nolag[which(tc.temp.nolag$month.szn == 4),])
    save.1$month.szn.4.g <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 4), 'greenness'], na.rm=TRUE)
    save.1$month.szn.4.w <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 4), 'wetness'], na.rm=TRUE)
    save.1$month.szn.4.b <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 4), 'brightness'], na.rm=TRUE)
    save.1$month.szn.4.n <- mean(tc.temp.nolag[which(tc.temp.nolag$month.szn == 4), 'NDWI'], na.rm=TRUE)
    ## save weather-based season data for each of 3 seasons
    save.1$weather.szn.0.obs <- nrow(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 0),])
    save.1$weather.szn.0.g <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 0), 'greenness'], na.rm=TRUE)
    save.1$weather.szn.0.w <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 0), 'wetness'], na.rm=TRUE)
    save.1$weather.szn.0.b <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 0), 'brightness'], na.rm=TRUE)
    save.1$weather.szn.0.n <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 0), 'NDWI'], na.rm=TRUE)
    save.1$weather.szn.1.obs <- nrow(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 1),])
    save.1$weather.szn.1.g <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 1), 'greenness'], na.rm=TRUE)
    save.1$weather.szn.1.w <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 1), 'wetness'], na.rm=TRUE)
    save.1$weather.szn.1.b <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 1), 'brightness'], na.rm=TRUE)
    save.1$weather.szn.1.n <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 1), 'NDWI'], na.rm=TRUE)
    save.1$weather.szn.2.obs <- nrow(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 2),])
    save.1$weather.szn.2.g <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 2), 'greenness'], na.rm=TRUE)
    save.1$weather.szn.2.w <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 2), 'wetness'], na.rm=TRUE)
    save.1$weather.szn.2.b <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 2), 'brightness'], na.rm=TRUE)
    save.1$weather.szn.2.n <- mean(tc.temp.nolag[which(tc.temp.nolag$weather.szn == 2), 'NDWI'], na.rm=TRUE)
    OUT1 <- rbind(OUT1, save.1)

    ## 6-month lag
    ## save annual information (number of scenes/observations, annual averages)
    save.2$num.obs <- nrow(tc.temp.6molag)
    save.2$ann.mean.g <- mean(tc.temp.6molag$greenness, na.rm=TRUE)
    save.2$ann.mean.w <- mean(tc.temp.6molag$wetness, na.rm=TRUE)
    save.2$ann.mean.b <- mean(tc.temp.6molag$brightness, na.rm=TRUE)
    save.2$ann.mean.n <- mean(tc.temp.6molag$NDWI, na.rm=TRUE)
    save.2$ann.sd.g <- sd(tc.temp.6molag$greenness, na.rm=TRUE)
    save.2$ann.sd.w <- sd(tc.temp.6molag$wetness, na.rm=TRUE)
    save.2$ann.sd.b <- sd(tc.temp.6molag$brightness, na.rm=TRUE)
    save.2$ann.sd.n <- sd(tc.temp.6molag$NDWI, na.rm=TRUE)
    save.2$ann.max.g <- max(tc.temp.6molag$greenness, na.rm=TRUE)
    save.2$ann.max.w <- max(tc.temp.6molag$wetness, na.rm=TRUE)
    save.2$ann.max.b <- max(tc.temp.6molag$brightness, na.rm=TRUE)
    save.2$ann.max.n <- max(tc.temp.6molag$NDWI, na.rm=TRUE)
    save.2$ann.min.g <- min(tc.temp.6molag$greenness, na.rm=TRUE)
    save.2$ann.min.w <- min(tc.temp.6molag$wetness, na.rm=TRUE)
    save.2$ann.min.b <- min(tc.temp.6molag$brightness, na.rm=TRUE)
    save.2$ann.min.n <- min(tc.temp.6molag$NDWI, na.rm=TRUE)
    ## save month-based season data for each of 4 seasons
    save.2$month.szn.1.obs <- nrow(tc.temp.6molag[which(tc.temp.6molag$month.szn == 1),])
    save.2$month.szn.1.g <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 1), 'greenness'], na.rm=TRUE)
    save.2$month.szn.1.w <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 1), 'wetness'], na.rm=TRUE)
    save.2$month.szn.1.b <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 1), 'brightness'], na.rm=TRUE)
    save.2$month.szn.1.n <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 1), 'NDWI'], na.rm=TRUE)
    save.2$month.szn.2.obs <- nrow(tc.temp.6molag[which(tc.temp.6molag$month.szn == 2),])
    save.2$month.szn.2.g <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 2), 'greenness'], na.rm=TRUE)
    save.2$month.szn.2.w <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 2), 'wetness'], na.rm=TRUE)
    save.2$month.szn.2.b <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 2), 'brightness'], na.rm=TRUE)
    save.2$month.szn.2.n <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 2), 'NDWI'], na.rm=TRUE)
    save.2$month.szn.3.obs <- nrow(tc.temp.6molag[which(tc.temp.6molag$month.szn == 3),])
    save.2$month.szn.3.g <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 3), 'greenness'], na.rm=TRUE)
    save.2$month.szn.3.w <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 3), 'wetness'], na.rm=TRUE)
    save.2$month.szn.3.b <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 3), 'brightness'], na.rm=TRUE)
    save.2$month.szn.3.n <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 3), 'NDWI'], na.rm=TRUE)
    save.2$month.szn.4.obs <- nrow(tc.temp.6molag[which(tc.temp.6molag$month.szn == 4),])
    save.2$month.szn.4.g <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 4), 'greenness'], na.rm=TRUE)
    save.2$month.szn.4.w <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 4), 'wetness'], na.rm=TRUE)
    save.2$month.szn.4.b <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 4), 'brightness'], na.rm=TRUE)
    save.2$month.szn.4.n <- mean(tc.temp.6molag[which(tc.temp.6molag$month.szn == 4), 'NDWI'], na.rm=TRUE)
    ## save weather-based season data for each of 3 seasons
    save.2$weather.szn.0.obs <- nrow(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 0),])
    save.2$weather.szn.0.g <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 0), 'greenness'], na.rm=TRUE)
    save.2$weather.szn.0.w <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 0), 'wetness'], na.rm=TRUE)
    save.2$weather.szn.0.b <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 0), 'brightness'], na.rm=TRUE)
    save.2$weather.szn.0.n <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 0), 'NDWI'], na.rm=TRUE)
    save.2$weather.szn.1.obs <- nrow(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 1),])
    save.2$weather.szn.1.g <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 1), 'greenness'], na.rm=TRUE)
    save.2$weather.szn.1.w <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 1), 'wetness'], na.rm=TRUE)
    save.2$weather.szn.1.b <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 1), 'brightness'], na.rm=TRUE)
    save.2$weather.szn.1.n <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 1), 'NDWI'], na.rm=TRUE)
    save.2$weather.szn.2.obs <- nrow(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 2),])
    save.2$weather.szn.2.g <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 2), 'greenness'], na.rm=TRUE)
    save.2$weather.szn.2.w <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 2), 'wetness'], na.rm=TRUE)
    save.2$weather.szn.2.b <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 2), 'brightness'], na.rm=TRUE)
    save.2$weather.szn.2.n <- mean(tc.temp.6molag[which(tc.temp.6molag$weather.szn == 2), 'NDWI'], na.rm=TRUE)
    OUT2 <- rbind(OUT2, save.2)

    ## 1-year lag
    ## save annual information (number of scenes/observations, annual averages)
    save.3$num.obs <- nrow(tc.temp.1yearlag)
    save.3$ann.mean.g <- mean(tc.temp.1yearlag$greenness, na.rm=TRUE)
    save.3$ann.mean.w <- mean(tc.temp.1yearlag$wetness, na.rm=TRUE)
    save.3$ann.mean.b <- mean(tc.temp.1yearlag$brightness, na.rm=TRUE)
    save.3$ann.mean.n <- mean(tc.temp.1yearlag$NDWI, na.rm=TRUE)
    save.3$ann.sd.g <- sd(tc.temp.1yearlag$greenness, na.rm=TRUE)
    save.3$ann.sd.w <- sd(tc.temp.1yearlag$wetness, na.rm=TRUE)
    save.3$ann.sd.b <- sd(tc.temp.1yearlag$brightness, na.rm=TRUE)
    save.3$ann.sd.n <- sd(tc.temp.1yearlag$NDWI, na.rm=TRUE)
    save.3$ann.max.g <- max(tc.temp.1yearlag$greenness, na.rm=TRUE)
    save.3$ann.max.w <- max(tc.temp.1yearlag$wetness, na.rm=TRUE)
    save.3$ann.max.b <- max(tc.temp.1yearlag$brightness, na.rm=TRUE)
    save.3$ann.max.n <- max(tc.temp.1yearlag$NDWI, na.rm=TRUE)
    save.3$ann.min.g <- min(tc.temp.1yearlag$greenness, na.rm=TRUE)
    save.3$ann.min.w <- min(tc.temp.1yearlag$wetness, na.rm=TRUE)
    save.3$ann.min.b <- min(tc.temp.1yearlag$brightness, na.rm=TRUE)
    save.3$ann.min.n <- min(tc.temp.1yearlag$NDWI, na.rm=TRUE)
    ## save month-based season data for each of 4 seasons
    save.3$month.szn.1.obs <- nrow(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 1),])
    save.3$month.szn.1.g <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 1), 'greenness'], na.rm=TRUE)
    save.3$month.szn.1.w <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 1), 'wetness'], na.rm=TRUE)
    save.3$month.szn.1.b <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 1), 'brightness'], na.rm=TRUE)
    save.3$month.szn.1.n <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 1), 'NDWI'], na.rm=TRUE)
    save.3$month.szn.2.obs <- nrow(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 2),])
    save.3$month.szn.2.g <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 2), 'greenness'], na.rm=TRUE)
    save.3$month.szn.2.w <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 2), 'wetness'], na.rm=TRUE)
    save.3$month.szn.2.b <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 2), 'brightness'], na.rm=TRUE)
    save.3$month.szn.2.n <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 2), 'NDWI'], na.rm=TRUE)
    save.3$month.szn.3.obs <- nrow(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 3),])
    save.3$month.szn.3.g <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 3), 'greenness'], na.rm=TRUE)
    save.3$month.szn.3.w <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 3), 'wetness'], na.rm=TRUE)
    save.3$month.szn.3.b <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 3), 'brightness'], na.rm=TRUE)
    save.3$month.szn.3.n <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 3), 'NDWI'], na.rm=TRUE)
    save.3$month.szn.4.obs <- nrow(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 4),])
    save.3$month.szn.4.g <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 4), 'greenness'], na.rm=TRUE)
    save.3$month.szn.4.w <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 4), 'wetness'], na.rm=TRUE)
    save.3$month.szn.4.b <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 4), 'brightness'], na.rm=TRUE)
    save.3$month.szn.4.n <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$month.szn == 4), 'NDWI'], na.rm=TRUE)
    ## save weather-based season data for each of 3 seasons
    save.3$weather.szn.0.obs <- nrow(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 0),])
    save.3$weather.szn.0.g <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 0), 'greenness'], na.rm=TRUE)
    save.3$weather.szn.0.w <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 0), 'wetness'], na.rm=TRUE)
    save.3$weather.szn.0.b <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 0), 'brightness'], na.rm=TRUE)
    save.3$weather.szn.0.n <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 0), 'NDWI'], na.rm=TRUE)
    save.3$weather.szn.1.obs <- nrow(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 1),])
    save.3$weather.szn.1.g <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 1), 'greenness'], na.rm=TRUE)
    save.3$weather.szn.1.w <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 1), 'wetness'], na.rm=TRUE)
    save.3$weather.szn.1.b <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 1), 'brightness'], na.rm=TRUE)
    save.3$weather.szn.1.n <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 1), 'NDWI'], na.rm=TRUE)
    save.3$weather.szn.2.obs <- nrow(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 2),])
    save.3$weather.szn.2.g <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 2), 'greenness'], na.rm=TRUE)
    save.3$weather.szn.2.w <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 2), 'wetness'], na.rm=TRUE)
    save.3$weather.szn.2.b <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 2), 'brightness'], na.rm=TRUE)
    save.3$weather.szn.2.n <- mean(tc.temp.1yearlag[which(tc.temp.1yearlag$weather.szn == 2), 'NDWI'], na.rm=TRUE)
    OUT3 <- rbind(OUT3, save.3)

    if(n %% 1000 == 0){
      write.table(OUT1, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_nolag.txt',
                  row.names = FALSE, quote=FALSE, sep='\t', append=TRUE, col.names=FALSE)
      write.table(OUT2, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_6monthlag.txt',
                  row.names = FALSE, quote=FALSE, sep='\t', append=TRUE, col.names=FALSE)
      write.table(OUT3, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_1yearlag.txt',
                  row.names = FALSE, quote=FALSE, sep='\t', append=TRUE, col.names=FALSE)
      
      OUT1 <- NULL ## no lag
      OUT2 <- NULL ## 6-month lag
      OUT3 <- NULL ## 1-year lag
    }
    n <- n+1
  }
  write.table(OUT1, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_nolag.txt',
              row.names = FALSE, quote=FALSE, sep='\t', append=TRUE, col.names=FALSE)
  write.table(OUT2, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_6monthlag.txt',
              row.names = FALSE, quote=FALSE, sep='\t', append=TRUE, col.names=FALSE)
  write.table(OUT3, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_1yearlag.txt',
              row.names = FALSE, quote=FALSE, sep='\t', append=TRUE, col.names=FALSE)

  OUT1 <- NULL ## no lag
  OUT2 <- NULL ## 6-month lag
  OUT3 <- NULL ## 1-year lag
}

## read in data and check for correlations
nolag <- read.table('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_nolag.txt', sep='\t')
sixmo.lag <- read.table('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_6monthlag.txt', sep='\t')
oneyear.lag <- read.table('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/all_cells_data_1yearlag.txt', sep='\t')

colnames(nolag)[1:19] <- c('cell.num','year','num.obs','ann.mean.g','ann.mean.w','ann.mean.b','ann.mean.n','ann.sd.g ','ann.sd.w ','ann.sd.b ','ann.sd.n ','ann.max.g','ann.max.w','ann.max.b','ann.max.n','ann.min.g','ann.min.w','ann.min.b','ann.min.n')
pairs(nolag[,c(4,8,12,16)], main='Greenness')
