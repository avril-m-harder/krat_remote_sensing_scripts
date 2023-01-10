library(sp)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
options("rgdal_show_exportToProj4_warnings"="none")
# library(rgdal)
library(TeachingDemos)
library(reshape2)
library(ggplot2)
`%notin%` <- Negate(`%in%`)

##### 1. Read in data #####
## read in information on occupied mounds:cells
## read in mounds occupied by an adult female or a juvenile, identified in demo_analyses.R
occ.mnds <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_occupied_mounds.csv')
occ.mnds <- occ.mnds[occ.mnds$year >= 1994 & occ.mnds$year <= 2005,]

## read in mound:cell assignments for occupied mounds
## from the first time I tried this
fit <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/archive/fitness_variables.csv')
fit <- fit[c(2,1)]
colnames(fit)[1] <- 'mound'
rd1 <- read.csv('/Users/Avril/Documents/krat_remote_sensing/sorting_out_mound_names/2021_first_round/manual_mound_cell_assignments.csv')
rd1[rd1$terr == 'R2', 'terr'] <- rd1[rd1$terr == 'R2', 'new.db.name']
rd1 <- rd1[,c(3,5)]
colnames(rd1) <- c('mound','cell.num')
## and the second time, adding some mounds with the new temporal/spatial scale
rd2 <- read.table('/Users/Avril/Documents/krat_remote_sensing/sorting_out_mound_names/2022_second_round/22_04_manual_cell_assignments.txt',
                sep = '\t', header = TRUE)
colnames(rd2) <- c('mound','cell.num')

## combine and clean
mnd.cells <- rbind(fit, rd1, rd2)
mnd.cells <- mnd.cells[!duplicated(mnd.cells),]
mnd.cells[duplicated(mnd.cells$mound),] ## no duplicated mound names; just need to remove the NA assignments
mnd.cells <- mnd.cells[!is.na(mnd.cells$cell.num),]

## get rid of mounds not associated with a cell (only 4 instances / 1,024)
occ.mnds <- occ.mnds[occ.mnds$database.name %in% mnd.cells$mound,]
## for each year, identify occupied and adjacent cells
OUT <- NULL
for(y in unique(occ.mnds$year)){
  y.mnds <- occ.mnds[occ.mnds$year == y, 'database.name']
  y.cells <- mnd.cells[mnd.cells$mound %in% y.mnds, 'cell.num']
  y.adj.cells <- NULL
  for(c in y.cells){
      ## identify 8 adjacent (surrounding) cells ; 92 rows, 70 columns in raster;
      ## math confirmed by manually plotting
      adj <- c((c-71), (c-70), (c-69), (c-1), (c+1), (c+69), (c+70), (c+71))
      y.adj.cells <- c(y.adj.cells, adj)
  }
  ## limit y.adj.cells to only adjacent cells, not cells also occupied (lots of overlap)
  y.adj.cells <- y.adj.cells[y.adj.cells %notin% y.cells]
  y.cells <- cbind(y.cells, rep(y, length(y.cells)), rep(1, length(y.cells)))
  y.adj.cells <- cbind(y.adj.cells, rep(y, length(y.adj.cells)), rep(0, length(y.adj.cells)))
  OUT <- rbind(OUT, y.cells, y.adj.cells)
}
occ.cells <- as.data.frame(OUT)
colnames(occ.cells) <- c('cell.num','year','status') ## 1 = occupied; 0 = adjacent only
occ.cells <- occ.cells[!duplicated(occ.cells),]

## read in remote sensing predictor variables
tc.fn <- 'tc_manual_cloudcheck' ## for TC data -- tc_manual_cloudcheck
tc.dat <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'))
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,1]
tc.dat$year <- as.numeric(tc.dat$year)
tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,2]
tc.dat$month <- as.numeric(tc.dat$month)
## create month index for easier identification of lag periods
tc.dat$month.index <- ((tc.dat$year-1) * 12) + tc.dat$month
tc.dat <- tc.dat[tc.dat$year >= 1993 & tc.dat$year <= 2005,]
## plot distribution of time points
time.pts <- unique(tc.dat[,c('year','doy')])
## assign meteorological seasons
time.pts[time.pts$doy < 60 | time.pts$doy >= 335, 'season'] <- 1
time.pts[time.pts$doy >= 60 & time.pts$doy < 152, 'season'] <- 2
time.pts[time.pts$doy >= 152 & time.pts$doy < 244, 'season'] <- 3
time.pts[time.pts$doy >= 244 & time.pts$doy < 335, 'season'] <- 4
plot(time.pts$doy, time.pts$year, xlim=c(1,366), pch = 19, xlab = 'Day of year', ylab = 'Year', col = 'transparent')
  points(time.pts[time.pts$season == 1, 'doy'], time.pts[time.pts$season == 1, 'year'], col = 'darkblue', pch = 19)
  points(time.pts[time.pts$season == 2, 'doy'], time.pts[time.pts$season == 2, 'year'], col = 'springgreen3', pch = 19)
  points(time.pts[time.pts$season == 3, 'doy'], time.pts[time.pts$season == 3, 'year'], col = 'darkgoldenrod1', pch = 19)
  points(time.pts[time.pts$season == 4, 'doy'], time.pts[time.pts$season == 4, 'year'], col = 'darkgoldenrod4', pch = 19)
OUT <- NULL
for(y in unique(time.pts$year)){
  save <- c(y, nrow(time.pts[time.pts$year == y & time.pts$season == 1,]),
            nrow(time.pts[time.pts$year == y & time.pts$season == 2,]),
            nrow(time.pts[time.pts$year == y & time.pts$season == 3,]),
            nrow(time.pts[time.pts$year == y & time.pts$season == 4,]))
  OUT <- rbind(OUT, save)
}
colnames(OUT) <- c('year','winter','spring','summer','autumn')
write.table(OUT, '/Users/Avril/Desktop/yearly_season_totals.txt', sep = '\t', quote = FALSE, row.names = FALSE)
## add season to tc.dat for season equalization
tc.dat[tc.dat$doy < 60 | tc.dat$doy >= 335, 'season'] <- 1
tc.dat[tc.dat$doy >= 60 & tc.dat$doy < 152, 'season'] <- 2
tc.dat[tc.dat$doy >= 152 & tc.dat$doy < 244, 'season'] <- 3
tc.dat[tc.dat$doy >= 244 & tc.dat$doy < 335, 'season'] <- 4

## Examine predictor variable correlations
## (pretty heatmap plotting of correlations: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization )
inds <- c(2:(grep('acq.date', colnames(tc.dat))-1)) ## get column numbers for remote sensing index data
cors <- round(cor(tc.dat[,inds]), 2)
## NA values for EVI2, MNDWI, NBRI, NDWI2 and TVI, so get rid of those
inds <- inds[inds %notin% c(which(colnames(tc.dat) == 'EVI2'), which(colnames(tc.dat) == 'MNDWI'),
                            which(colnames(tc.dat) == 'NBRI'), which(colnames(tc.dat) == 'NDWI2'),
                            which(colnames(tc.dat) == 'TVI'))]
## check again
cors <- round(cor(tc.dat[,inds]), 2) ## lots of highly correlated variables, but just keeping them for now I guess
## remove EVI2 and TVI from data set
tc.dat <- tc.dat[,-c(which(colnames(tc.dat) == 'EVI2'), which(colnames(tc.dat) == 'MNDWI'),
                     which(colnames(tc.dat) == 'NBRI'), which(colnames(tc.dat) == 'NDWI2'),
                     which(colnames(tc.dat) == 'TVI'))]
## get column indices for final set of remote sensing data
inds <- c(2:(grep('acq.date', colnames(tc.dat))-1))
## eh, just use TC + temp.k
inds <- c(2,3,4,5)

# ##### 2. Calculate summary statistics for occupied and adjacent cells #####
# ## 4 nonoverlapping 6-month intervals
# ALL <- NULL
# OCC <- NULL
# for(y in unique(occ.cells$year)){      ## for each year we need stats for,
#   sub <- occ.cells[occ.cells$year == y,]
#   cells.occ <- sub[sub$status == 1, 'cell.num']
#   cells.all <- sub$cell.num
#   ## collect data for 4 6-month time intervals covering years t-1 and t
#   i1 <- (y*12)-23                 ## set first month of first 6-month interval (Jan-June, year t-1),
#   i2 <- (y*12)-17                 ## set first month of second 6-month interval (July-Dec, year t-1),
#   i3 <- (y*12)-11                 ## set first month of third 6-month interval (Jan-June, year t),
#   i4 <- (y*12)-5                  ## set first month of fourth 6-month interval (July-Dec, year t).
#   ints <- c(i1,i2,i3,i4)
#   for(s in c(1,2,3,4)){    ## for each 6-month interval,
#     i <- ints[s]
#     all <- tc.dat[which(tc.dat$month.index >= i & tc.dat$month.index <= i+5 & tc.dat$cell.num %in% cells.all), ]
#     occ <- tc.dat[which(tc.dat$month.index >= i & tc.dat$month.index <= i+5 & tc.dat$cell.num %in% cells.occ), ]
#     ## summarize data using all cells (occupied + adjacent)
#     t.pts <- length(unique(all$prod.id))     ## get number of remote sensing observations,
#     n.cells <- length(cells.all)
#     save.a <- c(y, t.pts, n.cells, s)
#     for(col in inds){                        ## for each remote sensing measure,
#       temp <- all[,col]                      ## get all values,
#       avg <- mean(temp)                      ## calculate mean,
#       sd <- sd(temp)                         ## and SD.
#       save.a <- c(save.a, avg, sd)           ## save the information.
#     }
#     ## summarize data using just occupied cells
#     t.pts <- length(unique(occ$prod.id))     ## get number of remote sensing observations,
#     n.cells <- length(cells.occ)
#     save.o <- c(y, t.pts, n.cells, s)
#     for(col in inds){                ## for each remote sensing measure,
#       temp <- occ[,col]              ## get occ values,
#       avg <- mean(temp)              ## calculate mean,
#       sd <- sd(temp)                 ## and SD.
#       save.o <- c(save.o, avg, sd)   ## save the information.
#     }
#     ALL <- rbind(ALL, save.a)
#     OCC <- rbind(OCC, save.o)
#   }
# }
# ALL <- as.data.frame(ALL)
# OCC <- as.data.frame(OCC)
# colnames(ALL)[c(1:4)] <- c('year','num.obs','num.cells','interval')
# colnames(OCC)[c(1:4)] <- c('year','num.obs','num.cells','interval')
# ## create a vector of column names
# indices <- colnames(tc.dat)[inds]
# c <- 5
# while(c <= ncol(ALL)){
#   for(ind in indices){
#     colnames(ALL)[c] <- paste0(ind,'.mean')
#     c <- c+1
#     colnames(ALL)[c] <- paste0(ind,'.sd')
#     c <- c+1
#   }
# }
# c <- 5
# while(c <= ncol(OCC)){
#   for(ind in indices){
#     colnames(OCC)[c] <- paste0(ind,'.mean')
#     c <- c+1
#     colnames(OCC)[c] <- paste0(ind,'.sd')
#     c <- c+1
#   }
# }
# ALL <- ALL[order(ALL$year),]
# OCC <- OCC[order(OCC$year),]
# write.csv(ALL, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_6monthinterval_means.csv', row.names = FALSE)
# write.csv(OCC, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_6monthinterval_means.csv', row.names = FALSE)
# 
# ## 3 overlapping 1-year intervals
# ALL <- NULL
# OCC <- NULL
# for(y in unique(occ.cells$year)){      ## for each year we need stats for,
#   sub <- occ.cells[occ.cells$year == y,]
#   cells.occ <- sub[sub$status == 1, 'cell.num']
#   cells.all <- sub$cell.num
#   ## collect data for 3 12-month time intervals covering years t-1 and t
#   i1 <- (y*12)-23                 ## set first month of first 12-month interval (Jan-Dec, year t-1),
#   i2 <- (y*12)-17                 ## set first month of second 12-month interval (July, year t-1 - June, year t),
#   i3 <- (y*12)-11                 ## set first month of third 12-month interval (Jan-Dec, year t).
#   ints <- c(i1,i2,i3)
#   for(s in c(1,2,3)){    ## for each 12-month interval,
#     i <- ints[s]
#     all <- tc.dat[which(tc.dat$month.index >= i & tc.dat$month.index <= i+11 & tc.dat$cell.num %in% cells.all), ]
#     occ <- tc.dat[which(tc.dat$month.index >= i & tc.dat$month.index <= i+11 & tc.dat$cell.num %in% cells.occ), ]
#     ## summarize data using all cells (occupied + adjacent)
#     t.pts <- length(unique(all$prod.id))     ## get number of remote sensing observations,
#     n.cells <- length(cells.all)
#     save.a <- c(y, t.pts, n.cells, s)
#     for(col in inds){                        ## for each remote sensing measure,
#       temp <- all[,col]                      ## get all values,
#       avg <- mean(temp)                      ## calculate mean,
#       sd <- sd(temp)                         ## and SD.
#       save.a <- c(save.a, avg, sd)           ## save the information.
#     }
#     ## summarize data using just occupied cells
#     t.pts <- length(unique(occ$prod.id))     ## get number of remote sensing observations,
#     n.cells <- length(cells.occ)
#     save.o <- c(y, t.pts, n.cells, s)
#     for(col in inds){                ## for each remote sensing measure,
#       temp <- occ[,col]              ## get occ values,
#       avg <- mean(temp)              ## calculate mean,
#       sd <- sd(temp)                 ## and SD.
#       save.o <- c(save.o, avg, sd)   ## save the information.
#     }
#     ALL <- rbind(ALL, save.a)
#     OCC <- rbind(OCC, save.o)
#   }
# }
# ALL <- as.data.frame(ALL)
# OCC <- as.data.frame(OCC)
# colnames(ALL)[c(1:4)] <- c('year','num.obs','num.cells','interval')
# colnames(OCC)[c(1:4)] <- c('year','num.obs','num.cells','interval')
# ## create a vector of column names
# indices <- colnames(tc.dat)[inds]
# c <- 5
# while(c <= ncol(ALL)){
#   for(ind in indices){
#     colnames(ALL)[c] <- paste0(ind,'.mean')
#     c <- c+1
#     colnames(ALL)[c] <- paste0(ind,'.sd')
#     c <- c+1
#   }
# }
# c <- 5
# while(c <= ncol(OCC)){
#   for(ind in indices){
#     colnames(OCC)[c] <- paste0(ind,'.mean')
#     c <- c+1
#     colnames(OCC)[c] <- paste0(ind,'.sd')
#     c <- c+1
#   }
# }
# ALL <- ALL[order(ALL$year),]
# OCC <- OCC[order(OCC$year),]
# write.csv(ALL, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means.csv', row.names = FALSE)
# write.csv(OCC, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_12monthinterval_means.csv', row.names = FALSE)

## 3 overlapping 1-year intervals - seasons equalized
ALL <- NULL
OCC <- NULL
for(y in unique(occ.cells$year)){      ## for each year we need stats for,
  sub <- occ.cells[occ.cells$year == y,]
  cells.occ <- sub[sub$status == 1, 'cell.num']
  cells.all <- sub$cell.num
  ## collect data for 3 12-month time intervals covering years t-1 and t
  i1 <- (y*12)-23                 ## set first month of first 12-month interval (Jan-Dec, year t-1),
  i2 <- (y*12)-17                 ## set first month of second 12-month interval (July, year t-1 - June, year t),
  i3 <- (y*12)-11                 ## set first month of third 12-month interval (Jan-Dec, year t).
  ints <- c(i1,i2,i3)
  for(s in c(1,2,3)){    ## for each 12-month interval,
    i <- ints[s]
    all <- tc.dat[which(tc.dat$month.index >= i & tc.dat$month.index <= i+11 & tc.dat$cell.num %in% cells.all), ]
    occ <- tc.dat[which(tc.dat$month.index >= i & tc.dat$month.index <= i+11 & tc.dat$cell.num %in% cells.occ), ]
    ## summarize data using all cells (occupied + adjacent)
    t.pts <- length(unique(all$prod.id))     ## get number of remote sensing observations,
    n.cells <- length(cells.all)
    save.a <- c(y, t.pts, n.cells, s)
    for(col in inds){                        ## for each remote sensing measure,
      temp <- all[,c(col,ncol(all))]             ## get all values,
      avg <- mean(temp[,1])                      ## calculate mean,
      sd <- sd(temp[,1])                         ## and SD.
      ## also calculate season-equalized mean values
      m.1 <- mean(temp[temp$season == 1, 1])
      m.2 <- mean(temp[temp$season == 2, 1])
      m.3 <- mean(temp[temp$season == 3, 1])
      m.4 <- mean(temp[temp$season == 4, 1])
      s.avg <- mean(c(m.1,m.2,m.3,m.4), na.rm = TRUE)
      save.a <- c(save.a, avg, sd, s.avg)        ## save the information.
    }
    ## summarize data using just occupied cells
    t.pts <- length(unique(occ$prod.id))     ## get number of remote sensing observations,
    n.cells <- length(cells.occ)
    save.o <- c(y, t.pts, n.cells, s)
    for(col in inds){                        ## for each remote sensing measure,
      temp <- occ[,c(col,ncol(occ))]             ## get occ values,
      avg <- mean(temp[,1])                      ## calculate mean,
      sd <- sd(temp[,1])                         ## and SD.
      ## also calculate season-equalized mean values
      m.1 <- mean(temp[temp$season == 1, 1])
      m.2 <- mean(temp[temp$season == 2, 1])
      m.3 <- mean(temp[temp$season == 3, 1])
      m.4 <- mean(temp[temp$season == 4, 1])
      s.avg <- mean(c(m.1,m.2,m.3,m.4), na.rm = TRUE)
      save.o <- c(save.o, avg, sd, s.avg)        ## save the information.
    }
    ALL <- rbind(ALL, save.a)
    OCC <- rbind(OCC, save.o)
  }
}
ALL <- as.data.frame(ALL)
OCC <- as.data.frame(OCC)
colnames(ALL)[c(1:4)] <- c('year','num.obs','num.cells','interval')
colnames(OCC)[c(1:4)] <- c('year','num.obs','num.cells','interval')
## create a vector of column names
indices <- colnames(tc.dat)[inds]
c <- 5
while(c <= ncol(ALL)){
  for(ind in indices){
    colnames(ALL)[c] <- paste0(ind,'.mean')
    c <- c+1
    colnames(ALL)[c] <- paste0(ind,'.sd')
    c <- c+1
    colnames(ALL)[c] <- paste0(ind,'.mean.szneq')
    c <- c+1
  }
}
c <- 5
while(c <= ncol(OCC)){
  for(ind in indices){
    colnames(OCC)[c] <- paste0(ind,'.mean')
    c <- c+1
    colnames(OCC)[c] <- paste0(ind,'.sd')
    c <- c+1
    colnames(ALL)[c] <- paste0(ind,'.mean.szneq')
    c <- c+1
  }
}
ALL <- ALL[order(ALL$year),]
OCC <- OCC[order(OCC$year),]
write.csv(ALL, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means_seasoneq.csv', row.names = FALSE)
write.csv(OCC, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_12monthinterval_means_seasoneq.csv', row.names = FALSE)

##### 3. Apply filters to data #####
## 6-month intervals
# all.cells <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_6monthinterval_means.csv')
# occ.cells <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_6monthinterval_means.csv')
# m <- 4
# ## 12-month intervals
all.cells <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means.csv')
occ.cells <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_12monthinterval_means.csv')
m <- 3

## read in demographic data
demo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_demo_data.csv')
all.dat <- merge(all.cells, demo, by='year', all.x = TRUE)
occ.dat <- merge(occ.cells, demo, by='year', all.x = TRUE)

## plot correlations for each time interval separately
# inds <- gsub('.mean','',colnames(all.dat)[grep('mean', colnames(all.dat))])
## for now, since we're scaling down, just keep TC + temp.k
inds <- c('greenness','wetness','brightness','temp.k')
pdf('/Users/Avril/Desktop/test.pdf', width=5, height=5)
for(i in inds){
  c <- grep(i, colnames(all.dat))
  sub <- all.dat[,c(1:4,47:53,c[1])]
  for(n in c(1:m)){
    ## col 10 == num.off.perf; col 11 = num.surv.perf; col 7 = avg. age at death
    dep <- 7
    plot(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='darkorchid3',
         xlab=colnames(sub)[12], ylab=colnames(sub)[dep], main=n)
      mod <- lm(sub[sub$interval == n, dep] ~ sub[sub$interval == n, 12])
      if(summary(mod)$coefficients[,4][2] <= 0.05){
        points(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='springgreen3')
        abline(mod, lty=2)
        legend('topright', legend=paste0('p = ', round(summary(mod)$coefficients[,4][2], digits = 4),
                                         '\nr2 = ', round(summary(mod)$r.squared, digits = 4)))
      }
  }
}
dev.off()
