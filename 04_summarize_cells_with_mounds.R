### Output of this script:
###
### occ_and_adj_cells_12monthinterval_means_seasoneq.csv = remote sensing data averaged over each year (using [i] all observations
### and [ii] equalizing observations over meteorological seasons), for each summer rainy season (July + August), and for each winter rainy
### season (December - March) for all occupied and adjacent cells
###
### prism_annual_and_seasonal_means.csv = precipitation totals and averaged min, mean, and max temperatures on an annual and
### rainy seasonal basis

library(sp)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
library(NatParksPalettes)
options("rgdal_show_exportToProj4_warnings"="none")
# library(rgdal)
library(TeachingDemos)
library(reshape2)
library(ggplot2)
library(beyonce)
`%notin%` <- Negate(`%in%`)

## Yellowstone palette option
w.col <- natparks.pals('Yellowstone')[1]
g.col <- natparks.pals('Yellowstone')[4]
t.col <- natparks.pals('Yellowstone')[3]
b.col <- natparks.pals('Yellowstone')[6]
pp.col <- natparks.pals('Yellowstone')[5]
pt.col <- natparks.pals('Olympic')[7]
cols <- c(g.col, w.col, b.col, t.col, pp.col, pt.col)

## Beyonce palette
w.col <- beyonce_palette(75)[2]
b.col <- beyonce_palette(75)[5]
t.col <- beyonce_palette(75)[7]
cols <- c(w.col, b.col, t.col, g.col)
names <- c('wetness','brightness','temp.k','greenness')

## plot colors to test
pdf('/Users/Avril/Desktop/colors.pdf', width = 6, height = 5)
plot(0,0, xlim = c(0,7), ylim = c(0,1), col = 'transparent')
for(c in 1:6){
  polygon(c(c-1,c,c,c-1), c(0,0,1,1), col = cols[c], border = NA)
}
dev.off()

##### 1. Read in data #####
## read in information on occupied mounds:cells
## read in mounds occupied by an adult female or a juvenile, identified in demo_analyses.R
occ.mnds <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_occupied_mounds.csv')
occ.mnds <- occ.mnds[occ.mnds$year >= 1993 & occ.mnds$year <= 2005,] ## limited temporal coverage before 1993

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
## keep TC and temp.k
tc.dat <- tc.dat[,c(1:5,28:35)]
## get year and month #s
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,1]
tc.dat$year <- as.numeric(tc.dat$year)
tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,2]
tc.dat$month <- as.numeric(tc.dat$month)
tc.dat <- tc.dat[tc.dat$year <= 2005,] ## already meets this criterion
## plot distribution of time points
time.pts <- unique(tc.dat[,c('year','doy')])
## assign meteorological seasons
time.pts[time.pts$doy < 60 | time.pts$doy >= 335, 'season'] <- 1
time.pts[time.pts$doy >= 60 & time.pts$doy < 152, 'season'] <- 2
time.pts[time.pts$doy >= 152 & time.pts$doy < 244, 'season'] <- 3
time.pts[time.pts$doy >= 244 & time.pts$doy < 335, 'season'] <- 4

pdf('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/seasonal_availability_of_landsat_data.pdf',
    width = 11, height = 5)
par(mar = c(5.1, 4.1, 4.1, 10.1), xpd = FALSE)
s.rain <- 'grey90'
w.rain <- 'grey70'
win <- 'darkblue'
spr <- 'springgreen2'
sum <- 'darkgoldenrod1'
aut <- 'darkgoldenrod4'
plot(time.pts$doy, time.pts$year, xlim=c(1,366), pch = 19, xlab = 'Day of year', ylab = 'Year', col = 'transparent',
     ylim = c(1992, 2005))
  polygon(x = c(0, 90, 90, 0), y = c(1990, 1990, 2010, 2010), border = NA, col = w.rain)
  polygon(x = c(335, 365, 365, 335), y = c(1990, 1990, 2010, 2010), border = NA, col = w.rain)
  polygon(x = c(182, 243, 243, 182), y = c(1990, 1990, 2010, 2010), border = NA, col = s.rain)
  points(time.pts[time.pts$season == 1, 'doy'], time.pts[time.pts$season == 1, 'year'], col = 'black', bg = win, pch = 21)       ## winter
  points(time.pts[time.pts$season == 2, 'doy'], time.pts[time.pts$season == 2, 'year'], col = 'black', bg = spr, pch = 21)   ## spring
  points(time.pts[time.pts$season == 3, 'doy'], time.pts[time.pts$season == 3, 'year'], col = 'black', bg = sum, pch = 21) ## summer
  points(time.pts[time.pts$season == 4, 'doy'], time.pts[time.pts$season == 4, 'year'], col = 'black', bg = aut, pch = 21) ## fall
  par(xpd = TRUE)
  legend('right', legend = c('Winter','Spring','Summer','Autumn','Summer rainy season','Winter rainy season'),
         pch = c(21, 21, 21, 21, NA, NA), fill = c(NA, NA, NA, NA, s.rain, w.rain), border = NA,
         pt.bg = c(win, spr, sum, aut, NA, NA), inset = -.25, bty = 'n')
  # abline(h = 1992.5, lty = 1, col = 'red', lwd = 2)
  ## seasonal observations are pretty unevenly represented across years, should be equalized within seasons/years
dev.off()

pdf('/Users/Avril/Documents/krat_remote_sensing/C2L2_landsat_5_data_overviews/seasonal_availability_of_landsat_data_w_offset.pdf',
    width = 9, height = 6)

temp <- time.pts
temp[temp$doy >= 182, 'doy2'] <- temp[temp$doy >= 182, 'doy'] - 181
temp[temp$doy < 182, 'doy2'] <- temp[temp$doy < 182, 'doy'] + 184
temp[temp$doy >= 182, 'off.year'] <- temp[temp$doy >= 182, 'year'] + 1
temp[temp$doy < 182, 'off.year'] <- temp[temp$doy < 182, 'year']

par(mar = c(5.1, 4.1, 4.1, 10.1), xpd = FALSE)
s.rain <- 'grey90'
w.rain <- 'grey70'
win <- 'darkblue'
spr <- 'springgreen2'
sum <- 'darkgoldenrod1'
aut <- 'darkgoldenrod4'
plot(temp$doy2, temp$off.year, xlim=c(1,366), pch = 19, xlab = 'Day of year', ylab = 'Offspring year', col = 'transparent',
     ylim = c(1993, 2005), xaxt = 'n')
  axis(1, at = c(1, 154, 275, 365), labels = c('July 1','December 1','April 1','June 30'))
  polygon(x = c(154, 274, 274, 154), y = c(1990, 1990, 2010, 2010), border = NA, col = w.rain)
  polygon(x = c(1, 61, 61, 1), y = c(1990, 1990, 2010, 2010), border = NA, col = s.rain)
  points(temp[temp$season == 1, 'doy2'], temp[temp$season == 1, 'off.year'], col = 'black', bg = win, pch = 22, cex = 1.5) ## winter
  points(temp[temp$season == 2, 'doy2'], temp[temp$season == 2, 'off.year'], col = 'black', bg = spr, pch = 22, cex = 1.5) ## spring
  points(temp[temp$season == 3, 'doy2'], temp[temp$season == 3, 'off.year'], col = 'black', bg = sum, pch = 22, cex = 1.5) ## summer
  points(temp[temp$season == 4, 'doy2'], temp[temp$season == 4, 'off.year'], col = 'black', bg = aut, pch = 22, cex = 1.5) ## fall
  par(xpd = TRUE)
  legend('right', legend = c('Winter','Spring','Summer','Autumn','Summer rainy season','Winter rainy season'),
         pch = c(22, 22, 22, 22, NA, NA), fill = c(NA, NA, NA, NA, s.rain, w.rain), border = NA,
         pt.bg = c(win, spr, sum, aut, NA, NA), inset = -.34, bty = 'n', pt.cex = c(1.5, 1.5, 1.5, 1.5, NA, NA))
  # abline(h = 1992.5, lty = 1, col = 'red', lwd = 2)
  ## seasonal observations are pretty unevenly represented across years, should be equalized within seasons/years
dev.off()

OUT <- NULL
for(y in unique(time.pts$year)){
  save <- c(y, nrow(time.pts[time.pts$year == y & time.pts$season == 1,]),
            nrow(time.pts[time.pts$year == y & time.pts$season == 2,]),
            nrow(time.pts[time.pts$year == y & time.pts$season == 3,]),
            nrow(time.pts[time.pts$year == y & time.pts$season == 4,]))
  OUT <- rbind(OUT, save)
}
colnames(OUT) <- c('year','winter','spring','summer','autumn')
# write.table(OUT, '/Users/Avril/Desktop/yearly_season_totals.txt', sep = '\t', quote = FALSE, row.names = FALSE)
## add season to tc.dat for season equalization
tc.dat[tc.dat$doy < 60 | tc.dat$doy >= 335, 'season'] <- 1
tc.dat[tc.dat$doy >= 60 & tc.dat$doy < 152, 'season'] <- 2
tc.dat[tc.dat$doy >= 152 & tc.dat$doy < 244, 'season'] <- 3
tc.dat[tc.dat$doy >= 244 & tc.dat$doy < 335, 'season'] <- 4

## set 'offspring.year' to indicate which months' data go with which offspring-producing-intervals.
## i.e., July - December in year t-1 contribute to offspring produced in year t.
## July-August year t-1 contribute to offspring produced in year t.
## December year t-1 through March year t comprise the winter rainy season for offspring produced in year t.
tc.dat[tc.dat$month %in% c(7:12), 'offspring.year'] <- tc.dat[tc.dat$month %in% c(7:12), 'year']+1
tc.dat[tc.dat$month %in% c(1:6), 'offspring.year'] <- tc.dat[tc.dat$month %in% c(1:6), 'year']

## keep data from July 1 1992 - June 30 2005 (fixed on 3/21/23 from 1993 --> July 1 1992)
tc.dat <- tc.dat[tc.dat$offspring.year >= 1993 & tc.dat$offspring.year <= 2005,]

## scale and center values for TC and temp.k
tc.dat$greenness <- scale(tc.dat$greenness, center = TRUE, scale = TRUE)
tc.dat$wetness <- scale(tc.dat$wetness, center = TRUE, scale = TRUE)
tc.dat$brightness <- scale(tc.dat$brightness, center = TRUE, scale = TRUE)
# tc.dat$temp.k <- scale(tc.dat$temp.k, center = TRUE, scale = TRUE)

## plot temp C across days of year for all cells active at some point
temp.dat <- tc.dat[, c('cell.num','temp.k','doy','scene.id','season')]
occ.dat <- temp.dat[temp.dat$cell.num %in% occ.cells$cell.num,]
occ.dat$temp.c <- occ.dat$temp.k - 273.15
occ.dat[occ.dat$season == 1, 'colour'] <- 'dodgerblue2'
occ.dat[occ.dat$season == 2, 'colour'] <- 'springgreen3'
occ.dat[occ.dat$season == 3, 'colour'] <- 'gold2'
occ.dat[occ.dat$season == 4, 'colour'] <- 'sienna1'
nonocc.dat <- temp.dat[temp.dat$cell.num %notin% occ.cells$cell.num,]
nonocc.dat$temp.c <- nonocc.dat$temp.k - 273.15

pdf('/Users/Avril/Desktop/landsat_tempk_occupied_cells_notyearspecific.pdf', width = 8, height = 6.5)
plot(0, 0, xlim = c(0, 366), ylim = c(0, 70), col = 'transparent', xlab = 'Day of year', ylab = 'Temperature (deg C)',
     main = 'Landsat temp K for occupied cells (active in any year)')
  abline(v = 60, lty = 2, col = 'darkgrey')
  abline(v = 152, lty = 2, col = 'darkgrey')
  abline(v = 244, lty = 2, col = 'darkgrey')
  abline(v = 335, lty = 2, col = 'darkgrey')
  for(s in unique(occ.dat$scene.id)){
    sub.occ <- occ.dat[occ.dat$scene.id == s,]
    points(sub.occ$doy[1], mean(sub.occ$temp.c), col = sub.occ$colour[1], pch = 19)
    lines(c(sub.occ$doy[1], sub.occ$doy[1]), c(min(sub.occ$temp.c), max(sub.occ$temp.c)), col = sub.occ$colour[1])
  
  }
dev.off()

## read in acquisition times, plot against surface temps
acq.times <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/final_scenes_acq_time.csv')
occ.dat <- merge(occ.dat, acq.times, by = 'scene.id', all.x = TRUE)
occ.dat$precise.time <- (occ.dat$mst.hour + (occ.dat$minute/60) + (occ.dat$second/3600))
OUT <- NULL
pdf('/Users/Avril/Desktop/acqtime_vs_temp_by_seasons.pdf', width = 6, height = 7)
plot(0, 0, xlim = c(9.75, 10.8), ylim = c(5, 70), xlab = 'Time (MST)', ylab = 'Temperature (deg C)', xaxt = 'n')
  axis(1, at = c(9.75, 10, 10.25, 10.5, 10.75))
  for(s in unique(occ.dat$scene.id)){
    time <- occ.dat[occ.dat$scene.id == s, 'precise.time'][1]
    temp <- mean(occ.dat[occ.dat$scene.id == s, 'temp.c'])
    save <- c(occ.dat[occ.dat$scene.id == s, 'season'][1], occ.dat[occ.dat$scene.id == s, 'precise.time'][1], 
              temp, occ.dat[occ.dat$scene.id == s, 'colour'][1])
    OUT <- rbind(OUT, save)
    points(time, temp, pch = 19, col = occ.dat[occ.dat$scene.id == s, 'colour'][1])
  }
  OUT <- as.data.frame(OUT)
  for(c in c(1:3)){
    OUT[,c] <- as.numeric(OUT[,c])
  }
  colnames(OUT) <- c('season','time','temp','col')
  for(s in unique(OUT[,1])){
    sub <- (OUT[OUT[,1] == s,])

    mod <- lm(temp ~ time, data = sub)
  
    print(summary(mod))
    
    new.x <- as.data.frame(seq(min(sub$time), max(sub$time), length.out = 50))
    colnames(new.x) <- 'time'
    pred.vals <- predict(mod, new.x, interval = 'confidence')
    pred.vals <- pred.vals[order(pred.vals[,1]),]
    ## CI polygon
    polygon(x = c(new.x$time, sort(new.x$time, decreasing = TRUE)),
            y = c(pred.vals[,2], sort(pred.vals[,3], decreasing = TRUE)), border = NA,
            col = alpha(sub$col[1], 0.4))
    lines(new.x$time, pred.vals[,1], col = sub$col[1], lwd = 2)
  }
dev.off()

## plot the other variables, too. for all cells occupied at some point in study
temp.dat <- tc.dat[, c('cell.num','greenness','wetness','brightness','temp.k','doy','scene.id','season','year')]
occ.dat <- temp.dat[temp.dat$cell.num %in% occ.cells$cell.num,]
occ.dat$temp.c <- occ.dat$temp.k - 273.15
occ.dat[occ.dat$season == 1, 'colour'] <- 'dodgerblue2'
occ.dat[occ.dat$season == 2, 'colour'] <- 'springgreen3'
occ.dat[occ.dat$season == 3, 'colour'] <- 'gold2'
occ.dat[occ.dat$season == 4, 'colour'] <- 'sienna1'

## include 6-month offset and plot with left-most day == first day of July (doy = 182)
## if it's July 1 or later, it counts towards the next year
occ.dat[occ.dat$doy >= 182, 'off.year'] <- occ.dat[occ.dat$doy >= 182, 'year'] + 1
## if it's before July 1, it counts for the current year
occ.dat[occ.dat$doy < 182, 'off.year'] <- occ.dat[occ.dat$doy < 182, 'year']
## if it's July 1, it's day 1 (doy = 182 - 181)
occ.dat[occ.dat$doy >= 182, 'off.doy'] <- occ.dat[occ.dat$doy >= 182, 'doy'] - 181
## e.g., makes Jan. 1 (doy = 1 --> doy = 185) follow Dec. 31 (doy = 184)
occ.dat[occ.dat$doy < 182, 'off.doy'] <- occ.dat[occ.dat$doy < 182, 'doy'] + 184

colours <- c(g.col, w.col, b.col, t.col)
var.names <- c('greenness','wetness','brightness','temp.k')
s.rain <- 'grey90'
w.rain <- 'grey70'

pdf('/Users/Avril/Desktop/vars_across_days_with_offset.pdf', width = 10, height = 5)
par(mar = c(5.1, 4.1, 4.1, 8.1))
k <- 1
for(c in c(2:4,10)){
  OUT <- NULL
  for(y in unique(occ.dat$off.year)){
    sub <- occ.dat[occ.dat$off.year == y,]
    for(d in sort(unique(sub$off.doy))){
      save <- c(y, d, mean(sub[sub$off.doy == d, c]), sd(sub[sub$off.doy == d, c]))
      OUT <- rbind(OUT, save)
    }
  }
  par(xpd = FALSE)
  plot(0,0, xlim = c(0,386), ylim = c(min(OUT[,3] - OUT[,4]), max(OUT[,3] + OUT[,4])), col = 'transparent',
       xlab = 'Day of year', ylab = '', main = colnames(sub)[c], xaxt = 'n')
  axis(1, at = c(1, 154, 275), labels = c('July 1','December 1','April 1'))
  polygon(x = c(154, 274, 274, 154), y = c(-10, -10, 100, 100), border = NA, col = w.rain) ## winter rainy season
  polygon(x = c(1, 62, 62, 1), y = c(-10, -10, 100, 100), border = NA, col = s.rain) ## summer rainy season

  ## mean point +/- SD lines
  # for(y in unique(OUT[,1])){
  #   points(OUT[OUT[,1] == y, 2], OUT[OUT[,1] == y, 3], pch = 19, col = colours[k], cex = 0.75)
  #   lines(OUT[OUT[,1] == y, 2], OUT[OUT[,1] == y, 3], col = colours[k])
  #   for(r in 1:nrow(OUT)){
  #     lines(c(OUT[r,2], OUT[r,2]), c((OUT[r,3]-OUT[r,4]), (OUT[r,3]+OUT[r,4])), col = colours[k])
  #   }
  # }

  for(y in unique(OUT[,1])){
    sub <- OUT[OUT[,1] == y,]
    lower <- sub[,3]-sub[,4]
    upper <- sub[,3]+sub[,4]
    if(k == 4){
      polygon(c(sub[c(1:nrow(sub)),2], sub[c(nrow(sub):1),2]), c(lower, upper[length(upper):1]),
              border = NA, col = alpha(colours[k], 0.4))
    } else{
      polygon(c(sub[c(1:nrow(sub)),2], sub[c(nrow(sub):1),2]), c(lower, upper[length(upper):1]),
              border = NA, col = alpha(colours[k], 0.2))
    }
  }
  for(y in unique(OUT[,1])){
    sub <- OUT[OUT[,1] == y,]
    lower <- sub[,3]-sub[,4]
    upper <- sub[,3]+sub[,4]
    lines(sub[,2], sub[,3], col = 'white')
    points(sub[,2], sub[,3], col = 'white', cex = 0.5, pch = 19)
    points(372, mean(sub[,3]), pch = 19, col = alpha(colours[k], 0.7))
    points(383, mean(sub[which(sub[,2] >= 154 & sub[,2] <= 274),3]), pch = 15, col = alpha(colours[k], 0.7)) ## pch 15 = square; winter rainy season
    points(394, mean(sub[which(sub[,2] >= 1 & sub[,2] <= 62),3]), pch = 17, col = alpha(colours[k], 0.7)) ## pch 17 = triangle; summer rainy season
  }
  par(xpd = TRUE)
  legend('topright', pch = c(17,15,19), col = alpha(colours[k], 0.7), bty = 'n',
         legend = c('Summer rainy', 'Winter rainy', 'Annual'), inset = -.2,
         title = 'Means')
  legend('bottomright', fill = c(s.rain, w.rain), legend = c('Summer rainy','Winter rainy'),
         bty = 'n', title = 'Seasons', border = NA, inset = -.2)

  k <- k+1
}
dev.off()

## single page
pdf('/Users/Avril/Desktop/vars_across_days_with_offset_singlepage.pdf', width = 6.5, height = 8.5)
par(mar = c(3.1, 4.1, 0.1, 0.1), mfrow = c(4,1))
k <- 1
txt.size <- 1.1
y.names <- c('Greenness','Wetness','Brightness','Surface temperature (deg C)')
for(c in c(2:4,10)){
  OUT <- NULL
  for(y in unique(occ.dat$off.year)){
    sub <- occ.dat[occ.dat$off.year == y,]
    for(d in sort(unique(sub$off.doy))){
      save <- c(y, d, mean(sub[sub$off.doy == d, c]), sd(sub[sub$off.doy == d, c]))
      OUT <- rbind(OUT, save)
    }
  }
  par(xpd = FALSE)
  if(k < 4){
    plot(0,0, xlim = c(0,386), ylim = c(min(OUT[,3] - OUT[,4]), max(OUT[,3] + OUT[,4])), col = 'transparent',
         xlab = '', ylab = y.names[k], main = '', xaxt = 'n', cex.lab = txt.size, cex.axis = txt.size)
  } else{
    plot(0,0, xlim = c(0,386), ylim = c(min(OUT[,3] - OUT[,4]), max(OUT[,3] + OUT[,4])), col = 'transparent',
         xlab = '', ylab = y.names[k], main = '', xaxt = 'n', cex.lab = txt.size, cex.axis = txt.size)
  }
  axis(1, at = c(1, 154, 275), labels = c('July 1','December 1','April 1'), cex.lab = txt.size, cex.axis = txt.size)
  polygon(x = c(154, 274, 274, 154), y = c(-10, -10, 100, 100), border = NA, col = w.rain)
  polygon(x = c(1, 62, 62, 1), y = c(-10, -10, 100, 100), border = NA, col = s.rain)
  
  for(y in unique(OUT[,1])){
    sub <- OUT[OUT[,1] == y,]
    lower <- sub[,3]-sub[,4]
    upper <- sub[,3]+sub[,4]
    if(k == 4){
      polygon(c(sub[c(1:nrow(sub)),2], sub[c(nrow(sub):1),2]), c(lower, upper[length(upper):1]),
              border = NA, col = alpha(colours[k], 0.4))
    } else{
      polygon(c(sub[c(1:nrow(sub)),2], sub[c(nrow(sub):1),2]), c(lower, upper[length(upper):1]),
              border = NA, col = alpha(colours[k], 0.2))
    }
  }
  for(y in unique(OUT[,1])){
    sub <- OUT[OUT[,1] == y,]
    lower <- sub[,3]-sub[,4]
    upper <- sub[,3]+sub[,4]
    lines(sub[,2], sub[,3], col = 'white')
    points(sub[,2], sub[,3], col = 'white', cex = 0.6, pch = 19)
    points(372, mean(sub[,3]), pch = 19, col = alpha(colours[k], 0.7), cex = 1.2)
    points(383, mean(sub[which(sub[,2] >= 154 & sub[,2] <= 274),3]), pch = 15, col = alpha(colours[k], 0.7)) ## pch 15 = square; winter rainy season
    points(394, mean(sub[which(sub[,2] >= 1 & sub[,2] <= 62),3]), pch = 17, col = alpha(colours[k], 0.7)) ## pch 17 = triangle; summer rainy season
  }
  # par(xpd = TRUE)
  # legend('topright', pch = c(17,15,19), col = alpha(colours[k], 0.7), bty = 'n',
  #        legend = c('Summer rainy', 'Winter rainy', 'Annual'), inset = -.2,
  #        title = 'Means')
  # legend('bottomright', fill = c(s.rain, w.rain), legend = c('Summer rainy','Winter rainy'),
  #        bty = 'n', title = 'Seasons', border = NA, inset = -.2)
  abline(v = 367.5, lty = 2, lwd = 0.75, col = 'grey20')
  
  k <- k+1
}
dev.off()

### Visualize variation in variables across active cells by year ###
pdf('/Users/Avril/Desktop/intraannual_var_in_metrics.pdf', width = 20, height = 5)
par(mfrow = c(1,4))
names(cols) <- names
for(c in c(2:5)){
  color <- cols[which(names(cols) == colnames(occ.dat)[c])]
  plot(0, 0, xlim = c(1993, 2005), ylim = c(min(occ.dat[,c]), max(occ.dat[,c])), xlab = 'Offspring year',
       ylab = colnames(occ.dat[c]), main = 'All scenes, all time points')
  abline(h = 0, lty = 2, col = 'grey20')
  for(y in sort(unique(occ.dat$off.year))){
    sub <- occ.dat[occ.dat$off.year == y,]
    points(sub$off.year, sub[,c], col = alpha(color, 0.1), pch = 19)
    lines(c(y - 0.4, y + 0.4), c(mean(sub[,c]), mean(sub[,c])), lwd = 3, col = color)
  }
}

for(c in c(2:5)){
  color <- cols[which(names(cols) == colnames(occ.dat)[c])]
  plot(0, 0, xlim = c(1993, 2005), ylim = c(min(occ.dat[,c]), max(occ.dat[,c])), xlab = 'Offspring year',
       ylab = colnames(occ.dat[c]), main = 'Scene means')
  abline(h = 0, lty = 2, col = 'grey20')
  for(y in sort(unique(occ.dat$off.year))){
    sub <- occ.dat[occ.dat$off.year == y,]
    for(s in unique(sub$scene.id)){
      points(sub$off.year[1], mean(sub[sub$scene.id == s, c]), col = alpha(color, 0.8), pch = 19)
    }
    lines(c(y - 0.4, y + 0.4), c(mean(sub[,c]), mean(sub[,c])), lwd = 3, col = color)
  }
}
dev.off()

## plot some correlations
means <- NULL
for(s in unique(occ.dat$scene.id)){
  sub <- occ.dat[occ.dat$scene.id == s,]
  save <- c(mean(sub$greenness), mean(sub$wetness), mean(sub$brightness), mean(sub$temp.c))
  means <- rbind(means, save)
}
colnames(means) <- c('greenness','wetness','brightness','temp.c')
pdf('/Users/Avril/Desktop/variable_correlations.pdf', width = 6, height = 6)
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
}
pairs(means, pch = 19, col = alpha('plum4', 0.5), cex = 0.5, 
      upper.panel = panel.cor,
      gap=0, row1attop = FALSE)
rows <- sample(c(1:nrow(occ.dat)), size = 10000, replace = FALSE)
pairs(occ.dat[rows, c(2:4,10)], pch = 19, col = alpha('plum4', 0.2), cex = 0.25, 
      upper.panel = panel.cor,
      gap=0, row1attop = FALSE)
dev.off()

# pdf('/Users/Avril/Desktop/vars_across_days_no_offset.pdf', width = 10, height = 5)
# ## showing means in this figure doesn't make sense because these means aren't calculated with
# ## the appropriate lag that was used in the data analyses
# par(mar = c(5.1, 4.1, 4.1, 8.1))
# k <- 1
# for(c in c(2:4,10)){
#   OUT <- NULL
#   for(y in unique(occ.dat$year)){
#     sub <- occ.dat[occ.dat$year == y,]
#     for(d in sort(unique(sub$doy))){
#       save <- c(y, d, mean(sub[sub$doy == d, c]), sd(sub[sub$doy == d, c]))
#       OUT <- rbind(OUT, save)
#     }
#   }
#   par(xpd = FALSE)
#   plot(0,0, xlim = c(0,366), ylim = c(min(OUT[,3] - OUT[,4]), max(OUT[,3] + OUT[,4])), col = 'transparent',
#        xlab = 'Day of year', ylab = '', main = colnames(sub)[c], xaxt = 'n')
#   axis(1, at = c(100, 200, 300))
#   polygon(x = c(0, 90, 90, 0), y = c(-10, -10, 100, 100), border = NA, col = w.rain)
#   polygon(x = c(335, 365, 365, 335), y = c(-10, -10, 100, 100), border = NA, col = w.rain)
#   polygon(x = c(182, 243, 243, 182), y = c(-10, -10, 100, 100), border = NA, col = s.rain)
#   
#   ## mean point +/- SD lines
#   # for(y in unique(OUT[,1])){
#   #   points(OUT[OUT[,1] == y, 2], OUT[OUT[,1] == y, 3], pch = 19, col = colours[k], cex = 0.75)
#   #   lines(OUT[OUT[,1] == y, 2], OUT[OUT[,1] == y, 3], col = colours[k])
#   #   for(r in 1:nrow(OUT)){
#   #     lines(c(OUT[r,2], OUT[r,2]), c((OUT[r,3]-OUT[r,4]), (OUT[r,3]+OUT[r,4])), col = colours[k])
#   #   }
#   # }
#   
#   for(y in unique(OUT[,1])){
#     sub <- OUT[OUT[,1] == y,]
#     lower <- sub[,3]-sub[,4]
#     upper <- sub[,3]+sub[,4]
#     polygon(c(sub[c(1:nrow(sub)),2], sub[c(nrow(sub):1),2]), c(lower, upper[length(upper):1]),
#             border = NA, col = alpha(colours[k], 0.2))
#   }
#   for(y in unique(OUT[,1])){
#     sub <- OUT[OUT[,1] == y,]
#     lower <- sub[,3]-sub[,4]
#     upper <- sub[,3]+sub[,4]
#     lines(sub[,2], sub[,3], col = 'white')
#     points(sub[,2], sub[,3], col = 'white', cex = 0.5, pch = 19)
#     # points(372, mean(sub[,3]), pch = 19, col = alpha(colours[k], 0.7))
#     # points(383, mean(sub[which(sub[,2] >= 335 | sub[,2] <= 90),3]), pch = 15, col = alpha(colours[k], 0.7))
#     # points(394, mean(sub[which(sub[,2] >= 182 & sub[,2] <= 243),3]), pch = 17, col = alpha(colours[k], 0.7))
#   }
#   par(xpd = TRUE)
#   # legend('topright', pch = c(17,15,19), col = alpha(colours[k], 0.7), bty = 'n',
#   #        legend = c('Summer rainy', 'Winter rainy', 'Annual'), inset = -.2,
#   #        title = 'Means')
#   legend('bottomright', fill = c(s.rain, w.rain), legend = c('Summer rainy','Winter rainy'),
#          bty = 'n', title = 'Seasons', border = NA, inset = -.2)
#   
#   k <- k+1
# }
# dev.off()
  
# ##### 2. Calculate summary statistics for occupied and adjacent cells #####
# ## 3 overlapping 1-year intervals - seasons equalized
# inds <- c(2:5) ## column indices for environmental variables
# ALL <- NULL
# for(y in unique(occ.cells$year)){      ## for each year we need stats for,
#   sub <- occ.cells[occ.cells$year == y,]
#   cells.all <- sub$cell.num                       ## list of occupied + adjacent cells
#   ## collect data 12-month offspring intervals (July - June)
#   all <- tc.dat[which(tc.dat$offspring.year == y & tc.dat$cell.num %in% cells.all),]
#   ## summarize data using all cells (occupied + adjacent)
#   t.pts <- length(unique(all$prod.id))     ## get number of remote sensing observations,
#   n.cells <- length(cells.all)
#   save.a <- c(y, 'annual', NA, t.pts, n.cells)
#   for(col in inds){                                ## for each remote sensing measure,
#     temp <- all[,c(col,ncol(all)-1,ncol(all)-2)]   ## get all values,
#     avg <- mean(temp[,1])                          ## calculate annual mean,
#     sd <- sd(temp[,1])                             ## and SD.
#     ## also calculate season-equalized mean values
#     m.1 <- mean(temp[temp$season == 1, 1])
#     m.2 <- mean(temp[temp$season == 2, 1])
#     m.3 <- mean(temp[temp$season == 3, 1])
#     m.4 <- mean(temp[temp$season == 4, 1])
#     s.avg <- mean(c(m.1,m.2,m.3,m.4), na.rm = TRUE)
#     save.a <- c(save.a, avg, sd, s.avg)        ## save the information.
#   }
#   ## collect data for summer rainy season
#   all <- tc.dat[which(tc.dat$offspring.year == y & tc.dat$cell.num %in% cells.all & tc.dat$month %in% c(7,8)),]
#   ## summarize data using all cells (occupied + adjacent)
#   t.pts <- length(unique(all$prod.id))     ## get number of remote sensing observations,
#   n.cells <- length(cells.all)
#   save.b <- c(y, 'seasonal', 1, t.pts, n.cells)
#   for(col in inds){                                ## for each remote sensing measure,
#     temp <- all[,c(col,ncol(all)-1,ncol(all)-2)]   ## get all values,
#     avg <- mean(temp[,1])                          ## calculate seasonal mean,
#     sd <- sd(temp[,1])                             ## and SD.
#     ## (obvs no seasonally equalized means for these)
#     save.b <- c(save.b, avg, sd, NA)        ## save the information.
#   }
#   ## collect data for winter rainy season
#   all <- tc.dat[which(tc.dat$offspring.year == y & tc.dat$cell.num %in% cells.all & tc.dat$month %in% c(12,1,2,3)),]
#   ## summarize data using all cells (occupied + adjacent)
#   t.pts <- length(unique(all$prod.id))     ## get number of remote sensing observations,
#   n.cells <- length(cells.all)
#   save.c <- c(y, 'seasonal', 2, t.pts, n.cells)
#   for(col in inds){                                ## for each remote sensing measure,
#     temp <- all[,c(col,ncol(all)-1,ncol(all)-2)]   ## get all values,
#     avg <- mean(temp[,1])                          ## calculate seasonal mean,
#     sd <- sd(temp[,1])                             ## and SD.
#     ## (obvs no seasonally equalized means for these)
#     save.c <- c(save.c, avg, sd, NA)        ## save the information.
#   }
#   ALL <- rbind(ALL, save.a)
#   ALL <- rbind(ALL, save.b)
#   ALL <- rbind(ALL, save.c)
# }
# ALL <- as.data.frame(ALL)
# colnames(ALL)[c(1:5)] <- c('offspring.year','temporal.scale','season','num.obs','num.cells')
# ## create a vector of column names
# indices <- colnames(tc.dat)[inds]
# c <- 6
# while(c <= ncol(ALL)){
#   for(ind in indices){
#     colnames(ALL)[c] <- paste0(ind,'.mean')
#     c <- c+1
#     colnames(ALL)[c] <- paste0(ind,'.sd')
#     c <- c+1
#     colnames(ALL)[c] <- paste0(ind,'.mean.szneq')
#     c <- c+1
#   }
# }
# 
# ALL <- ALL[order(ALL$offspring.year),]
# write.csv(ALL, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means_seasoneq.csv', row.names = FALSE)

tc.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means_seasoneq.csv')

##### 3. Read in and summarize PRISM data #####
prism.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/raw_data/23_01_13_daily_prism_data.csv', header=TRUE)
colnames(prism.dat) <- c('date','precip.mm','min.t.degC','mean.t.degC','max.t.degC')
prism.dat$year <- as.numeric(do.call(rbind, strsplit(prism.dat$date, split='-', fixed=TRUE))[,1])
prism.dat$month <- as.numeric(do.call(rbind, strsplit(prism.dat$date, split='-', fixed=TRUE))[,2])
prism.dat$day <- as.numeric(do.call(rbind, strsplit(prism.dat$date, split='-', fixed=TRUE))[,3])
for(y in unique(prism.dat$year)){
  prism.dat[prism.dat$year == y, 'doy'] <- c(1:nrow(prism.dat[prism.dat$year == y,]))
}
## add season for season equalization
prism.dat[prism.dat$doy < 60 | prism.dat$doy >= 335, 'season'] <- 1
prism.dat[prism.dat$doy >= 60 & prism.dat$doy < 152, 'season'] <- 2
prism.dat[prism.dat$doy >= 152 & prism.dat$doy < 244, 'season'] <- 3
prism.dat[prism.dat$doy >= 244 & prism.dat$doy < 335, 'season'] <- 4

## set 'offspring.year' to indicate which months' data go with which offspring-producing-intervals.
## i.e., July - December in year t-1 contribute to offspring produced in year t.
## July-August year t-1 contribute to offspring produced in year t.
## December year t-1 through March year t comprise the winter rainy season for offspring produced in year t.
prism.dat[prism.dat$month %in% c(7:12), 'offspring.year'] <- prism.dat[prism.dat$month %in% c(7:12), 'year']+1
prism.dat[prism.dat$month %in% c(1:6), 'offspring.year'] <- prism.dat[prism.dat$month %in% c(1:6), 'year']

## only keep data for offspring produced in 1993-2005
prism.dat <- prism.dat[prism.dat$offspring.year >= 1993 & prism.dat$offspring.year <= 2005,]

pdf('/Users/Avril/Desktop/prism_temp_data.pdf', width = 8, height = 6.5)
## just for this figure
temp <- prism.dat[prism.dat$offspring.year >= 1993 & prism.dat$offspring.year <= 2005,]
plot(c(temp$doy, temp$doy, temp$doy),
     c(temp$mean.t.degC, temp$max.t.degC, temp$min.t.degC), col = 'transparent',
     xlab = 'Day of year', ylab = 'Temperature (deg C)', main = 'PRISM daily mins and maxes for all days July 1, 1992 - June 30, 2005')
  # polygon(x = c(0, 90, 90, 0), y = c(-10, -10, 40, 40), border = NA, col = w.rain)
  # polygon(x = c(335, 365, 365, 335), y = c(-10, -10, 40, 40), border = NA, col = w.rain)
  # polygon(x = c(182, 243, 243, 182), y = c(-10, -10, 40, 40), border = NA, col = s.rain)
  abline(v = 60, lty = 2, col = 'darkgrey')
  abline(v = 152, lty = 2, col = 'darkgrey')
  abline(v = 244, lty = 2, col = 'darkgrey')
  abline(v = 335, lty = 2, col = 'darkgrey')
  points(temp[temp$season == 1, 'doy'], temp[temp$season == 1, 'min.t.degC'],
         pch = 19, cex = 0.5, col = 'dodgerblue4')
  points(temp[temp$season == 2, 'doy'], temp[temp$season == 2, 'min.t.degC'],
         pch = 19, cex = 0.5, col = 'springgreen4')
  points(temp[temp$season == 3, 'doy'], temp[temp$season == 3, 'min.t.degC'],
         pch = 19, cex = 0.5, col = 'gold3')
  points(temp[temp$season == 4, 'doy'], temp[temp$season == 4, 'min.t.degC'],
         pch = 19, cex = 0.5, col = 'sienna')

  points(temp[temp$season == 1, 'doy'], temp[temp$season == 1, 'max.t.degC'],
         pch = 19, cex = 0.5, col = 'dodgerblue')
  points(temp[temp$season == 2, 'doy'], temp[temp$season == 2, 'max.t.degC'],
         pch = 19, cex = 0.5, col = 'springgreen2')
  points(temp[temp$season == 3, 'doy'], temp[temp$season == 3, 'max.t.degC'],
         pch = 19, cex = 0.5, col = 'gold1')
  points(temp[temp$season == 4, 'doy'], temp[temp$season == 4, 'max.t.degC'],
         pch = 19, cex = 0.5, col = 'sienna2')

  points(temp[temp$season == 1, 'doy'], temp[temp$season == 1, 'mean.t.degC'],
         pch = 19, cex = 0.5, col = 'dodgerblue2')
  points(temp[temp$season == 2, 'doy'], temp[temp$season == 2, 'mean.t.degC'],
         pch = 19, cex = 0.5, col = spr)
  points(temp[temp$season == 3, 'doy'], temp[temp$season == 3, 'mean.t.degC'],
         pch = 19, cex = 0.5, col = sum)
  points(temp[temp$season == 4, 'doy'], temp[temp$season == 4, 'mean.t.degC'],
         pch = 19, cex = 0.5, col = aut)
dev.off()

pdf('/Users/Avril/Desktop/prism_precip_data.pdf', width = 8, height = 6.5)
## just for this figure
temp <- prism.dat[prism.dat$offspring.year >= 1993 & prism.dat$offspring.year <= 2005,]
plot(temp$doy, temp$precip.mm, col = 'transparent',
     xlab = 'Day of year', ylab = 'Precipitation (mm)', main = 'PRISM daily precipitation for all days July 1, 1992 - June 30, 2005')
  # polygon(x = c(0, 90, 90, 0), y = c(-10, -10, 40, 40), border = NA, col = w.rain)
  # polygon(x = c(335, 365, 365, 335), y = c(-10, -10, 40, 40), border = NA, col = w.rain)
  # polygon(x = c(182, 243, 243, 182), y = c(-10, -10, 40, 40), border = NA, col = s.rain)
  abline(v = 60, lty = 2, col = 'darkgrey')
  abline(v = 152, lty = 2, col = 'darkgrey')
  abline(v = 244, lty = 2, col = 'darkgrey')
  abline(v = 335, lty = 2, col = 'darkgrey')
  points(temp[temp$season == 1, 'doy'], temp[temp$season == 1, 'precip.mm'],
         pch = 19, cex = 0.5, col = 'dodgerblue2')
  points(temp[temp$season == 2, 'doy'], temp[temp$season == 2, 'precip.mm'],
         pch = 19, cex = 0.5, col = spr)
  points(temp[temp$season == 3, 'doy'], temp[temp$season == 3, 'precip.mm'],
         pch = 19, cex = 0.5, col = sum)
  points(temp[temp$season == 4, 'doy'], temp[temp$season == 4, 'precip.mm'],
         pch = 19, cex = 0.5, col = aut)
dev.off()  

# ALL <- NULL
# for(y in unique(prism.dat$offspring.year)){      ## for each year we need stats for,
#   sub <- prism.dat[prism.dat$offspring.year == y,]
#   ## save annual data
#   p.precip <- sum(sub$precip.mm)
#   p.mean.t.degC <- mean(sub$mean.t.degC)
#   p.mean.min.t.degC <- mean(sub$min.t.degC)
#   p.mean.max.t.degC <- mean(sub$max.t.degC)
#   save.a <- c(y, 'annual', NA, p.precip, p.mean.t.degC, p.mean.min.t.degC, p.mean.max.t.degC)        ## save the information.
#   ALL <- rbind(ALL, save.a)
#   ## save summer rainy season data
#   sub1 <- sub[sub$month %in% c(7,8),]
#   p.precip <- sum(sub1$precip.mm)
#   p.mean.t.degC <- mean(sub1$mean.t.degC)
#   p.mean.min.t.degC <- mean(sub1$min.t.degC)
#   p.mean.max.t.degC <- mean(sub1$max.t.degC)
#   save.a <- c(y, 'seasonal', 1, p.precip, p.mean.t.degC, p.mean.min.t.degC, p.mean.max.t.degC)        ## save the information.
#   ALL <- rbind(ALL, save.a)
#   ## save winter rainy season data
#   sub1 <- sub[sub$month %in% c(12,1,2,3),]
#   p.precip <- sum(sub1$precip.mm)
#   p.mean.t.degC <- mean(sub1$mean.t.degC)
#   p.mean.min.t.degC <- mean(sub1$min.t.degC)
#   p.mean.max.t.degC <- mean(sub1$max.t.degC)
#   save.a <- c(y, 'seasonal', 2, p.precip, p.mean.t.degC, p.mean.min.t.degC, p.mean.max.t.degC)        ## save the information.
#   ALL <- rbind(ALL, save.a)
# }
# ALL <- as.data.frame(ALL)
# colnames(ALL) <- c('offspring.year','temporal.scale','season','total.precip.mm','mean.t.degC','mean.min.t.degC','mean.max.t.degC')
# write.csv(ALL, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/prism_annual_and_seasonal_means.csv', row.names = FALSE)

prism.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/prism_annual_and_seasonal_means.csv')

##### 4. Compare PRISM and season-equalized vs. unequalized RS data #####
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual',]
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
comb <- merge(sub.tc, sub.prism, by = 'offspring.year')

pairs(comb[,c(6,8,9,11,12,14,15,17,20,21,22,23)], pch = 19, cex = 0.6, col = 'dodgerblue3')
## nothing is really correlated with anything, so just sticking with seasonally equalized means for the remote sensing data
## but double-check with the temperature ones
pairs(comb[,c(15,17,20:23)], pch = 19, col = 'dodgerblue3')
## yeah, nothing going on.

# ##### 5. Compare landscape-level variation within years against across-year variation #####
# cells <- unique(occ.cells$cell.num) ## get list of cells analyzed in at least one year (i.e., potential active landscape)
# ## read in raw remote sensing predictor variables
# tc.fn <- 'tc_manual_cloudcheck' ## for TC data -- tc_manual_cloudcheck
# tc.dat <- read.csv(paste0('/Users/Avril/Documents/krat_remote_sensing/C2L2_tc_output_tables/C2L2_',tc.fn,'.csv'))
# ## keep TC and temp.k
# tc.dat <- tc.dat[,c(1:5,28:35)]
# ## get year and month #s
# tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,1]
# tc.dat$year <- as.numeric(tc.dat$year)
# tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-', fixed=TRUE))[,2]
# tc.dat$month <- as.numeric(tc.dat$month)
# tc.dat <- tc.dat[tc.dat$year >= 1993 & tc.dat$year <= 2005,]
# 
# ## scale and center values for TC and temp.k
# tc.dat$greenness <- scale(tc.dat$greenness, center = TRUE, scale = TRUE)
# tc.dat$wetness <- scale(tc.dat$wetness, center = TRUE, scale = TRUE)
# tc.dat$brightness <- scale(tc.dat$brightness, center = TRUE, scale = TRUE)
# 
# ## define seasons by expected precip (1=July-August, 2=Dec-March, 0=outside these months)
# tc.dat[which(tc.dat$month %in% c(7,8)), 'weather.szn'] <- 1
# tc.dat[which(tc.dat$month %in% c(12,1,2,3)), 'weather.szn'] <- 2
# 
# ## add season to tc.dat for season equalization
# tc.dat[tc.dat$doy < 60 | tc.dat$doy >= 335, 'season'] <- 1
# tc.dat[tc.dat$doy >= 60 & tc.dat$doy < 152, 'season'] <- 2
# tc.dat[tc.dat$doy >= 152 & tc.dat$doy < 244, 'season'] <- 3
# tc.dat[tc.dat$doy >= 244 & tc.dat$doy < 335, 'season'] <- 4
# 
# ## subset data to list of active cells
# tc.dat <- tc.dat[tc.dat$cell.num %in% cells,]
# 
# ## calculate CVs? 
# ## for each year, calculate CVs (landscape SD / landscape mean) -- not season-equalized bc idk how to combine SDs
# OUT <- NULL
# for(y in unique(tc.dat$year)){
#   sub <- tc.dat[tc.dat$year == y,]
#   mn <- mean(sub$greenness)
#   sd <- sd(sub$greenness)
#   cv <- sd(sub$greenness)/mean(sub$greenness)
#   save <- c(y, mn, sd, cv)
#   OUT <- rbind(OUT, save)
# }
# year.cvs <- as.data.frame(OUT)
# colnames(year.cvs) <- c('year','mean','sd','cv')
# plot(year.cvs$year, year.cvs$mean)
