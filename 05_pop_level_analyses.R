library(sp)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
library(lmtest)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(TeachingDemos)
source('/Users/Avril/Documents/krat_remote_sensing/krat_remote_sensing_scripts/03b_c2l2readMeta.R')
`%notin%` <- Negate(`%in%`)


##### 1. Read in data #####
## Read in 1 cropped scene to get background image and cell #s (rows, columns)
ls5.stack <- brick('/Users/Avril/Documents/krat_remote_sensing/C2L2_cropped_landsat45tm_scenes/LT05_L2SP_035038_20020622_20200905_02_T1_CROPPED.grd')
raster::plotRGB(ls5.stack, r=3, g=2, b=1, scale=ls5.stack@data@max[c(3,2,1)], margins=FALSE)

## Read in occupied cell data 
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

## set extent for all analyses
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750
ext <- extent(lo.x, hi.x, lo.y, hi.y)

## Read in remote sensing data (calculated in summarize_cells_with_mounds.R)
## 6-month intervals
# all.cells.6mo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_6monthinterval_means.csv')
# occ.cells.6mo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_6monthinterval_means.csv')

## 12-month intervals
# all.cells.12mo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means.csv')
# occ.cells.12mo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_12monthinterval_means.csv')

## 12-month intervals - season-equalized
all.cells.12mo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means_seasoneq.csv')
occ.cells.12mo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_cells_only_12monthinterval_means_seasoneq.csv')

## read in demographic data
demo <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_demo_data.csv')
# all.dat.6mo <- merge(all.cells.6mo, demo, by='year', all.x = TRUE)
# occ.dat.6mo <- merge(occ.cells.6mo, demo, by='year', all.x = TRUE)
all.dat.12mo <- merge(all.cells.12mo, demo, by='year', all.x = TRUE)
occ.dat.12mo <- merge(occ.cells.12mo, demo, by='year', all.x = TRUE)

##### 2. Plot correlations for each time interval separately #####
# inds <- gsub('.mean','',colnames(all.dat)[grep('mean', colnames(all.dat))])
## for now, since we're scaling down, just keep TC + temp.k
inds <- c('greenness','wetness','brightness','temp.k')

## MEANS
## 6-month intervals - all cells
# pdf('/Users/Avril/Desktop/6months_allcells.pdf', width=5, height=5)
# for(i in inds){
#   c <- grep(i, colnames(all.dat.6mo))
#   sub <- all.dat.6mo[,c(1:4,47:53,c[1])]
#   for(r in c(7,10,11)){
#     for(n in order(unique(sub$interval))){
#       ## col 10 == num.off.perf; col 11 = num.surv.perf; col 7 = avg. age at death
#       dep <- r
#       plot(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='darkorchid3',
#            xlab=colnames(sub)[12], ylab=colnames(sub)[dep], main=paste0(colnames(sub)[r],' - ',n))
#       mod <- lm(sub[sub$interval == n, dep] ~ sub[sub$interval == n, 12])
#       if(summary(mod)$coefficients[,4][2] <= 0.05){
#         points(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='springgreen3')
#         abline(mod, lty=2)
#         legend('topright', legend=paste0('p = ', round(summary(mod)$coefficients[,4][2], digits = 4),
#                                          '\nr2 = ', round(summary(mod)$r.squared, digits = 4)))
#       }
#     }
#   }
# }
# dev.off()

## 12-month intervals - all cells
pdf('/Users/Avril/Desktop/12months_allcells.pdf', width=5, height=5)
for(i in inds){
  c <- grep(i, colnames(all.dat.12mo))
  sub <- all.dat.12mo[,c(1:4,19,22,23,c[3])] ## c[1] == without season equalization
  for(r in c(7,10,11)){
    for(n in order(unique(sub$interval))){
      ## col 10 == num.off.perf; col 11 = num.surv.perf; col 7 = avg. age at death
      dep <- r
      plot(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='darkorchid3',
           xlab=colnames(sub)[12], ylab=colnames(sub)[dep], main=paste0(colnames(sub)[r],' - ',n))
      mod <- lm(sub[sub$interval == n, dep] ~ sub[sub$interval == n, 12])
      if(summary(mod)$coefficients[,4][2] <= 0.05){
        points(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='springgreen3')
        abline(mod, lty=2)
        legend('topright', legend=paste0('p = ', round(summary(mod)$coefficients[,4][2], digits = 4),
                                         '\nr2 = ', round(summary(mod)$r.squared, digits = 4)))
      }
    }
  }
}
dev.off()

# ## SDs - just to check
# ## >>> nope, this is nothing
# ## 6-month intervals - all cells
# pdf('/Users/Avril/Desktop/6months_allcells_SD.pdf', width=5, height=5)
# for(i in inds){
#   c <- grep(i, colnames(all.dat.6mo))
#   sub <- all.dat.6mo[,c(1:4,47:53,c[2])]
#   for(r in c(7,10,11)){
#     for(n in order(unique(sub$interval))){
#       ## col 10 == num.off.perf; col 11 = num.surv.perf; col 7 = avg. age at death
#       dep <- r
#       plot(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='darkorchid3',
#            xlab=colnames(sub)[12], ylab=colnames(sub)[dep], main=paste0(colnames(sub)[r],' - ',n))
#       mod <- lm(sub[sub$interval == n, dep] ~ sub[sub$interval == n, 12])
#       if(summary(mod)$coefficients[,4][2] <= 0.05){
#         points(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='springgreen3')
#         abline(mod, lty=2)
#         legend('topright', legend=paste0('p = ', round(summary(mod)$coefficients[,4][2], digits = 4),
#                                          '\nr2 = ', round(summary(mod)$r.squared, digits = 4)))
#       }
#     }
#   }
# }
# dev.off()
# 
# ## 12-month intervals - all cells
# pdf('/Users/Avril/Desktop/12months_allcells_SD.pdf', width=5, height=5)
# for(i in inds){
#   c <- grep(i, colnames(all.dat.12mo))
#   sub <- all.dat.12mo[,c(1:4,47:53,c[2])]
#   for(r in c(7,10,11)){
#     for(n in order(unique(sub$interval))){
#       ## col 10 == num.off.perf; col 11 = num.surv.perf; col 7 = avg. age at death
#       dep <- r
#       plot(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='darkorchid3',
#            xlab=colnames(sub)[12], ylab=colnames(sub)[dep], main=paste0(colnames(sub)[r],' - ',n))
#       mod <- lm(sub[sub$interval == n, dep] ~ sub[sub$interval == n, 12])
#       if(summary(mod)$coefficients[,4][2] <= 0.05){
#         points(sub[sub$interval == n, 12], sub[sub$interval == n, dep], pch=19, col='springgreen3')
#         abline(mod, lty=2)
#         legend('topright', legend=paste0('p = ', round(summary(mod)$coefficients[,4][2], digits = 4),
#                                          '\nr2 = ', round(summary(mod)$r.squared, digits = 4)))
#       }
#     }
#   }
# }
# dev.off()

##### 3. LMs and LRTs #####
## just trying some stuff : backwards stepwise on models that were promising
## average age at death wasn't significant in any preliminary correlation checks, so chucking it for now
## number surviving offspring
sub <- all.dat.12mo[all.dat.12mo$interval == 2,]
mod <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod) ## NS
mod <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod) ## NS
mod <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod) ## NS
mod <- lm(sub$num.surv.perf ~ sub$temp.k.mean.szneq)
summary(mod) ## NS

sub <- all.dat.12mo[all.dat.12mo$interval == 3,]
mod <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod) ## NS
mod <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$greenness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod) ## sig
mod <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod) ## sig - both preds significant

# sub <- all.dat.6mo[all.dat.6mo$interval == 3,]
# mod <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod) ## NS (p = 0.059)
# mod <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod) ## NS
# mod <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod) ## sig - both preds sig

# sub <- all.dat.6mo[all.dat.6mo$interval == 4,]
# mod <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod) ## NS
# mod <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod) ## NS
# mod <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod) ## sig - only 1 pred sig
# mod <- lm(sub$num.surv.perf ~ sub$temp.k.mean.szneq)
# summary(mod) ## NS
## for number of surviving offspring per female, temp.k and wetness both appear to be important in year t, and using 12-month intervals best captures this? p is lowest anyway 

## now try number of offspring
# sub <- all.dat.6mo[all.dat.6mo$interval == 4,]
# mod <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod)
## none of the interval regressions (for 6- and 12-) were significant, and no predictors were ever significant in these models
## let's move forward with just the 12-month interval data. fewer intervals to test, and the results between 6- and 12- appear to be concordant for all response variables anyway.

# ## number surviving offspring per female
# ## interval 1
# sub <- all.dat.12mo[all.dat.12mo$interval == 1,]
# mod.a <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.a)
# mod.b <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq)
# summary(mod.b)
# mod.c <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$brightness.mean.szneq)
# summary(mod.c)
# mod.d <- lm(sub$num.surv.perf ~ sub$brightness.mean.szneq)
# lrtest(mod.a, mod.b, mod.c, mod.d) ## all NS different, all NS
# ## interval 2
# sub <- all.dat.12mo[all.dat.12mo$interval == 2,]
# mod.a <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.a)
# mod.b <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.b)
# lrtest(mod.a, mod.b) ## NS
# mod.c <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.c)
# lrtest(mod.b, mod.c) ## NS
# mod.d <- lm(sub$num.surv.perf ~ sub$temp.k.mean.szneq)
# summary(mod.d)
# lrtest(mod.c, mod.d) ## sig diff models, simple is worse, but neither model is significant
# interval 3
sub <- all.dat.12mo[all.dat.12mo$interval == 3,]
mod.a <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod.a) ## NS - p = 0.059
mod.b <- lm(sub$num.surv.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod.b) ## sig
lrtest(mod.a, mod.b) ## NS
mod.c <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod.c) ## sig
lrtest(mod.b, mod.c) ## NS
mod.d <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq)
summary(mod.d) ## NS!!!
lrtest(mod.c, mod.d) ## sig and mod.c is better, mod.d is NS
pdf('/Users/Avril/Desktop/two_preds_for_interval3.pdf', width = 5, height = 5)
plot(sub$wetness.mean.szneq, sub$num.surv.perf, pch = 19, main = 'Interval 3', xlab = 'wetness', ylab = 'Number surviving offspring per female')
  mod <- lm(sub$num.surv.perf ~ sub$wetness.mean.szneq)
  abline(mod, lty = 2)
  legend('topleft', legend=paste0('p = ', round(summary(mod)$coefficients[,4][2], digits = 4),
                                   '\nr2 = ', round(summary(mod)$r.squared, digits = 4)), bty = 'n', inset = 0.02)
plot(sub$temp.k.mean.szneq, sub$num.surv.perf, pch = 19, main = 'Interval 3', xlab = 'temp.k', ylab = 'Number surviving offspring per female')  
  mod <- lm(sub$num.surv.perf ~ sub$temp.k.mean.szneq)  
  abline(mod, lty = 2)
  legend('topleft', legend=paste0('p = ', round(summary(mod)$coefficients[,4][2], digits = 4),
                                   '\nr2 = ', round(summary(mod)$r.squared, digits = 4)), bty = 'n', inset = 0.02)
dev.off()

##### !!! check assumptions #####
shapiro.test(mod.c$residuals) ## NS
plot(mod.c, 1)
plot(mod.c, 2)
plot(mod.c, 3)
plot(mod.c, 4)
## remove observation #12 and see if it still holds
temp <- sub[-12,]
temp.mod <- lm(temp$num.surv.perf ~ temp$wetness.mean.szneq + temp$temp.k.mean.szneq)
summary(temp.mod) ## shit, no longer significant
plot(sub$temp.k.mean.szneq, sub$num.surv.perf)
plot(temp$temp.k.mean.szneq, temp$num.surv.perf)
## remove #2 and #9 just to see how much worse it gets
temp <- sub[-c(2,9,12),]
temp.mod <- lm(temp$num.surv.perf ~ temp$wetness.mean.szneq + temp$temp.k.mean.szneq)
summary(temp.mod) ## NS
plot(sub$temp.k.mean.szneq, sub$num.surv.perf)
plot(temp$temp.k.mean.szneq, temp$num.surv.perf)

plot(mod.c, 5)

mod.l <- lm(log(sub$num.surv.perf) ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
shapiro.test(mod.l$residuals)
plot(mod.l)
    
# ## number offspring per female
# ## interval 1
# sub <- all.dat.12mo[all.dat.12mo$interval == 1,]
# mod.a <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.a)
# mod.b <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$temp.k.mean.szneq + sub$wetness.mean.szneq)
# summary(mod.b)
# lrtest(mod.a, mod.b)
# mod.c <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$brightness.mean.szneq)
# summary(mod.c)
# lrtest(mod.b, mod.c)
# mod.d <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq)
# summary(mod.d)
# ## interval 2
# sub <- all.dat.12mo[all.dat.12mo$interval == 2,]
# mod.a <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.a)
# mod.b <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq)
# summary(mod.b)
# lrtest(mod.a, mod.b) ## NS
# mod.c <- lm(sub$num.off.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.c)
# lrtest(mod.b, mod.c) ## NS
# mod.d <- lm(sub$num.off.perf ~ sub$wetness.mean.szneq)
# summary(mod.d)
# lrtest(mod.c, mod.d) ## all NS
## interval 3
sub <- all.dat.12mo[all.dat.12mo$interval == 3,]
mod.a <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod.a) ## NS
mod.b <- lm(sub$num.off.perf ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod.b) ## NS
lrtest(mod.a, mod.b) ## NS
mod.c <- lm(sub$num.off.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
summary(mod.c) ## NS
lrtest(mod.b, mod.c) ## NS (p = 0.069)
mod.d <- lm(sub$num.off.perf ~ sub$temp.k.mean.szneq)
summary(mod.d) ## NS (but p = 0.107)
lrtest(mod.c, mod.d) ## sig - wetness + temp.k better than just temp.k


# ## average age at death
# ## interval 1
# sub <- all.dat.12mo[all.dat.12mo$interval == 1,]
# mod.a <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.a) ## NS
# mod.b <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$temp.k.mean.szneq + sub$brightness.mean.szneq)
# summary(mod.b) ## NS
# lrtest(mod.a, mod.b)
# mod.c <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$brightness.mean.szneq)
# summary(mod.c) ## NS
# lrtest(mod.b, mod.c)
# mod.d <- lm(sub$avg.aad ~ sub$brightness.mean.szneq)
# summary(mod.d) ## NS, but p = 0.067
# ## interval 2
# sub <- all.dat.12mo[all.dat.12mo$interval == 2,]
# mod.a <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.a)
# mod.b <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.b)
# lrtest(mod.a, mod.b) ## NS
# mod.c <- lm(sub$avg.aad ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.c)
# lrtest(mod.b, mod.c) ## NS
# mod.d <- lm(sub$avg.aad ~ sub$temp.k.mean.szneq)
# summary(mod.d)
# lrtest(mod.c, mod.d) ## all NS
# ## interval 3
# sub <- all.dat.12mo[all.dat.12mo$interval == 3,]
# mod.a <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$brightness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.a) ## NS
# mod.b <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$wetness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.b) ## NS
# lrtest(mod.a, mod.b) ## NS
# mod.c <- lm(sub$avg.aad ~ sub$greenness.mean.szneq + sub$temp.k.mean.szneq)
# summary(mod.c) ## NS
# lrtest(mod.b, mod.c) ## NS
# mod.d <- lm(sub$avg.aad ~ sub$temp.k.mean.szneq)
# summary(mod.d) ## NS (but p = 0.075)
# lrtest(mod.c, mod.d) ## all NS

## lots of overlap in how these models worked out, check correlations among response variables
pairs(all.dat.12mo[,c(19,22,23)])

## try a 3D plot for sub$num.surv.perf ~ sub$wetness.mean.szneq + sub$temp.k.mean.szneq for interval 3
# library(rgl)
# sub <- all.dat.12mo[all.dat.12mo$interval == 3,]
# plot3d(x = sub$temp.k.mean.szneq, y = sub$wetness.mean.szneq, z = sub$num.surv.perf, col = 'blue', size = 10)

### compare annual means with seasonal representation
seasons <- read.table('/Users/Avril/Desktop/yearly_season_totals.txt', header = TRUE)
temp <- all.cells.12mo[all.cells.12mo$interval == 3, c('year','greenness.mean.szneq','wetness.mean.szneq','brightness.mean.szneq','temp.k.mean.szneq')]
seasons <- merge(seasons, temp, by = 'year')

pdf('/Users/Avril/Desktop/seasons_vs_metrics.pdf', width = 5, height = 5)
for(s in c(2:5)){
  for(i in c(6:9)){
    plot(seasons[,s], seasons[,i], pch = 19, xlab = colnames(seasons)[s], ylab = colnames(seasons)[i], xlim = c(0,7))
      mod <- lm(seasons[,i] ~ seasons[,s])
      if(summary(mod)$coefficients[,4][2] < 0.05){
        points(seasons[,s], seasons[,i], pch=19, col='red')
      }
  }
}
dev.off()
