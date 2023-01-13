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

g.col <- 'forestgreen'
w.col <- 'deepskyblue2'
b.col <- 'tan4'
t.col <- 'chocolate2'
pp.col <- 'turquoise4'
pt.col <- 'tomato2'
cols <- c(g.col, w.col, b.col, t.col, pp.col, pt.col)
plot(0,0, xlim = c(0,7), ylim = c(0,1), col = 'transparent')
  for(c in 1:6){
    polygon(c(c-1,c,c,c-1), c(0,0,1,1), col = cols[c], border = NA)
  }

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

## scale and center TC
tc.dat$z.g <- scale(tc.dat$greenness, scale=TRUE, center=TRUE)
tc.dat$z.b <- scale(tc.dat$brightness, scale=TRUE, center=TRUE)
tc.dat$z.w <- scale(tc.dat$wetness, scale=TRUE, center=TRUE)

## get list of mound:cell associations
mounds.cells.only <- mc.key[,c('database.name','cell.num')]
mounds.cells.only <- mounds.cells.only[!duplicated(mounds.cells.only),]

##### Read in PRISM data #####
prism.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/raw_data/23_01_11_monthly_prism_data.csv', header=TRUE)
for(c in c(3:5)){
  prism.dat[,c] <- (prism.dat[,c] - 32) * 5/9
}
colnames(prism.dat) <- c('date','precip.mm','min.t.degC','mean.t.degC','max.t.degC')
prism.dat$year <- as.numeric(do.call(rbind, strsplit(prism.dat$date, split='-', fixed=TRUE))[,1])
prism.dat$month <- as.numeric(do.call(rbind, strsplit(prism.dat$date, split='-', fixed=TRUE))[,2])


##### Define seasons #####
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,1]
tc.dat$year <- as.numeric(tc.dat$year)
tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,2]
tc.dat$month <- as.numeric(tc.dat$month)
## define surviving offspring lag:
## for females that reproduce in year t (offspring produced in year t), the response variable is the proportion of 
## offspring born in year t still surviving in year t+1, and the predictor variables span July year t - June year t+1
tc.dat[which(tc.dat$month %in% c(7:12)), 'half.year.lag'] <- tc.dat[which(tc.dat$month %in% c(7:12)), 'year'] + 1
tc.dat[which(tc.dat$month %in% c(1:6)), 'half.year.lag'] <- tc.dat[which(tc.dat$month %in% c(1:6)), 'year']
prism.dat[which(prism.dat$month %in% c(7:12)), 'half.year.lag'] <- prism.dat[which(prism.dat$month %in% c(7:12)), 'year']
prism.dat[which(prism.dat$month %in% c(1:6)), 'half.year.lag'] <- prism.dat[which(prism.dat$month %in% c(1:6)), 'year']-1

## define seasons by expected precip (1=July-August, 2=Dec-March, 0=outside these months)
tc.dat[which(tc.dat$month %in% c(7,8)), 'weather.szn'] <- 1
tc.dat[which(tc.dat$month %in% c(12,1,2,3)), 'weather.szn'] <- 2
tc.dat[which(tc.dat$month %in% c(4,5,6,9,10,11)), 'weather.szn'] <- 0
prism.dat[which(prism.dat$month %in% c(7,8)), 'weather.szn'] <- 1
prism.dat[which(prism.dat$month %in% c(12,1,2,3)), 'weather.szn'] <- 2
prism.dat[which(prism.dat$month %in% c(4,5,6,9,10,11)), 'weather.szn'] <- 0


##### Get number of offspring recorded per female per year and assign to cells in the raster #####
## >> relies on pedigree information: where mom was recorded in a year and how many offspring
## >> were recorded for her in that year 
## 
## >> pop.dat              == Peter's data
## >> j.dat                == J's data
## >> tc.dat               == environmental data
## >> mounds.cells.only    == mound:cell assignments (1:1)
OUT <- NULL
for(f in unique(pop.dat[pop.dat$sex == 'Female', 'id'])){           ## for each female,
  sub.pop <- pop.dat[pop.dat$id == f & pop.dat$offspring == 0,]     ## get her location for all years that she's an adult.
  for(y in unique(sub.pop$year)){                                   ## for each year she's alive,
    off <- j.dat[j.dat$momid == f & j.dat$birthyear == y,]          ## collect her offspring's data,
    num.off <- length(unique(off$id))                               ## calculate number and number surviving.
    num.surv <- length(unique(off[off$deathyear - off$birthyear > 1, 'id']))
    ## save info for female + year: ID, mound, num.off, num.surv
    save <- c(f, y, sub.pop[sub.pop$year == y, 'terr'], num.off, num.surv)
    OUT <- rbind(OUT, save)
  }
}
fem.ann <- as.data.frame(OUT)
colnames(fem.ann) <- c('id','year','database.name','num.off','num.surv')
fem.ann$num.off <- as.numeric(fem.ann$num.off)
fem.ann$num.surv <- as.numeric(fem.ann$num.surv)
fem.ann$prop.surv <- fem.ann$num.surv/fem.ann$num.off
fem.ann <- merge(fem.ann, mounds.cells.only, by = 'database.name', all.x = TRUE)  ## add cell # information
fem.ann <- fem.ann[!is.na(fem.ann$cell.num),]              ## not all database names were able to be assigned to cells in the raster
fem.ann <- fem.ann[fem.ann$year <= 2005,]

##### Add environmental summary stats to each fitness observation #####
# OUT <- NULL
# for(r in 1:nrow(fem.ann)){  ## for each observation,
#   r/nrow(fem.ann))
#   cn <- fem.ann$cell.num[r]
#   ## identify 8 adjacent (surrounding) cells ; 92 rows, 70 columns in raster;
#   ## (math confirmed by manually plotting)
#   cells <- c(cn, (cn-71), (cn-70), (cn-69), (cn-1), (cn+1), (cn+69), (cn+70), (cn+71))
#   sub.tc <- tc.dat[tc.dat$half.year.lag == fem.ann$year[r] & tc.dat$cell.num %in% cells,
#                     c('cell.num','z.g','z.b','z.w','temp.k','year','month','half.year.lag','weather.szn')]
#   sub.prism <- prism.dat[prism.dat$half.year.lag == fem.ann$year[r],]
#   ## save annual information
#   g <- mean(sub.tc$z.g)
#   b <- mean(sub.tc$z.b)
#   w <- mean(sub.tc$z.w)
#   t <- mean(sub.tc$temp.k)
#   pp <- sum(sub.prism$precip.mm)
#   pt <- mean(sub.prism$mean.t.degC)
#   g.sd <- sd(sub.tc$z.g)
#   b.sd <- sd(sub.tc$z.b)
#   w.sd <- sd(sub.tc$z.w)
#   t.sd <- sd(sub.tc$temp.k)
#   save <- as.numeric(c(unlist(fem.ann[r,c(2:6)]), '1', NA, g, b, w, t, pp, pt, g.sd, b.sd, w.sd, t.sd))
#   OUT <- rbind(OUT, save)
#   ## then save season information
#   for(s in c(1,2)){                                 ## for each weather-based season,
#     temp <- sub.tc[sub.tc$weather.szn == s,]        ## subset the data,
#     temp1 <- sub.prism[sub.prism$weather.szn == s,]
#     g <- mean(temp$z.g)                             ## and collect TC + temp.k means + SDs
#     b <- mean(temp$z.b)
#     w <- mean(temp$z.w)
#     t <- mean(temp$temp.k)
#     pp <- sum(temp1$precip.mm)
#     pt <- mean(temp1$mean.t.degC)
#     g.sd <- sd(temp$z.g)
#     b.sd <- sd(temp$z.b)
#     w.sd <- sd(temp$z.w)
#     t.sd <- sd(temp$temp.k)
#     save <- as.numeric(c(unlist(fem.ann[r,c(2:6)]), '2', s, g, b, w, t, pp, pt, g.sd, b.sd, w.sd, t.sd))
#     OUT <- rbind(OUT, save)
#   }
# }
# env.fem.ann <- as.data.frame(OUT)
# colnames(env.fem.ann) <- c('id','year','num.off','num.surv','prop.surv','type','season','mean.g',
#                            'mean.b','mean.w','mean.tempk', 'precip.mm', 'mean.t.degC',
#                            'sd.g','sd.b','sd.w','sd.tempk')
# write.csv(env.fem.ann, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/indiv_propsurv_x_env_bycells.csv', row.names = FALSE)
env.fem.ann <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/indiv_propsurv_x_env_bycells.csv')
env.fem.ann <- env.fem.ann[env.fem.ann$num.off > 0,] ## for # surviving offspring stuff, only analyze observations where at least
                                                     ## 1 offspring was produced in year t

##### Plot relationships between environmental summary statistics and individual annual female fitness #####
types <- c('annual','weather-based')
pdf('/Users/Avril/Desktop/env_v_propsurv_bycells.pdf', width = 5, height = 5)
for(t in unique(env.fem.ann$type)){
  sub <- env.fem.ann[env.fem.ann$type == t,]
  if(t == 1){
    plot(sub$mean.g, sub$prop.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Proportion offspring surviving',
         main = paste0(types[t]))
    plot(sub$mean.b, sub$prop.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Proportion offspring surviving',
         main = paste0(types[t]))
    plot(sub$mean.w, sub$prop.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Proportion offspring surviving',
         main = paste0(types[t]))
    plot(sub$mean.tempk, sub$prop.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Proportion offspring surviving',
         main = paste0(types[t]))
  } else{
    for(s in c(1,2)){
      sub1 <- sub[sub$season == s,]
      plot(sub1$mean.g, sub1$prop.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Proportion offspring surviving',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.b, sub1$prop.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Proportion offspring surviving',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.w, sub1$prop.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Proportion offspring surviving',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.tempk, sub1$prop.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Proportion offspring surviving',
           main = paste0(types[t],' - ',s))
    }
  }
}
dev.off()

types <- c('annual','weather-based')
pdf('/Users/Avril/Desktop/env_v_numsurv_bycells.pdf', width = 5, height = 5)
for(t in unique(env.fem.ann$type)){
  sub <- env.fem.ann[env.fem.ann$type == t,]
  if(t == 1){
    plot(sub$mean.g, sub$num.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Number of offspring surviving',
         main = paste0(types[t]))
    plot(sub$mean.b, sub$num.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Number of offspring surviving',
         main = paste0(types[t]))
    plot(sub$mean.w, sub$num.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Number of offspring surviving',
         main = paste0(types[t]))
    plot(sub$mean.tempk, sub$num.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Number of offspring surviving',
         main = paste0(types[t]))
  } else{
    for(s in c(1,2)){
      sub1 <- sub[sub$season == s,]
      plot(sub1$mean.g, sub1$num.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Number of offspring surviving',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.b, sub1$num.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Number of offspring surviving',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.w, sub1$num.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Number of offspring surviving',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.tempk, sub1$num.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Number of offspring surviving',
           main = paste0(types[t],' - ',s))
    }
  }
}
dev.off()


# ##### Construct linear models and conduct LRTs (proportion offspring surviving) #####
# ##### >> Annual averages / totals #####
# sub <- env.fem.ann[env.fem.ann$type == 1,]        ## subset to annual summary stats
# 
# mod1 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk)
# summary(mod1) ## remove mean.g -- not significant tho
# 
# mod2 <- lm(sub$prop.surv ~ sub$mean.b + sub$mean.w + sub$mean.tempk)
# lrtest(mod1, mod2) ## not a significant improvement from mod1 --> mod2
# summary(mod2) ## remove mean.w
# 
# mod3 <- lm(sub$prop.surv ~ sub$mean.b + sub$mean.tempk)
# lrtest(mod2, mod3) ## not a significant improvement from mod2 --> mod3
# summary(mod3) ## remove mean.b
# 
# mod4 <- lm(sub$prop.surv ~ sub$mean.tempk)
# lrtest(mod3, mod4) ## not a significant improvement from mod3 --> mod4
# summary(mod4) ## significant, very small R2
# 
# plot(lm(sub$prop.surv ~ sub$mean.tempk))  ## assumption-checking plots looks pretty weird
# shapiro.test(mod4$residuals) ## SUPER significant
# hist(mod4$residuals)
# 
# plot(sub$mean.tempk, sub$prop.surv, pch = 19, col = alpha(t.col, 0.7))
#   abline(lm(sub$prop.surv ~ sub$mean.tempk))
#   ## 95% CI polygon
#   new.dat <- data.frame(tempk=c(sort(sub$mean.tempk[!is.na(sub$mean.tempk)]))) ## new data for prediction
#   new.vals <- predict(mod6, newdata = new.dat, interval = 'confidence')
#   # new.vals <- cbind(new.vals, new.dat)
#   new.vals <- new.vals[order(-new.vals[,1]),]
#   polygon(x = c(new.dat$tempk, sort(new.dat$tempk, decreasing = TRUE)),
#           y = c(new.vals[,2], sort(new.vals[,3], decreasing = FALSE)), border = NA,
#           col = alpha(t.col, 0.4))
#   lines(c(sort(new.dat$tempk)), new.vals[,1], lwd = 3,
#         col = t.col)
# 
# ##### >> Summer rainy season averages / totals #####
# # sink('/Users/Avril/Documents/krat_remote_sensing/22_01_lm_and_lrt_results/propsurv_summerrainy_season.txt')
# 
# sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 1,]        ## subset to seasonal summary stats
# 
# mod1 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk + sub$precip.mm + sub$mean.t.degC)
# summary(mod1) ## remove mean.tempk
# 
# 
# mod2 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$precip.mm + sub$mean.t.degC)
# summary(mod2) ## remove precip.mm
# lrtest(mod1, mod2) ## not a significant improvement from mod1 --> mod2
# 
# 
# mod3 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.t.degC)
# summary(mod3) ## remove mean.w
# lrtest(mod2, mod3) ## not a significant improvement from mod2 --> mod3
# 
# 
# mod4 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.t.degC)
# summary(mod4) ## remove mean.g
# lrtest(mod3, mod4) ## not a significant improvement from mod3 --> mod4
# 
# 
# mod5 <- lm(sub$prop.surv ~ sub$mean.b + sub$mean.t.degC)
# summary(mod5) ## remove mean.b
# lrtest(mod4, mod5) ## not a significant improvement from mod4 --> mod5
# 
# 
# mod6 <- lm(sub$prop.surv ~ sub$mean.t.degC)
# summary(mod6)
# shapiro.test(residuals(mod6)
# 
# # sink()
# 
# plot(sub$mean.t.degC, sub$prop.surv, pch = 19, col = alpha(pt.col, 0.7))
#   abline(lm(sub$prop.surv ~ sub$mean.t.degC))
#   # plot(lm(sub$prop.surv ~ sub$mean.tempk))  ## assumption-checking plots looks pretty weird
# 
# ##### >> Winter rainy season averages / totals #####
# sink('/Users/Avril/Documents/krat_remote_sensing/22_01_lm_and_lrt_results/propsurv_winterrainy_season.txt')
# 
# sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 2,]        ## subset to seasonal summary stats
# 
# mod1 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk + sub$precip.mm + sub$mean.t.degC)
# summary(mod1) ## remove mean.t.degC
# 
# 
# mod2 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk + sub$precip.mm)
# summary(mod2) ## remove precip.mm
# lrtest(mod1, mod2) ## not a significant improvement from mod1 --> mod2
# 
# 
# mod3 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk)
# summary(mod3) ## remove mean.tempk
# lrtest(mod2, mod3) ## not a significant improvement from mod2 --> mod3
# 
# 
# mod4 <- lm(sub$prop.surv ~ sub$mean.g + sub$mean.b + sub$mean.w)
# summary(mod4) ## remove mean.g
# lrtest(mod3, mod4) ## not a significant improvement from mod3 --> mod4
# 
# 
# mod5 <- lm(sub$prop.surv ~ sub$mean.b + sub$mean.w)
# summary(mod5) ## significant but very low R2
# 
# 
# sink()
# 
# plot(sub$mean.w, sub$prop.surv, col = alpha(w.col, 0.7), pch = 19)
#   abline(lm(sub$prop.surv ~ sub$mean.w))
# plot(sub$mean.b, sub$prop.surv, col = alpha(b.col, 0.7), pch = 19)
#   abline(lm(sub$prop.surv ~ sub$mean.b))


##### Construct linear models and conduct LRTs (number offspring surviving) #####
##### >> Annual averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 1,]        ## subset to annual summary stats

mod1 <- lm(sub$num.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk)
summary(mod1) ## remove mean.g

mod2 <- lm(sub$num.surv ~ sub$mean.b + sub$mean.w + sub$mean.tempk)
lrtest(mod1, mod2) ## not a significant improvement from mod1 --> mod2
summary(mod2) ## remove mean.w

mod3 <- lm(sub$num.surv ~ sub$mean.b + sub$mean.tempk)
lrtest(mod2, mod3) ## not a significant improvement from mod2 --> mod3
summary(mod3) ## remove mean.b

mod4 <- lm(sub$num.surv ~ sub$mean.tempk)
lrtest(mod3, mod4) ## not a significant improvement from mod3 --> mod4
summary(mod4) ## remove mean.b

plot(sub$mean.tempk, sub$num.surv, pch = 19, col = alpha(t.col, 0.7))
  abline(mod4)

plot(mod4)  ## assumption-checking plots looks pretty weird
shapiro.test(residuals(mod4)) ## super sig
hist(residuals(mod4))

##### >> Summer rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 1,]        ## subset to seasonal summary stats

mod1 <- lm(sub$num.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk)
summary(mod1) ## remove mean.tempk

mod2 <- lm(sub$num.surv ~ sub$mean.g + sub$mean.b + sub$mean.w)
summary(mod2) ## remove mean.g
lrtest(mod1, mod2) ## not a significant improvement from mod1 --> mod2

mod3 <- lm(sub$num.surv ~ sub$mean.b + sub$mean.w)
summary(mod3) ## remove mean.w
lrtest(mod2, mod3) ## not a significant improvement from mod2 --> mod3

mod4 <- lm(sub$num.surv ~ sub$mean.b)
summary(mod4) ## all significant
lrtest(mod3, mod4) ## not a significant improvement from mod3 --> mod4

plot(sub$mean.b, sub$num.surv, pch = 19, col = alpha(b.col, 0.7))
  abline(mod4)
  plot(mod4)
  shapiro.test(residuals(mod4))
  
##### >> Winter rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 2,]        ## subset to seasonal summary stats

mod1 <- lm(sub$num.surv ~ sub$mean.g + sub$mean.b + sub$mean.w + sub$mean.tempk)
summary(mod1) ## remove mean.tempk -- not significant tho


##### Okay, all the linear models look like shit. So just compare +/- surviving offspring #####
## Only for observations where at least 1 offspring was produced in year t

##### T-tests for +/- offspring produced #####
pdf('/Users/Avril/Desktop/numsurv_ttests.pdf', width = 5, height = 5)
##### >> Annual averages / totals #####
k <- 1
while(k == 1){
sub <- env.fem.ann[env.fem.ann$type == 1,]        ## subset to annual summary stats
sub[sub$num.surv == 0, 'off.or.no'] <- 1
sub[sub$num.surv > 0, 'off.or.no'] <- 2

wid <- 0.3
poly.alph <- 0.3
pt.alph <- 0.3
jit <- 4
pt.cex <- 0.7

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.g, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean greenness', main = 'Annual means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.g'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.g'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.g']) - sd(sub[sub$off.or.no == n, 'mean.g'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.g']) + sd(sub[sub$off.or.no == n, 'mean.g'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(g.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.g']), mean(sub[sub$off.or.no == n, 'mean.g'])),
          lwd = 6, col = g.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.g, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(g.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.g'],
         sub[sub$off.or.no == 2, 'mean.g'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.b, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean brightness', main = 'Annual means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.b'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.b'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.b']) - sd(sub[sub$off.or.no == n, 'mean.b'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.b']) + sd(sub[sub$off.or.no == n, 'mean.b'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(b.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.b']), mean(sub[sub$off.or.no == n, 'mean.b'])),
          lwd = 6, col = b.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.b, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(b.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.b'],
         sub[sub$off.or.no == 2, 'mean.b'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.w, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean wetness', main = 'Annual means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.w'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.w'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.w']) - sd(sub[sub$off.or.no == n, 'mean.w'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.w']) + sd(sub[sub$off.or.no == n, 'mean.w'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(w.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.w']), mean(sub[sub$off.or.no == n, 'mean.w'])),
          lwd = 6, col = w.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.w, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(w.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.w'],
         sub[sub$off.or.no == 2, 'mean.w'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.tempk, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean temp K', main = 'Annual means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.tempk'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.tempk'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.tempk']) - sd(sub[sub$off.or.no == n, 'mean.tempk'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.tempk']) + sd(sub[sub$off.or.no == n, 'mean.tempk'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(t.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.tempk']), mean(sub[sub$off.or.no == n, 'mean.tempk'])),
          lwd = 6, col = t.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.tempk, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(t.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.tempk'],
         sub[sub$off.or.no == 2, 'mean.tempk'],
         paired = FALSE) ## DIFFERENT


##### >> Summer rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 1,]        ## subset to annual summary stats
sub[sub$num.surv == 0, 'off.or.no'] <- 1
sub[sub$num.surv > 0, 'off.or.no'] <- 2

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.g, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean greenness', main = 'Summer rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.g'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.g'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.g']) - sd(sub[sub$off.or.no == n, 'mean.g'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.g']) + sd(sub[sub$off.or.no == n, 'mean.g'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(g.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.g']), mean(sub[sub$off.or.no == n, 'mean.g'])),
          lwd = 6, col = g.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.g, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(g.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.g'],
         sub[sub$off.or.no == 2, 'mean.g'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.b, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean brightness', main = 'Summer rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.b'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.b'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.b']) - sd(sub[sub$off.or.no == n, 'mean.b'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.b']) + sd(sub[sub$off.or.no == n, 'mean.b'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(b.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.b']), mean(sub[sub$off.or.no == n, 'mean.b'])),
          lwd = 6, col = b.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.b, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(b.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.b'],
         sub[sub$off.or.no == 2, 'mean.b'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.w, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean wetness', main = 'Summer rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.w'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.w'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.w']) - sd(sub[sub$off.or.no == n, 'mean.w'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.w']) + sd(sub[sub$off.or.no == n, 'mean.w'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(w.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.w']), mean(sub[sub$off.or.no == n, 'mean.w'])),
          lwd = 6, col = w.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.w, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(w.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.w'],
         sub[sub$off.or.no == 2, 'mean.w'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.tempk, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean temp K', main = 'Summer rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.tempk'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.tempk'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.tempk']) - sd(sub[sub$off.or.no == n, 'mean.tempk'])
    hi <- mean(sub[sub$off.or.no == n, 'mean.tempk']) + sd(sub[sub$off.or.no == n, 'mean.tempk'])
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(t.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.tempk']), mean(sub[sub$off.or.no == n, 'mean.tempk'])),
          lwd = 6, col = t.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.tempk, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(t.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.tempk'],
         sub[sub$off.or.no == 2, 'mean.tempk'],
         paired = FALSE) ## NOT DIFFERENT


##### >> Winter rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 2,]        ## subset to annual summary stats
sub[sub$num.surv == 0, 'off.or.no'] <- 1
sub[sub$num.surv > 0, 'off.or.no'] <- 2

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.g, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean greenness', main = 'Winter rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.g'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.g'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.g'], na.rm = TRUE) - sd(sub[sub$off.or.no == n, 'mean.g'], na.rm = TRUE)
    hi <- mean(sub[sub$off.or.no == n, 'mean.g'], na.rm = TRUE) + sd(sub[sub$off.or.no == n, 'mean.g'], na.rm = TRUE)
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(g.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.g'], na.rm = TRUE), 
                             mean(sub[sub$off.or.no == n, 'mean.g'], na.rm = TRUE)),
          lwd = 6, col = g.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.g, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(g.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.g'],
         sub[sub$off.or.no == 2, 'mean.g'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.b, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean brightness', main = 'Winter rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.b'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.b'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.b'], na.rm = TRUE) - sd(sub[sub$off.or.no == n, 'mean.b'], na.rm = TRUE)
    hi <- mean(sub[sub$off.or.no == n, 'mean.b'], na.rm = TRUE) + sd(sub[sub$off.or.no == n, 'mean.b'], na.rm = TRUE)
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(b.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.b'], na.rm = TRUE), mean(sub[sub$off.or.no == n, 'mean.b'], na.rm = TRUE)),
          lwd = 6, col = b.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.b, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(b.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.b'],
         sub[sub$off.or.no == 2, 'mean.b'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.w, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean wetness', main = 'Winter rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.w'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.w'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.w'], na.rm = TRUE) - sd(sub[sub$off.or.no == n, 'mean.w'], na.rm = TRUE)
    hi <- mean(sub[sub$off.or.no == n, 'mean.w'], na.rm = TRUE) + sd(sub[sub$off.or.no == n, 'mean.w'], na.rm = TRUE)
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(w.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.w'], na.rm = TRUE), mean(sub[sub$off.or.no == n, 'mean.w'], na.rm = TRUE)),
          lwd = 6, col = w.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.w, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(w.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.w'],
         sub[sub$off.or.no == 2, 'mean.w'],
         paired = FALSE) ## NOT DIFFERENT

plot(jitter(sub$off.or.no, factor = 0.8), sub$mean.tempk, xlim = c(0.6, 2.4), pch = 19,
     col = 'transparent', xaxt = 'n', xlab = 'Number of surviving offspring',
     ylab = 'Mean temp K', main = 'Winter rainy season means')
  axis(1, at = c(1,2), labels = c('0','> 0'))
  for(n in c(1,2)){
    ## 83% quantile
    # lo <- quantile(sub[sub$off.or.no == n, 'mean.tempk'], probs = c(0.085, 0.915))[1]
    # hi <- quantile(sub[sub$off.or.no == n, 'mean.tempk'], probs = c(0.085, 0.915))[2]
    ## SD
    lo <- mean(sub[sub$off.or.no == n, 'mean.tempk'], na.rm = TRUE) - sd(sub[sub$off.or.no == n, 'mean.tempk'], na.rm = TRUE)
    hi <- mean(sub[sub$off.or.no == n, 'mean.tempk'], na.rm = TRUE) + sd(sub[sub$off.or.no == n, 'mean.tempk'], na.rm = TRUE)
    polygon(c(n-wid, n+wid, n+wid, n-wid), c(lo, lo, hi, hi), col = alpha(t.col, poly.alph), border = NA)
    lines(c(n-wid, n+wid), c(mean(sub[sub$off.or.no == n, 'mean.tempk'], na.rm = TRUE), mean(sub[sub$off.or.no == n, 'mean.tempk'], na.rm = TRUE)),
          lwd = 6, col = t.col)
  }
  points(jitter(sub$off.or.no, factor = 0.8), sub$mean.tempk, xlim = c(0.6, 2.4), pch = 19,
         col = alpha(t.col, pt.alph), cex = pt.cex)
  
  t.test(sub[sub$off.or.no == 1, 'mean.tempk'],
         sub[sub$off.or.no == 2, 'mean.tempk'],
         paired = FALSE) ## NOT DIFFERENT

k <- k+1
dev.off()
}
