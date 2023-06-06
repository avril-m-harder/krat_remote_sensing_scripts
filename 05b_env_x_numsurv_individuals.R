library(sp)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
library(lmtest)
library(NatParksPalettes)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(TeachingDemos)
source('/Users/Avril/Documents/krat_remote_sensing/krat_remote_sensing_scripts/03b_c2l2readMeta.R')
library(MASS)
library(pscl)
library(beyonce)
`%notin%` <- Negate(`%in%`)

w.col <- beyonce_palette(75)[2]
b.col <- beyonce_palette(75)[5]
t.col <- beyonce_palette(75)[7]
cols <- c(w.col, b.col, t.col)
names <- c('wetness','brightness','temp.k')

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

## get list of mound:cell associations
mounds.cells.only <- mc.key[,c('database.name','cell.num')]
mounds.cells.only <- mounds.cells.only[!duplicated(mounds.cells.only),]


##### Define seasons #####
tc.dat$year <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,1]
## define brood-year lag months (e.g., for indivs born in 2000, lag.year would be set to 2000 for July 1999-June 2000)
## i.e., abiotic data from the months of July - June are predicting survival/fitness for indivs presumably born between those months
tc.dat$year <- as.numeric(tc.dat$year)
tc.dat$month <- do.call(rbind, strsplit(tc.dat$acq.date, split='-'))[,2]
tc.dat$month <- as.numeric(tc.dat$month)
tc.dat[which(tc.dat$month %in% c(7:12)), 'half.year.lag'] <- tc.dat[which(tc.dat$month %in% c(7:12)), 'year'] + 1
tc.dat[which(tc.dat$month %in% c(1:6)), 'half.year.lag'] <- tc.dat[which(tc.dat$month %in% c(1:6)), 'year']

## limit to 1993 - 2005 because of low Landsat data availability before 1993 (also matches pop-level analyses)
## updated on 3/21/23 to make environmental data match offspring years 1993 - 2005
tc.dat <- tc.dat[tc.dat$half.year.lag >= 1993 & tc.dat$half.year.lag <= 2005,]

## scale and center TC
tc.dat$z.g <- scale(tc.dat$greenness, scale=TRUE, center=TRUE)
tc.dat$z.b <- scale(tc.dat$brightness, scale=TRUE, center=TRUE)
tc.dat$z.w <- scale(tc.dat$wetness, scale=TRUE, center=TRUE)

## define seasons by expected precip (1=July-August, 2=Dec-March, 0=outside these months)
tc.dat[which(tc.dat$month %in% c(7,8)), 'weather.szn'] <- 1
tc.dat[which(tc.dat$month %in% c(12,1,2,3)), 'weather.szn'] <- 2
tc.dat[which(tc.dat$month %in% c(4,5,6,9,10,11)), 'weather.szn'] <- 0


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
fem.ann <- fem.ann[fem.ann$year >= 1993 & fem.ann$year <= 2005,]
## because looking at # of offspring surviving, limit to observations where at least 1 offspring was produced in year t
fem.ann <- fem.ann[fem.ann$num.off > 0,]

# ##### Add environmental summary stats to each fitness observation #####
# OUT <- NULL
# for(r in 1:nrow(fem.ann)){  ## for each observation,
#   print(r/nrow(fem.ann))
#   cn <- fem.ann$cell.num[r]
#   ## identify 8 adjacent (surrounding) cells ; 92 rows, 70 columns in raster;
#   ## (math confirmed by manually plotting)
#   cells <- c(cn, (cn-71), (cn-70), (cn-69), (cn-1), (cn+1), (cn+69), (cn+70), (cn+71))
#   sub.tc <- tc.dat[tc.dat$half.year.lag == fem.ann$year[r] & tc.dat$cell.num %in% cells,
#                     c('cell.num','z.g','z.b','z.w','temp.k','year','month','half.year.lag','weather.szn')]
#   ## save annual information
#   g <- mean(sub.tc$z.g)
#   b <- mean(sub.tc$z.b)
#   w <- mean(sub.tc$z.w)
#   t <- mean(sub.tc$temp.k)
#   g.sd <- sd(sub.tc$z.g)
#   b.sd <- sd(sub.tc$z.b)
#   w.sd <- sd(sub.tc$z.w)
#   t.sd <- sd(sub.tc$temp.k)
#   save <- as.numeric(c(unlist(fem.ann[r,c(2:6)]), '1', NA, g, b, w, t, g.sd, b.sd, w.sd, t.sd))
#   OUT <- rbind(OUT, save)
#   ## then save season information
#   for(s in c(1,2)){                                 ## for each weather-based season,
#     temp <- sub.tc[sub.tc$weather.szn == s,]        ## subset the data,
#     g <- mean(temp$z.g)                             ## and collect TC + temp.k means + SDs
#     b <- mean(temp$z.b)
#     w <- mean(temp$z.w)
#     t <- mean(temp$temp.k)
#     g.sd <- sd(temp$z.g)
#     b.sd <- sd(temp$z.b)
#     w.sd <- sd(temp$z.w)
#     t.sd <- sd(temp$temp.k)
#     save <- as.numeric(c(unlist(fem.ann[r,c(2:6)]), '2', s, g, b, w, t, g.sd, b.sd, w.sd, t.sd))
#     OUT <- rbind(OUT, save)
#   }
# }
# env.fem.ann <- as.data.frame(OUT)
# colnames(env.fem.ann) <- c('id','year','num.off','num.surv','prop.surv','type','season','mean.g',
#                            'mean.b','mean.w','mean.tempk',
#                            'sd.g','sd.b','sd.w','sd.tempk')
# write.csv(env.fem.ann, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/indiv_propsurv_x_env_bycells.csv', row.names = FALSE)
env.fem.ann <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/indiv_propsurv_x_env_bycells.csv')

##### Plot relationships between environmental summary statistics and individual annual female fitness #####
# types <- c('annual','weather-based')
# pdf('/Users/Avril/Desktop/env_v_propsurv_bycells.pdf', width = 5, height = 5)
# for(t in unique(env.fem.ann$type)){
#   sub <- env.fem.ann[env.fem.ann$type == t,]
#   if(t == 1){
#     plot(sub$mean.g, sub$prop.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Proportion offspring surviving',
#          main = paste0(types[t]))
#     plot(sub$mean.b, sub$prop.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Proportion offspring surviving',
#          main = paste0(types[t]))
#     plot(sub$mean.w, sub$prop.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Proportion offspring surviving',
#          main = paste0(types[t]))
#     plot(sub$mean.tempk, sub$prop.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Proportion offspring surviving',
#          main = paste0(types[t]))
#   } else{
#     for(s in c(1,2)){
#       sub1 <- sub[sub$season == s,]
#       plot(sub1$mean.g, sub1$prop.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Proportion offspring surviving',
#            main = paste0(types[t],' - ',s))
#       plot(sub1$mean.b, sub1$prop.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Proportion offspring surviving',
#            main = paste0(types[t],' - ',s))
#       plot(sub1$mean.w, sub1$prop.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Proportion offspring surviving',
#            main = paste0(types[t],' - ',s))
#       plot(sub1$mean.tempk, sub1$prop.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Proportion offspring surviving',
#            main = paste0(types[t],' - ',s))
#     }
#   }
# }
# dev.off()

# types <- c('annual','weather-based')
# pdf('/Users/Avril/Desktop/env_v_numsurv_bycells.pdf', width = 5, height = 5)
# for(t in unique(env.fem.ann$type)){
#   sub <- env.fem.ann[env.fem.ann$type == t,]
#   if(t == 1){
#     plot(sub$mean.g, sub$num.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Number of offspring surviving',
#          main = paste0(types[t]))
#     plot(sub$mean.b, sub$num.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Number of offspring surviving',
#          main = paste0(types[t]))
#     plot(sub$mean.w, sub$num.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Number of offspring surviving',
#          main = paste0(types[t]))
#     plot(sub$mean.tempk, sub$num.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Number of offspring surviving',
#          main = paste0(types[t]))
#   } else{
#     for(s in c(1,2)){
#       sub1 <- sub[sub$season == s,]
#       plot(sub1$mean.g, sub1$num.surv, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Number of offspring surviving',
#            main = paste0(types[t],' - ',s))
#       plot(sub1$mean.b, sub1$num.surv, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Number of offspring surviving',
#            main = paste0(types[t],' - ',s))
#       plot(sub1$mean.w, sub1$num.surv, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Number of offspring surviving',
#            main = paste0(types[t],' - ',s))
#       plot(sub1$mean.tempk, sub1$num.surv, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Number of offspring surviving',
#            main = paste0(types[t],' - ',s))
#     }
#   }
# }
# dev.off()


##### Check correlations among annual, summer, winter values for wetness, brightness, and temp.k #####
pdf('/Users/Avril/Desktop/cross_season_correlations_numsurv.pdf', width = 6, height = 5)
col <- 1
for(c in c(10,9,11)){
  sub <- env.fem.ann[,c(1,2,6,7,c)]
  sub$comb.id <- paste0(sub$id,'-',sub$year)
  
  if(c == 11){
    sub[,5] <- sub[,5]-273.15
  }
  
  OUT <- NULL
  for(i in unique(sub$comb.id)){
    save <- c(sub[sub$comb.id == i & sub$type == 1, 5], sub[sub$comb.id == i & sub$type == 2 & sub$season == 1, 5])
    OUT <- rbind(OUT, save)
  }
  OUT <- OUT[complete.cases(OUT),]
  plot(OUT[,1], OUT[,2], pch = 19, col = alpha(cols[col], 0.4),
       main = colnames(env.fem.ann)[c], xlab = 'Annual', ylab = 'Summer rainy season')
  # mod <- lm(OUT[,2] ~ OUT[,1])
  # abline(mod, lty = 2, lwd = 2, col = cols[col])
  # legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
  #                                   '\nadj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3),
  #                                   '\ncorr coeff = ',round(cor(OUT[,1], OUT[,2]), digits = 3)),
  #        bty = 'n')
  legend('topleft', legend = paste0('corr coeff = ',round(cor(OUT[,1], OUT[,2]), digits = 3)),
         bty = 'n')
  
  temp <- sub[sub$type == 2 & sub$season == 2,]
  temp <- temp[!is.na(temp[,5]), 'comb.id']
  OUT <- NULL
  for(i in unique(temp)){
    save <- c(sub[sub$comb.id == i & sub$type == 1, 5], sub[sub$comb.id == i & sub$type == 2 & sub$season == 2, 5])
    OUT <- rbind(OUT, save)
  }
  OUT <- OUT[complete.cases(OUT),]
  plot(OUT[,1], OUT[,2], pch = 19, col = alpha(cols[col], 0.4),
       main = colnames(env.fem.ann)[c], xlab = 'Annual', ylab = 'Winter rainy season')
  # mod <- lm(OUT[,2] ~ OUT[,1])
  # abline(mod, lty = 2, lwd = 2, col = cols[col])
  # legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
  #                                   '\nadj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3),
  #                                   '\ncorr coeff = ',round(cor(OUT[,1], OUT[,2]), digits = 3)),
  #        bty = 'n')
  legend('topleft', legend = paste0('corr coeff = ',round(cor(OUT[,1], OUT[,2]), digits = 3)),
         bty = 'n')
  
  temp <- sub[sub$type == 2 & sub$season == 2,]
  temp <- temp[!is.na(temp[,5]), 'comb.id']
  OUT <- NULL
  for(i in unique(temp)){
    save <- c(sub[sub$comb.id == i & sub$type == 2 & sub$season == 1, 5], sub[sub$comb.id == i & sub$type == 2 & sub$season == 2, 5])
    OUT <- rbind(OUT, save)
  }
  OUT <- OUT[complete.cases(OUT),]
  plot(OUT[,1], OUT[,2], pch = 19, col = alpha(cols[col], 0.4), 
       main = colnames(env.fem.ann)[c], xlab = 'Summer rainy season', ylab = 'Winter rainy season')
  # mod <- lm(OUT[,2] ~ OUT[,1])
  # abline(mod, lty = 2, lwd = 2, col = cols[col])
  # legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
  #                                   '\nadj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3),
  #                                   '\ncorr coeff = ',round(cor(OUT[,1], OUT[,2]), digits = 3)),
  #        bty = 'n')
  legend('topleft', legend = paste0('corr coeff = ',round(cor(OUT[,1], OUT[,2]), digits = 3)),
         bty = 'n')
  col <- col+1
}
dev.off()


##### Construct backwards stepwise regressions and conduct LRTs (proportion offspring surviving) #####
##### >> Annual averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 1,]        ## subset to annual summary stats

## Poisson GLM
p1 <- glm(num.surv ~ mean.g + mean.b + mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p1) ## remove mean.b

E2 <- resid(p1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p1))   
sum(E2^2) / (N - p)
logLik(p1)

p2 <- glm(num.surv ~ mean.g + mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p2) ## remove mean.g

E2 <- resid(p2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p2))   
sum(E2^2) / (N - p)
logLik(p2)

p3 <- glm(num.surv ~ mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p3) ## remove mean.w

E2 <- resid(p3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p3))   
sum(E2^2) / (N - p)
logLik(p3)

p4 <- glm(num.surv ~ mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p4) ## p = 4.08e-7

## check for over/underdispersion in the model
E2 <- resid(p4, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p4))   
sum(E2^2) / (N - p) ## 1.25 -- overdispersion
logLik(p4)

## NB GLM
nb1 <- glm.nb(num.surv ~ mean.g + mean.b + mean.w + mean.tempk, 
              data = sub)
summary(nb1) ## remove mean.b

E2 <- resid(nb1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb1))   
sum(E2^2) / (N - p)
logLik(nb1)

nb2 <- glm.nb(num.surv ~ mean.g + mean.w + mean.tempk, 
              data = sub)
summary(nb2) ## remove mean.g

E2 <- resid(nb2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb2))   
sum(E2^2) / (N - p)
logLik(nb2)

nb3 <- glm.nb(num.surv ~ mean.w + mean.tempk, 
              data = sub)
summary(nb3) ## remove mean.w

E2 <- resid(nb3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb3))   
sum(E2^2) / (N - p)
logLik(nb3)

nb4 <- glm.nb(num.surv ~ mean.tempk, 
              data = sub)
summary(nb4) ## p = 8.3e-6

## check for over/underdispersion in the model
E2 <- resid(nb4, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb4))   
sum(E2^2) / (N - p) ## 1.07 -- slight overdispersion??
logLik(nb4)

lrtest(p4, nb4)

## summary for each single-predictor model
summary(glm.nb(num.surv ~ mean.b, data = sub))      ## p = 0.305
summary(glm.nb(num.surv ~ mean.w, data = sub))      ## p = 0.824
summary(glm.nb(num.surv ~ mean.g, data = sub))      ## p = 0.855
summary(glm.nb(num.surv ~ mean.tempk, data = sub))  ## p = 8.3e-6
cor(sub[,c(8:11)])

##### >> Summer rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 1,]        ## subset to seasonal summary stats

## Poisson GLM
p1 <- glm(num.surv ~ mean.g + mean.b + mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p1) ## remove mean.tempk

E2 <- resid(p1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p1))   
sum(E2^2) / (N - p)
logLik(p1)

p2 <- glm(num.surv ~ mean.g + mean.w + mean.b, 
          family = 'poisson',
          data = sub)
summary(p2) ## remove mean.g

E2 <- resid(p2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p2))   
sum(E2^2) / (N - p)
logLik(p2)

p3 <- glm(num.surv ~ mean.w + mean.b, 
          family = 'poisson',
          data = sub)
summary(p3) ## remove mean.w

E2 <- resid(p3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p3))   
sum(E2^2) / (N - p)
logLik(p3)

p4 <- glm(num.surv ~ mean.b, 
          family = 'poisson',
          data = sub)
summary(p4) ## p = 0.02

## check for over/underdispersion in the model
E2 <- resid(p4, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p4))   
sum(E2^2) / (N - p) ## 1.22 -- overdispersion
logLik(p4)

## NB GLM
nb1 <- glm.nb(num.surv ~ mean.g + mean.b + mean.w + mean.tempk, 
              data = sub)
summary(nb1) ## remove mean.tempk

E2 <- resid(nb1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb1))   
sum(E2^2) / (N - p)
logLik(nb1)

nb2 <- glm.nb(num.surv ~ mean.g + mean.w + mean.b, 
              data = sub)
summary(nb2) ## remove mean.g

E2 <- resid(nb2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb2))   
sum(E2^2) / (N - p)
logLik(nb2)

nb3 <- glm.nb(num.surv ~ mean.w + mean.b, 
              data = sub)
summary(nb3) ## remove mean.w

E2 <- resid(nb3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb3))   
sum(E2^2) / (N - p)
logLik(nb3)

nb4 <- glm.nb(num.surv ~ mean.b, 
              data = sub)
summary(nb4) ## p = 0.04

## check for over/underdispersion in the model
E2 <- resid(nb4, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb4))   
sum(E2^2) / (N - p) ## 0.98 -- underdispersion
logLik(nb4)

lrtest(p4, nb4)

## summary for each single-predictor model
summary(glm.nb(num.surv ~ mean.b, data = sub))      ## p = 0.0375
summary(glm.nb(num.surv ~ mean.w, data = sub))      ## p = 0.687
summary(glm.nb(num.surv ~ mean.g, data = sub))      ## p = 0.9975
summary(glm.nb(num.surv ~ mean.tempk, data = sub))  ## p = 0.372
cor(sub[!is.na(sub$mean.g),c(8:11)])

##### >> Winter rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 2,]        ## subset to seasonal summary stats

## Poisson GLM
p1 <- glm(num.surv ~ mean.g + mean.b + mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p1) ## remove mean.tempk

E2 <- resid(p1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p1))   
sum(E2^2) / (N - p) ## 0.98 -- underdispersion
logLik(p1)

p2 <- glm(num.surv ~ mean.g + mean.w + mean.b, 
          family = 'poisson',
          data = sub)
summary(p2) ## remove mean.g

p3 <- glm(num.surv ~ mean.w + mean.b, 
          family = 'poisson',
          data = sub)
summary(p3) ## remove mean.b

p4 <- glm(num.surv ~ mean.w, 
          family = 'poisson',
          data = sub)
summary(p4) ## not significant

## NB GLM
nb1 <- glm.nb(num.surv ~ mean.g + mean.b + mean.w + mean.tempk, 
              data = sub)
summary(nb1) ## remove mean.tempk

nb2 <- glm.nb(num.surv ~ mean.g + mean.w + mean.b, 
              data = sub)
summary(nb2) ## remove mean.g

nb3 <- glm.nb(num.surv ~ mean.w + mean.b, 
              data = sub)
summary(nb3) ## remove mean.b

nb4 <- glm.nb(num.surv ~ mean.w, 
              data = sub)
summary(nb4) ## not significant


##### Final model for each time frame #####
library(jtools)
# pdf('/Users/Avril/Desktop/numsurv_sig_predictors.pdf', width = 6, height = 5)
## Annual
sub <- env.fem.ann[env.fem.ann$type == 1,]        ## subset to annual summary stats
sub$mean.tempk <- sub$mean.tempk - 273.15
nb4 <- glm.nb(num.surv ~ mean.tempk, 
              data = sub)
summary(nb4) ## p = 8.3e-6

# plot(sub$mean.tempk-273.15, sub$num.off, pch = 19, col = alpha(t.col, 0.8), xlab = 'Mean annual surface temperature (deg C)',
#      ylab = 'Number of surviving offspring')

pdf('/Users/Avril/Desktop/annual_surftemp_v_numsurv_effects.pdf', width = 3, height = 4)
effect_plot(nb4, pred = mean.tempk, interval = TRUE, plot.points = TRUE, partial.residuals = FALSE, 
            colors = t.col, point.alpha = 0.6, centered = 'all') +
  theme_bw() + 
  labs(x = 'Mean annual surface temperature (deg C)', y = 'Number of surviving offspring') +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
        panel.grid.major = element_blank(), text=element_text(color="black"),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12, colour = 'black'),
        axis.title=element_text(size=12))
dev.off()

## Summer rainy
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 1,]        ## subset to summer rainy season summary stats
nb4 <- glm.nb(num.surv ~ mean.b, 
              data = sub)
summary(nb4) ## p = 0.04

plot(sub$mean.b, sub$num.off, pch = 19, col = alpha(b.col, 0.8), xlab = 'Mean summer rainy season brightness',
     ylab = 'Number of surviving offspring')

## Winter rainy
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 2,]        ## subset to winter rainy season summary stats

## no significant models for this season
# dev.off()

