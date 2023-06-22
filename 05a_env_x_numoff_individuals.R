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
cols <- c(w.col, b.col, t.col)
names <- c('wetness','brightness','temp.k')

## manual option
# g.col <- 'forestgreen'
# w.col <- 'deepskyblue2'
# b.col <- 'tan4'
# t.col <- 'chocolate2'
# pp.col <- 'turquoise4'
# pt.col <- 'tomato2'
# cols <- c(g.col, w.col, b.col, t.col, pp.col, pt.col)

## plot colors to test
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
fem.ann <- fem.ann[fem.ann$year >= 1993 & fem.ann$year <= 2005,] ## keep data for offspring produced in 1993 - 2005


##### Add environmental summary stats to each fitness observation #####
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
#   save <- as.numeric(c(unlist(fem.ann[r,c(2:5)]), '1', NA, g, b, w, t, g.sd, b.sd, w.sd, t.sd))
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
#     save <- as.numeric(c(unlist(fem.ann[r,c(2:5)]), '2', s, g, b, w, t, g.sd, b.sd, w.sd, t.sd))
#     OUT <- rbind(OUT, save)
#   }
# }
# env.fem.ann <- as.data.frame(OUT)
# colnames(env.fem.ann) <- c('id','year','num.off','num.surv','type','season','mean.g',
#                            'mean.b','mean.w','mean.tempk',
#                            'sd.g','sd.b','sd.w','sd.tempk')
# write.csv(env.fem.ann, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/indiv_female_fitness_x_env_bycells.csv', row.names = FALSE)
env.fem.ann <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/indiv_female_fitness_x_env_bycells.csv')


##### Plot relationships between environmental summary statistics and individual annual female fitness #####
types <- c('annual','weather-based')
pdf('/Users/Avril/Desktop/env_v_numoff_bycells.pdf', width = 5, height = 5)
for(t in unique(env.fem.ann$type)){
  sub <- env.fem.ann[env.fem.ann$type == t,]
  if(t == 1){
    plot(sub$mean.g, sub$num.off, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Number of offspring',
         main = paste0(types[t]))
    plot(sub$mean.b, sub$num.off, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Number of offspring',
         main = paste0(types[t]))
    plot(sub$mean.w, sub$num.off, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Number of offspring',
         main = paste0(types[t]))
    plot(sub$mean.tempk, sub$num.off, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Number of offspring',
         main = paste0(types[t]))
  } else{
    for(s in c(1,2)){
      sub1 <- sub[sub$season == s,]
      plot(sub1$mean.g, sub1$num.off, col = alpha(g.col, 0.4), pch = 19, xlab = 'Mean greenness', ylab = 'Number of offspring',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.b, sub1$num.off, col = alpha(b.col, 0.4), pch = 19, xlab = 'Mean brightness', ylab = 'Number of offspring',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.w, sub1$num.off, col = alpha(w.col, 0.4), pch = 19, xlab = 'Mean wetness', ylab = 'Number of offspring',
           main = paste0(types[t],' - ',s))
      plot(sub1$mean.tempk, sub1$num.off, col = alpha(t.col, 0.4), pch = 19, xlab = 'Mean temp.k', ylab = 'Number of offspring',
           main = paste0(types[t],' - ',s))
    }
  }
}
dev.off()

# pdf('/Users/Avril/Desktop/env_v_numoff_bycells_boxplots.pdf', width = 7, height = 5)
# for(t in unique(env.fem.ann$type)){
#   sub <- env.fem.ann[env.fem.ann$type == t,]
#   if(t == 1){
#     boxplot(sub$mean.g ~ sub$num.off, col = alpha(g.col, 1), pch = 19, ylab = 'Mean greenness', xlab = 'Number of offspring',
#          main = paste0(types[t]))
#     boxplot(sub$mean.b ~ sub$num.off, col = alpha(b.col, 1), pch = 19, ylab= 'Mean brightness', xlab = 'Number of offspring',
#          main = paste0(types[t]))
#     boxplot(sub$mean.w ~ sub$num.off, col = alpha(w.col, 1), pch = 19, ylab= 'Mean wetness', xlab = 'Number of offspring',
#          main = paste0(types[t]))
#     boxplot(sub$mean.tempk ~ sub$num.off, col = alpha(t.col, 1), pch = 19, ylab= 'Mean temp.k', xlab = 'Number of offspring',
#          main = paste0(types[t]))
#   } else{
#     for(s in c(1,2)){
#       sub1 <- sub[sub$season == s,]
#       boxplot(sub1$mean.g ~ sub1$num.off, col = alpha(g.col, 1), pch = 19, ylab= 'Mean greenness', xlab = 'Number of offspring',
#            main = paste0(types[t],' - ',s))
#       boxplot(sub1$mean.b ~ sub1$num.off, col = alpha(b.col, 1), pch = 19, ylab= 'Mean brightness', xlab = 'Number of offspring',
#            main = paste0(types[t],' - ',s))
#       boxplot(sub1$mean.w ~ sub1$num.off, col = alpha(w.col, 1), pch = 19, ylab= 'Mean wetness', xlab = 'Number of offspring',
#            main = paste0(types[t],' - ',s))
#       boxplot(sub1$mean.tempk ~ sub1$num.off, col = alpha(t.col, 1), pch = 19, ylab= 'Mean temp.k', xlab = 'Number of offspring',
#            main = paste0(types[t],' - ',s))
#     }
#   }
# }
# dev.off()

##### Check correlations among annual, summer, winter values for wetness, brightness, and temp.k #####
pdf('/Users/Avril/Desktop/cross_season_correlations_numoff.pdf', width = 6, height = 5)
col <- 1
for(c in c(9,8,10)){
  sub <- env.fem.ann[,c(1,2,5,6,c)]
  sub$comb.id <- paste0(sub$id,'-',sub$year)
  
  if(c == 10){
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
  

##### Construct backwards stepwise regressions and conduct LRTs (number of offspring) #####
##### >> Annual averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 1,]        ## subset to annual summary stats

## Poisson GLM
p1 <- glm(num.off ~ mean.g + mean.b + mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p1) ## remove mean.w
logLik(p1)

E2 <- resid(p1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p1))   
sum(E2^2) / (N - p) 

p2 <- glm(num.off ~ mean.g + mean.b + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p2) ## remove mean.g
logLik(p2)

E2 <- resid(p2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p2))   
sum(E2^2) / (N - p)

p3 <- glm(num.off ~ mean.b + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p3) ## keeping mean.b and mean.tempk

## check for over/underdispersion in the model
E2 <- resid(p3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p3))   
sum(E2^2) / (N - p) ## 2.39 -- overdispersion
logLik(p3)

## NB GLM
nb1 <- glm.nb(num.off ~ mean.g + mean.b + mean.w + mean.tempk, 
          data = sub)

summary(nb1) ## remove mean.w

E2 <- resid(nb1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb1))   
sum(E2^2) / (N - p) 
logLik(nb1)

nb2 <- glm.nb(num.off ~ mean.g + mean.b + mean.tempk, 
          data = sub)
summary(nb2) ## remove mean.g

E2 <- resid(nb2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb2))   
sum(E2^2) / (N - p) 
logLik(nb2)

nb3 <- glm.nb(num.off ~ mean.b + mean.tempk, 
          data = sub)
summary(nb3) ## keeping mean.b and mean.tempk

## check for over/underdispersion in the model
E2 <- resid(nb3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb3))   
sum(E2^2) / (N - p) ## 0.96 -- underdispersion
logLik(nb3)


## compare the two approaches
lrtest(p3, nb3) ## p < 2.2e-16; NB model significantly better fit (AIC is lower too, idk if those can be directly compared?)

## summary for each single-predictor model
summary(glm.nb(num.off ~ mean.b, data = sub))      ## p = 0.0308
summary(glm.nb(num.off ~ mean.w, data = sub))      ## p = 0.107
summary(glm.nb(num.off ~ mean.g, data = sub))      ## p = 0.816
summary(glm.nb(num.off ~ mean.tempk, data = sub))  ## p = 0.0610
cor(sub[,c(7:10)])


##### >> Summer rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 1,]        ## subset to summer rainy season summary stats

## Poisson GLM
p1 <- glm(num.off ~ mean.g + mean.b + mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p1) ## remove mean.g

E2 <- resid(p1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p1))   
sum(E2^2) / (N - p)
logLik(p1)

p2 <- glm(num.off ~ mean.w + mean.b + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p2) ## keeping mean.b, mean.w, and mean.tempk

E2 <- resid(p2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p2))   
sum(E2^2) / (N - p)
logLik(p2)

## NB GLM
nb1 <- glm.nb(num.off ~ mean.g + mean.b + mean.w + mean.tempk, 
              data = sub)
summary(nb1) ## remove mean.g

E2 <- resid(nb1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb1))   
sum(E2^2) / (N - p)
logLik(nb1)

nb2 <- glm.nb(num.off ~ mean.w + mean.b + mean.tempk, 
              data = sub)
summary(nb2) ## remove mean.w

E2 <- resid(nb2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb2))   
sum(E2^2) / (N - p)
logLik(nb2)

nb3 <- glm.nb(num.off ~ mean.b + mean.tempk, 
              data = sub)
summary(nb3) ## remove mean.tempk

E2 <- resid(nb3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb3))   
sum(E2^2) / (N - p)
logLik(nb3)

nb4 <- glm.nb(num.off ~ mean.b, 
              data = sub)
summary(nb4) ## p = 0.0001

E2 <- resid(nb4, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb4))   
sum(E2^2) / (N - p)
logLik(nb4)

lrtest(p2, nb4) ## neg binomial better (not sure if it works to compare different sets of predictors?)
## also compare poisson and neg bin models with matching predictors
p1 <- glm(num.off ~ mean.b, 
                family = 'poisson',
                data = sub)
summary(p1)
## check for over/underdispersion in the model
E2 <- resid(p1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p1))   
sum(E2^2) / (N - p) ## 2.41 -- overdispersion

nb1 <- glm.nb(num.off ~ mean.b,
              data = sub)
lrtest(p1, nb1)

## summary for each single-predictor model
summary(glm.nb(num.off ~ mean.b, data = sub))      ## p = 0.0011
summary(glm.nb(num.off ~ mean.w, data = sub))      ## p = 0.116
summary(glm.nb(num.off ~ mean.g, data = sub))      ## p = 0.909
summary(glm.nb(num.off ~ mean.tempk, data = sub))  ## p = 0.875
cor(sub[,c(7:10)])

##### >> Winter rainy season averages / totals #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 2,]        ## subset to winter rainy season summary stats

## Poisson GLM
p1 <- glm(num.off ~ mean.g + mean.b + mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p1) ## remove mean.g

E2 <- resid(p1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p1))   
sum(E2^2) / (N - p)
logLik(p1)

p2 <- glm(num.off ~ mean.w + mean.b + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p2) ## remove mean.b

E2 <- resid(p2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p2))   
sum(E2^2) / (N - p)
logLik(p2)

p3 <- glm(num.off ~ mean.w + mean.tempk, 
          family = 'poisson',
          data = sub)
summary(p3)

## check for over/underdispersion in the model
E2 <- resid(p3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(p3))   
sum(E2^2) / (N - p) ## 2.46 -- overdispersion
logLik(p3)


## NB GLM
nb1 <- glm.nb(num.off ~ mean.g + mean.b + mean.w + mean.tempk, 
              data = sub)
summary(nb1) ## remove mean.g

E2 <- resid(nb1, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb1))   
sum(E2^2) / (N - p) 
logLik(nb1)

nb2 <- glm.nb(num.off ~ mean.w + mean.b + mean.tempk, 
              data = sub)
summary(nb2) ## remove mean.b

E2 <- resid(nb2, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb2))   
sum(E2^2) / (N - p) 
logLik(nb2)

nb3 <- glm.nb(num.off ~ mean.w + mean.tempk, 
              data = sub)
summary(nb3)

## check for over/underdispersion in the model
E2 <- resid(nb3, type = "pearson")
N  <- nrow(sub)
p  <- length(coef(nb3))   
sum(E2^2) / (N - p) ## 0.95 -- slight underdispersion
logLik(nb3)

lrtest(p3, nb3)

## summary for each single-predictor model
summary(glm.nb(num.off ~ mean.b, data = sub))      ## p = 0.16
summary(glm.nb(num.off ~ mean.w, data = sub))      ## p = 0.0419
summary(glm.nb(num.off ~ mean.g, data = sub))      ## p = 0.112
summary(glm.nb(num.off ~ mean.tempk, data = sub))  ## p = 0.0114
cor(sub[,c(7:10)])

##### Final model for each time frame #####
library(jtools)
# pdf('/Users/Avril/Desktop/numoff_sig_predictors.pdf', width = 6, height = 5)
##### >> Annual #####
sub <- env.fem.ann[env.fem.ann$type == 1,]        ## subset to annual summary stats
sub$mean.tempk <- sub$mean.tempk - 273.15
nb3 <- glm.nb(num.off ~ mean.b + mean.tempk, 
              data = sub)
summary(nb3)
cor(sub$mean.b, sub$mean.tempk) ## predictors correlation coefficient = -0.042

plot(sub$mean.b, sub$num.off, pch = 19, col = alpha(b.col, 0.8), xlab = 'Mean annual brightness',
     ylab = 'Number of offspring')
plot(sub$mean.tempk, sub$num.off, pch = 19, col = alpha(t.col, 0.8), xlab = 'Mean annual surface temperature (deg C)',
     ylab = 'Number of offspring')

## vignette: https://cran.r-project.org/web/packages/jtools/vignettes/effect_plot.html
## centered can == 'all', 'none'; 'all' means that values for other predictor variable are set to the mean;
## 'none' bases all predictions on other variables being set to 0
# pdf('/Users/Avril/Desktop/annual_surftemp_v_numoff_effects.pdf', width = 3, height = 4)
effect_plot(nb3, pred = mean.tempk, interval = TRUE, plot.points = TRUE, partial.residuals = FALSE, 
            colors = t.col, point.alpha = 0.6, centered = 'all') +
  theme_bw() + 
  scale_y_continuous(breaks=seq(0,10,2)) +
  labs(x = 'Mean annual surface temperature (deg C)', y = 'Number of offspring') +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
        panel.grid.major = element_blank(), text=element_text(color="black"),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12, colour = 'black'),
        axis.title=element_text(size=12))
# dev.off()

# pdf('/Users/Avril/Desktop/2_x_2_annual_brightness_v_numoff_effects.pdf', width = 4.5, height = 4.5)
effect_plot(nb3, pred = mean.b, interval = TRUE, plot.points = TRUE, partial.residuals = FALSE, 
            colors = b.col, point.alpha = 0.6, point.size = 2, centered = 'all') +
  theme_bw() + 
  labs(x = 'Mean annual brightness', y = 'Number of offspring') +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
        panel.grid.major = element_blank(), text=element_text(color="black"),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12, colour = 'black'),
        axis.title=element_text(size=12))
# dev.off()

## t-test for <= 1 vs. >1 surviving offspring, surface temperature
sub[sub$num.off <= 2, 'cat.surv'] <- 0
sub[sub$num.off > 2, 'cat.surv'] <- 1
n <- nrow(sub[!is.na(sub$mean.tempk),])
m1 <- mean(sub[sub$cat.surv == 0, 'mean.tempk'], na.rm = TRUE)
sd1 <- sd(sub[sub$cat.surv == 0, 'mean.tempk'], na.rm = TRUE)/sqrt(n)*1.96
m2 <- mean(sub[sub$cat.surv == 1, 'mean.tempk'], na.rm = TRUE)
sd2 <- sd(sub[sub$cat.surv == 1, 'mean.tempk'], na.rm = TRUE)/sqrt(n)*1.96

pdf('/Users/Avril/Desktop/t-test_catnumoff_v_annual_surfacetemp.pdf', width = 5, height = 6)
plot(jitter(sub$cat.surv, factor = 0.8), sub$mean.tempk, 
     # pch = 19, col = alpha(b.col, 0.5),  ## with points() below, toggle based on whether points should be in back- or foreground
     pch = 19, col = 'transparent',
     xlim = c(-0.4, 1.4),
     xlab = 'Number of offspring',
     ylab = 'Mean annual surface temperature (deg C)', xaxt = 'n')
  axis(1, at = c(0,1), labels = c('<= 1','> 1'))
  lines(x = c(-0.2, 0.2), 
        y = c(m1, m1),
        col = t.col, lwd = 3)
  polygon(x = c(-0.15, 0.15, 0.15, -0.15),
          y = c(m1-sd1, m1-sd1, m1+sd1, m1+sd1),
          border = NA, col = alpha(t.col, 0.6))
  lines(x = c(0.8, 1.2), 
        y = c(m2, m2),
        col = t.col, lwd = 3)
  polygon(x = c(0.85, 1.15, 1.15, 0.85),
          y = c(m2-sd2, m2-sd2, m2+sd2, m2+sd2),
          border = NA, col = alpha(t.col, 0.6))
  points(jitter(sub$cat.surv, factor = 0.8), sub$mean.tempk, 
         pch = 19, col = alpha(t.col, 0.5), cex = 0.75)
dev.off()

shapiro.test(sub[sub$cat.surv == 0, 'mean.tempk']) ## neither normally distributed
shapiro.test(sub[sub$cat.surv == 1, 'mean.tempk']) 
bartlett.test(sub$mean.tempk ~ sub$cat.surv) ## equal variances, p > 0.05
t.test(sub[sub$cat.surv == 0, 'mean.tempk'], sub[sub$cat.surv == 1, 'mean.tempk'],
       paired = FALSE, var.equal = FALSE) ## p = 0.2222, not sig different

## t-test for <=1 vs. >1 surviving offspring, brightness
sub[sub$num.off <= 1, 'cat.surv'] <- 0
sub[sub$num.off > 1, 'cat.surv'] <- 1
n <- nrow(sub[!is.na(sub$mean.b),])
m1 <- mean(sub[sub$cat.surv == 0, 'mean.b'], na.rm = TRUE)
sd1 <- sd(sub[sub$cat.surv == 0, 'mean.b'], na.rm = TRUE)/sqrt(n)*1.96
m2 <- mean(sub[sub$cat.surv == 1, 'mean.b'], na.rm = TRUE)
sd2 <- sd(sub[sub$cat.surv == 1, 'mean.b'], na.rm = TRUE)/sqrt(n)*1.96

pdf('/Users/Avril/Desktop/t-test_catnumoff_v_annual_brightness.pdf', width = 5, height = 6)
plot(jitter(sub$cat.surv, factor = 0.8), sub$mean.b, 
     # pch = 19, col = alpha(b.col, 0.5),  ## with points() below, toggle based on whether points should be in back- or foreground
     pch = 19, col = 'transparent',
     xlim = c(-0.4, 1.4),
     xlab = 'Number of offspring',
     ylab = 'Mean annual brightness', xaxt = 'n')
  axis(1, at = c(0,1), labels = c('0','> 0'))
  lines(x = c(-0.2, 0.2), 
        y = c(m1, m1),
        col = b.col, lwd = 3)
  polygon(x = c(-0.15, 0.15, 0.15, -0.15),
          y = c(m1-sd1, m1-sd1, m1+sd1, m1+sd1),
          border = NA, col = alpha(b.col, 0.6))
  lines(x = c(0.8, 1.2), 
        y = c(m2, m2),
        col = b.col, lwd = 3)
  polygon(x = c(0.85, 1.15, 1.15, 0.85),
          y = c(m2-sd2, m2-sd2, m2+sd2, m2+sd2),
          border = NA, col = alpha(b.col, 0.6))
  points(jitter(sub$cat.surv, factor = 0.8), sub$mean.b, 
         pch = 19, col = alpha(b.col, 0.5), cex = 0.75)
dev.off()

shapiro.test(sub[sub$cat.surv == 0, 'mean.b']) ## both normally distributed
shapiro.test(sub[sub$cat.surv == 1, 'mean.b']) 
bartlett.test(sub$mean.b ~ sub$cat.surv) ## p > 0.05, equal variances
t.test(sub[sub$cat.surv == 0, 'mean.b'], sub[sub$cat.surv == 1, 'mean.b'],
       paired = FALSE, var.equal = TRUE) ## p = 0.006836, sig different

## check for density-dependent effects
OUT <- NULL
for(y in sort(unique(sub$year))){
  fs <- length(unique(sub[sub$year == y, 'id']))
  save <- c(y, fs)
  OUT <- rbind(OUT, save)
}

pdf('/Users/Avril/Desktop/offspring_by_numadultfemales.pdf', width = 6, height = 5)
OUT3 <- NULL
plot(0,0, xlim = c(min(OUT[,2]), max(OUT[,2])), ylim = c(0, 10), xlab = 'Number of reproductive females',
     ylab = 'Number of offspring (by individual)')
  for(r in 1:nrow(sub)){
    points(OUT[OUT[,1] == sub$year[r], 2], sub$num.off[r], col = alpha('black', 0.3), pch = 19)
    save <- c(OUT[OUT[,1] == sub$year[r], 2], sub$num.off[r])
    OUT3 <- rbind(OUT3, save)
  }
  mod <- lm(OUT3[,2] ~ OUT3[,1])
  summary(mod)
  abline(mod, col = 'red')
  text(30, 9, adj = c(0.5, 0.5), labels = 'p = 0.00015\nadj. R2 = 0.0168')
  
  
OUT3 <- NULL
plot(0,0, xlim = c(min(OUT[,2]), max(OUT[,2])), ylim = c(0, 10), xlab = 'Number of reproductive females',
     ylab = 'Number of offspring (by individual)')
  for(r in 1:nrow(sub)){
    if(sub$num.off[r] > 0){
      points(OUT[OUT[,1] == sub$year[r], 2], sub$num.surv[r], col = alpha('black', 0.3), pch = 19)
      save <- c(OUT[OUT[,1] == sub$year[r], 2], sub$num.surv[r])
      OUT3 <- rbind(OUT3, save)
    }
  }
  mod <- lm(OUT3[,3] ~ OUT3[,1])
  summary(mod)
  abline(mod, col = 'red')
  text(20, 9, adj = c(0.5, 0.5), labels = 'p = 0.0207\nadj. R2 = 0.0096')

dev.off()



##### >> Summer rainy #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 1,]        ## subset to summer rainy season summary stats
nb4 <- glm.nb(num.off ~ mean.b, 
              data = sub)
summary(nb4)

plot(sub$mean.b, sub$num.off, pch = 19, col = alpha(b.col, 0.8), xlab = 'Mean summer rainy season brightness',
     ylab = 'Number of offspring')

# plot(sub$mean.b, sub$num.off, pch = 19, col = alpha(b.col, 0.4))
#   new.x <- as.data.frame(seq(min(sub$mean.b), max(sub$mean.b), length.out = 100))
#   colnames(new.x) <- 'mean.b'
#   pred.vals <- predict(nb4, new.x, se.fit = TRUE)
#   low <- pred.vals$fit - pred.vals$se.fit*1.96
#   upp <- pred.vals$fit + pred.vals$se.fit*1.96
#   polygon(x = c(new.x$mean.b, sort(new.x$mean.b, decreasing = TRUE)),
#           y = c(low, sort(upp, decreasing = TRUE)), border = NA,
#           col = alpha(b.col, 0.4))
#   lines(new.x$mean.b, pred.vals$fit, col = b.col, lwd = 2)
#   
#   ## CI polygon
#   polygon(x = c(new.x$mean.b, sort(new.x$mean.b, decreasing = TRUE)),
#           y = c(pred.vals[,2], sort(pred.vals[,3], decreasing = TRUE)), border = NA,
#           col = alpha(w.col, 0.4))
#   lines(new.x$wetness.mean, pred.vals[,1], col = w.col, lwd = 1.5)

# pdf('/Users/Avril/Desktop/2_x_2_summer_brightness_v_numoff_effects.pdf', width = 4.5, height = 4.5)
effect_plot(nb4, pred = mean.b, interval = TRUE, plot.points = TRUE, partial.residuals = FALSE, 
            colors = b.col, point.alpha = 0.6, point.size = 2, centered = 'all') +
  theme_bw() + 
  labs(x = 'Mean summer rainy season brightness', y = 'Number of offspring') +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
        panel.grid.major = element_blank(), text=element_text(color="black"),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12, colour = 'black'),
        axis.title=element_text(size=12))
# dev.off()

## t-test for 0 vs. >0 surviving offspring, brightness
sub[sub$num.surv == 0, 'cat.surv'] <- 0
sub[sub$num.surv > 0, 'cat.surv'] <- 1
n <- nrow(sub[!is.na(sub$mean.b),])
m1 <- mean(sub[sub$cat.surv == 0, 'mean.b'], na.rm = TRUE)
sd1 <- sd(sub[sub$cat.surv == 0, 'mean.b'], na.rm = TRUE)/sqrt(n)*1.96
m2 <- mean(sub[sub$cat.surv == 1, 'mean.b'], na.rm = TRUE)
sd2 <- sd(sub[sub$cat.surv == 1, 'mean.b'], na.rm = TRUE)/sqrt(n)*1.96

pdf('/Users/Avril/Desktop/t-test_catnumoff_v_annual_brightness.pdf', width = 5, height = 6)
plot(jitter(sub$cat.surv, factor = 0.8), sub$mean.b, 
     # pch = 19, col = alpha(b.col, 0.5),  ## with points() below, toggle based on whether points should be in back- or foreground
     pch = 19, col = 'transparent',
     xlim = c(-0.4, 1.4),
     xlab = 'Number of offspring',
     ylab = 'Mean annual brightness', xaxt = 'n')
axis(1, at = c(0,1), labels = c('0','> 0'))
lines(x = c(-0.2, 0.2), 
      y = c(m1, m1),
      col = b.col, lwd = 3)
polygon(x = c(-0.15, 0.15, 0.15, -0.15),
        y = c(m1-sd1, m1-sd1, m1+sd1, m1+sd1),
        border = NA, col = alpha(b.col, 0.6))
lines(x = c(0.8, 1.2), 
      y = c(m2, m2),
      col = b.col, lwd = 3)
polygon(x = c(0.85, 1.15, 1.15, 0.85),
        y = c(m2-sd2, m2-sd2, m2+sd2, m2+sd2),
        border = NA, col = alpha(b.col, 0.6))
points(jitter(sub$cat.surv, factor = 0.8), sub$mean.b, 
       pch = 19, col = alpha(b.col, 0.5), cex = 0.75)
dev.off()

shapiro.test(sub[sub$cat.surv == 0, 'mean.b']) ## both normally distributed
shapiro.test(sub[sub$cat.surv == 1, 'mean.b']) 
bartlett.test(sub$mean.b ~ sub$cat.surv) ## p > 0.05, equal variances
t.test(sub[sub$cat.surv == 0, 'mean.b'], sub[sub$cat.surv == 1, 'mean.b'],
       paired = FALSE, var.equal = TRUE) ## p = 0.01966, sig different


##### >> Winter rainy #####
sub <- env.fem.ann[env.fem.ann$type == 2 & env.fem.ann$season == 2,]        ## subset to winter rainy season summary stats
sub$mean.tempk <- sub$mean.tempk - 273.15
nb3 <- glm.nb(num.off ~ mean.w + mean.tempk, 
              data = sub)
summary(nb3)
cor(sub$mean.w, sub$mean.tempk) ## predictors correlation coefficient = 0.03

plot(sub$mean.w, sub$num.off, pch = 19, col = alpha(w.col, 0.8), xlab = 'Mean winter rainy season wetness',
     ylab = 'Number of offspring')
plot(sub$mean.tempk, sub$num.off, pch = 19, col = alpha(t.col, 0.8), xlab = 'Mean winter rainy season surface temperature (deg C)',
     ylab = 'Number of offspring')

pdf('/Users/Avril/Desktop/winter_surftemp_v_numoff_effects.pdf', width = 3, height = 4)
effect_plot(nb3, pred = mean.tempk, interval = TRUE, plot.points = TRUE, partial.residuals = FALSE, 
            colors = t.col, point.alpha = 0.6, centered = 'all') +
  theme_bw() + 
  scale_y_continuous(breaks=seq(0,10,2)) +
  labs(x = 'Mean winter rainy season surface temperature (deg C)', y = 'Number of offspring') +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_blank(), text=element_text(color="black"),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12, colour = 'black'),
        axis.title=element_text(size=12))
dev.off()

pdf('/Users/Avril/Desktop/2_x_2_winter_wetness_v_numoff_effects.pdf', width = 4.5, height = 4.5)
effect_plot(nb3, pred = mean.w, interval = TRUE, plot.points = TRUE, partial.residuals = FALSE, 
            colors = w.col, point.alpha = 0.6, point.size = 2, centered = 'all') +
  theme_bw() + 
  labs(x = 'Mean winter rainy season wetness', y = 'Number of offspring') +
  scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), 
        panel.grid.major = element_blank(), text=element_text(color="black"),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12, colour = 'black'),
        axis.title=element_text(size=12))
dev.off()

