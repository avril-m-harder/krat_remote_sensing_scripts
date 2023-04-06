`%notin%` <- Negate(`%in%`)
library(scales)

##### Read in and filter data #####
## read in data from RQRES database file:
## Peter's definition: "Lists the mound that we determined to be each eartagged 
## individual’s primary residence during each quarter."
res <- read.csv('/Users/Avril/Documents/krat_remote_sensing/raw_data/peters_databases/RQRES.csv')
## limit to the years for which we have decent temporal Landsat coverage (i.e., beginning in 1994) &
## juvenile survival data (i.e., through 2005) -- turned off for now 
# res <- res[res$YEAR >= 1994 & res$YEAR <= 2005,]
## filter by population, using same list J did for krat2
res <- res[res$POP %in% c('R2','SSW','R1W','R1E'),]

## write dataframe with just number of active mounds per year
OUT <- NULL
for(y in unique(res$YEAR)){
  save <- c(y, length(unique(res[res$YEAR == y, 'RESIDENCE'])))
  OUT <- rbind(OUT, save)
}
colnames(OUT) <- c('year','num.active.mnds')
write.csv(OUT, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_active_mounds.csv', row.names = FALSE)

## make data frame of all active mounds
mnds <- res
##### !! convert this to a table of year / pop / residence #####
OUT <- NULL
for(y in unique(mnds$YEAR)){
  sub <- mnds[mnds$YEAR == y, c('YEAR','POP','RESIDENCE')]
  sub <- sub[!duplicated(sub),]
  OUT <- rbind(OUT, sub)
}
colnames(OUT) <- c('year','pop','database.name')
write.csv(OUT, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_occupied_mounds_all_indivs.csv',
          row.names = FALSE)

## for purpose of identifying occupied mounds, create new dataframe with only females and juveniles 
## (i.e., an attempt to capture all potential natal mounds / reproductive sites)
mnds <- res[which((res$AGE == 'A' & res$SEX == 'F') | (res$AGE == 'J')),]
##### !! convert this to a table of year / pop / residence #####
OUT <- NULL
for(y in unique(mnds$YEAR)){
  sub <- mnds[mnds$YEAR == y, c('YEAR','POP','RESIDENCE')]
  sub <- sub[!duplicated(sub),]
  OUT <- rbind(OUT, sub)
}
colnames(OUT) <- c('year','pop','database.name')
write.csv(OUT, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_occupied_mounds.csv',
          row.names = FALSE)

##### Calculate demographic summaries based on individual IDs #####
OUT <- NULL
for(i in unique(res$NAME)){
  sub <- res[res$NAME == i,]
  if(length(unique(sub$POP)) > 1){
    print(i)
  }
  b.year <- min(sub$YEAR)
  d.year <- max(sub$YEAR)
  ## determine whether focal individual was first observed as a juvenile or not
  obs.as.j <- sub[sub$YEAR == b.year, 'AGE'][1] == 'J'
  save <- c(i, b.year, d.year, (d.year-b.year), obs.as.j)
  OUT <- rbind(OUT, save)
}
demo.summary <- as.data.frame(OUT)
colnames(demo.summary) <- c('id','b.year','d.year','age.at.death','obs.as.j')
table(demo.summary[demo.summary$obs.as.j == 0, 'b.year'])
## individuals that were not observed as juveniles are distribution fairly evenly across years

##### Calculate annual numbers based on individual data #####
OUT <- NULL
for(y in unique(demo.summary$b.year)){
  sub <- demo.summary[demo.summary$b.year == y,]
  obs.as.j <- sub[sub$obs.as.j == 1,]
  ## for individuals first observed as juveniles in this year, calculate average age at death
  avg.aad <- mean(obs.as.j$age.at.death)
  ## number of offspring first observed as juvniles (born) in this year
  num.off <- length(unique(obs.as.j$id))
  ## number of offspring produced in this year that survive to at least 1 y.o.
  num.surv <- length(unique(obs.as.j[obs.as.j$age.at.death > 0, 'id']))
  ## calculate number of adult females
  adt.fs <- length(unique(res[which(res$AGE == 'A' & res$SEX == 'F' & res$YEAR == y), 'NAME']))
  ## calculate total pop size
  tot <- length(unique(res[which(res$YEAR == y), 'NAME']))
  save <- c(y, num.off, num.surv, avg.aad, adt.fs, tot)
  OUT <- rbind(OUT, save)
}
OUT <- OUT[order(OUT[,1]),]
ann.dat <- as.data.frame(OUT)
colnames(ann.dat) <- c('year','num.off','num.surv','avg.aad','adt.fs','tot')
ann.dat <- ann.dat[ann.dat$year >= 1994 & ann.dat$year <= 2005,]
ann.dat$num.off.perf <- ann.dat$num.off/ann.dat$adt.fs
ann.dat$num.surv.perf <- ann.dat$num.surv/ann.dat$adt.fs

write.csv(ann.dat, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_demo_data.csv',
          row.names = FALSE)

##### Calculate annual numbers *for populations* based on individual data? #####
##### !!! set this up !!! - not edited yet #####
# OUT <- NULL
# for(y in unique(demo.summary$b.year)){
#   sub <- demo.summary[demo.summary$b.year == y,]
#   obs.as.j <- sub[sub$obs.as.j == 1,]
#   ## for individuals first observed as juveniles in this year, calculate average age at death
#   avg.aad <- mean(obs.as.j$age.at.death)
#   ## number of offspring first observed as juvniles (born) in this year
#   num.off <- length(unique(obs.as.j$id))
#   ## number of offspring produced in this year that survive to at least 1 y.o.
#   num.surv <- length(unique(obs.as.j[obs.as.j$age.at.death > 0, 'id']))
#   ## calculate number of adult females
#   adt.fs <- length(unique(res[which(res$AGE == 'A' & res$SEX == 'F' & res$YEAR == y), 'NAME']))
#   ## calculate total pop size
#   tot <- length(unique(res[which(res$YEAR == y), 'NAME']))
#   save <- c(y, num.off, num.surv, avg.aad, adt.fs, tot)
#   OUT <- rbind(OUT, save)
# }
# OUT <- OUT[order(OUT[,1]),]
# ann.dat <- as.data.frame(OUT)
# colnames(ann.dat) <- c('year','num.off','num.surv','avg.aad','adt.fs','tot')
# ann.dat <- ann.dat[ann.dat$year >= 1994 & ann.dat$year <= 2005,]
# ann.dat$num.off.perf <- ann.dat$num.off/ann.dat$adt.fs
# ann.dat$num.surv.perf <- ann.dat$num.surv/ann.dat$adt.fs
# 
# write.csv(ann.dat, '/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_demo_data_by_pop.csv',
#           row.names = FALSE)