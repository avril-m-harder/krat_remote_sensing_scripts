library(scales)
library(lmtest)
library(beyonce)
`%notin%` <- Negate(`%in%`)

## Beyonce palette option (only need wetness, brightness, surface temp)
w.col <- beyonce_palette(75)[2]
b.col <- beyonce_palette(75)[5]
t.col <- beyonce_palette(75)[7]
cols <- c(w.col, b.col, t.col)
names <- c('wetness','brightness','temp.k')

##### Read in data #####
## demographic data
pop.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_demo_data.csv')

## environmental data
prism.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/prism_annual_and_seasonal_means.csv')
tc.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means_seasoneq.csv')

## active mounds
act.mnds <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_active_mounds.csv')

##### Trying total number of active mounds as the response #####
##### >> Annual data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual',]

temp.dat <- merge(act.mnds, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(5:8,15,18,21,24)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_annual_activemnds.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.active.mnds, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of active mounds')
  mod <- lm(temp.dat$num.active.mnds ~ temp.dat[,c])
  # mod <- glm(temp.dat$num.active.mnds ~ temp.dat[,c], family = 'poisson')
  if(summary(mod)$coefficients[2,4] < 0.05){
    print(names[k])
    abline(mod)
  }
  k <- k+1
}
dev.off()

##### >> Summer rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 1,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 1,]

temp.dat <- merge(act.mnds, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(5:8,13,16,19,22)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_summer_activemnds.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.active.mnds, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of active mounds')
  mod <- lm(temp.dat$num.active.mnds ~ temp.dat[,c])
  # mod <- glm(temp.dat$num.active.mnds ~ temp.dat[,c], family = 'poisson')
  if(summary(mod)$coefficients[2,4] < 0.05){
    print(names[k])
    abline(mod)
  }
  k <- k+1
}
dev.off()

##### >> Winter rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 2,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 2,]

temp.dat <- merge(act.mnds, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(5:8,13,16,19,22)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_winter_activemnds.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.active.mnds, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of active mounds')
  mod <- lm(temp.dat$num.active.mnds ~ temp.dat[,c])
  # mod <- glm(temp.dat$num.active.mnds ~ temp.dat[,c], family = 'poisson')
  if(summary(mod)$coefficients[2,4] < 0.05){
    print(names[k])
    abline(mod)
  }
  k <- k+1
}
dev.off()


##### Change in population size - absolute #####
##### >> Annual data #####
## annual environmental data = July year t-1 --> June year t
## population data = population size in year t or change from year t-1 to year t
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual',]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')
for(r in 2:nrow(temp.dat)){
  temp.dat$change[r] <- temp.dat$tot[r]-temp.dat$tot[r-1]
}

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,21,24,27,30)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_annual_popsize_popchange.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$tot, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Population size')
  mod <- lm(temp.dat$tot ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$change, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Change in population size')
  mod <- lm(temp.dat$change ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
dev.off()

##### >> Summer rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 1,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 1,]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')
for(r in 2:nrow(temp.dat)){
  temp.dat$change[r] <- temp.dat$tot[r]-temp.dat$tot[r-1]
}

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,19,22,25,28)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_summer_popsize_popchange.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$tot, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Population size')
  mod <- lm(temp.dat$tot ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$change, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Change in population size')
  mod <- lm(temp.dat$change ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
dev.off()

##### >> Winter rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 2,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 2,]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')
for(r in 2:nrow(temp.dat)){
  temp.dat$change[r] <- temp.dat$tot[r]-temp.dat$tot[r-1]
}

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,19,22,25,28)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_winter_popsize_popchange.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$tot, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Population size')
  mod <- lm(temp.dat$tot ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$change, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Change in population size')
  mod <- lm(temp.dat$change ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
dev.off()

##### Change in population size - proportional #####
##### >> Annual data #####
## annual environmental data = July year t-1 --> June year t
## population data = population size in year t or change from year t-1 to year t
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual',]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')
temp.dat <- temp.dat[order(temp.dat$year),]
for(r in 2:nrow(temp.dat)){
  temp.dat$change[r] <- (temp.dat$tot[r]-temp.dat$tot[r-1])/temp.dat$tot[r-1]
}

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,21,24,27,30)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_annual_popsize_popchange.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$tot, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Population size')
  mod <- lm(temp.dat$tot ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$change, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Change in population size')
  mod <- lm(temp.dat$change ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
dev.off()

##### >> Summer rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 1,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 1,]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')
temp.dat <- temp.dat[order(temp.dat$year),]

for(r in 2:nrow(temp.dat)){
  temp.dat$change[r] <- (temp.dat$tot[r]-temp.dat$tot[r-1])/temp.dat$tot[r-1]
}

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,19,22,25,28)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_summer_popsize_popchange.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$tot, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Population size')
  mod <- lm(temp.dat$tot ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$change, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Change in population size')
  mod <- lm(temp.dat$change ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
dev.off()

##### >> Winter rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 2,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 2,]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')
temp.dat <- temp.dat[order(temp.dat$year),]

for(r in 2:nrow(temp.dat)){
  temp.dat$change[r] <- (temp.dat$tot[r]-temp.dat$tot[r-1])/temp.dat$tot[r-1]
}

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,19,22,25,28)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_winter_popsize_popchange.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$tot, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Population size')
  mod <- lm(temp.dat$tot ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$change, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Change in population size')
  mod <- lm(temp.dat$change ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
    print(c)
  }
  k <- k+1
}
dev.off()


##### Average # offspring per female #####
## num.off.perf ~ environmental data from offspring.year
##### >> Annual data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual',]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,21,24,27,30)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_annual.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.off.perf, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of offspring per female')
  mod <- lm(temp.dat$num.off.perf ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
  }
  k <- k+1
}
dev.off()



##### >> Summer rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 1,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 1,]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,19,22,25,28)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_summer.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.off.perf, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of offspring per female')
  mod <- lm(temp.dat$num.off.perf ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
  }
  k <- k+1
}
dev.off()


##### >> Winter rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 2,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 2,]

temp.dat <- merge(pop.dat, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(11:14,19,22,25,28)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_winter.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.off.perf, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of offspring per female')
  mod <- lm(temp.dat$num.off.perf ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
  }
  k <- k+1
}
dev.off()


##### Average # surviving offspring per female #####
## num.surv.perf ~ environmental data from offspring.year + 1
##### >> Annual data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual',]

temp.dat <- pop.dat
temp.dat$surv.year <- temp.dat$year + 1
temp.dat <- merge(temp.dat, sub.prism, by.x = 'surv.year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'surv.year', by.y = 'offspring.year')


## define columns with environmental data to be used as predictor variables
cols <- c(12:15,22,25,28,31)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_annual_numsurv.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.surv.perf, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of surviving offspring per female')
  mod <- lm(temp.dat$num.surv.perf ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
  }
  k <- k+1
}
dev.off()


##### >> Summer rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 1,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 1,]

temp.dat <- pop.dat
temp.dat$surv.year <- temp.dat$year + 1
temp.dat <- merge(temp.dat, sub.prism, by.x = 'surv.year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'surv.year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(12:15,20,23,26,29)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_summer_numsurv.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.surv.perf, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of surviving offspring per female')
  mod <- lm(temp.dat$num.surv.perf ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
  }
  k <- k+1
}
dev.off()
 
# ## backwards stepwise regression
# mod1 <- lm(temp.dat$num.surv.perf ~ temp.dat$total.precip.mm + temp.dat$mean.t.degC + temp.dat$mean.min.t.degC +
#              temp.dat$mean.max.t.degC + temp.dat$greenness.mean + temp.dat$brightness.mean + temp.dat$temp.k.mean +
#              temp.dat$wetness.mean)
# summary(mod1) ## this is shit
# 
# temp.dat <- temp.dat[-c(6,7,9),]
# mod1 <- lm(num.surv.perf ~ wetness.mean, data = temp.dat) ## p = 0.03, R^2 = 0.35
# summary(mod1)
# plot(mod1)
# shapiro.test(residuals(mod1)) ## residuals = normal
# 
# pdf('/Users/Avril/Desktop/poplevel_summerrainy_numsurv_wetness.pdf', width = 5, height = 5)
# par(mar = c(4.1, 4.1, 3.1, 2.1))
# plot(temp.dat$wetness.mean, temp.dat$num.surv.perf, pch = 19, col = w.col, ylim = c(0.07, 1.24),
#      xlab = 'Mean wetness', ylab = 'Number surviving offspring per female', main = 'Summer rainy season')
# 
#   new.x <- as.data.frame(seq(min(temp.dat$wetness.mean), max(temp.dat$wetness.mean), length.out = 100))
#   colnames(new.x) <- 'wetness.mean'
#   pred.vals <- predict(mod1, new.x, interval = 'confidence')
#   pred.vals <- pred.vals[order(pred.vals[,1]),]
# 
#   ## CI polygon
#   polygon(x = c(new.x$wetness.mean, sort(new.x$wetness.mean, decreasing = TRUE)),
#           y = c(pred.vals[,2], sort(pred.vals[,3], decreasing = TRUE)), border = NA,
#           col = alpha(w.col, 0.4))
#   lines(new.x$wetness.mean, pred.vals[,1], col = w.col, lwd = 1.5)
# dev.off()
# 
# 
# mod2 <- lm(num.surv.perf ~ brightness.mean, data = temp.dat) ## p = 0.01, R^2 = 0.47
# summary(mod2)
# plot(mod2)
# shapiro.test(residuals(mod2)) ## residuals = normal
# 
# pdf('/Users/Avril/Desktop/poplevel_summerrainy_numsurv_brightness.pdf', width = 5, height = 5)
# par(mar = c(4.1, 4.1, 3.1, 2.1))
# plot(temp.dat$brightness.mean, temp.dat$num.surv.perf, pch = 19, col = b.col, ylim = c(0.07, 1.24),
#      xlab = 'Mean brightness', ylab = 'Number surviving offspring per female', main = 'Summer rainy season')
# 
#   new.x <- as.data.frame(seq(min(temp.dat$brightness.mean), max(temp.dat$brightness.mean), length.out = 100))
#   colnames(new.x) <- 'brightness.mean'
#   pred.vals <- predict(mod2, new.x, interval = 'confidence')
#   pred.vals <- pred.vals[order(pred.vals[,1]),]
# 
#   ## CI polygon
#   polygon(x = c(new.x$brightness.mean, sort(new.x$brightness.mean, decreasing = TRUE)),
#           y = c(pred.vals[,2], sort(pred.vals[,3], decreasing = TRUE)), border = NA,
#           col = alpha(b.col, 0.4))
#   lines(new.x$brightness.mean, pred.vals[,1], col = b.col, lwd = 1.5)
# dev.off()
# 
# mod3 <- lm(temp.dat$num.surv.perf ~ temp.dat$wetness.mean + temp.dat$brightness.mean)
# summary(mod3)
# lrtest(mod1, mod2)
# lrtest(mod1, mod3)
# lrtest(mod2, mod3)
# plot(mod3)
# 
##### >> Winter rainy season data #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 2,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 2,]

temp.dat <- pop.dat
temp.dat$surv.year <- temp.dat$year + 1
temp.dat <- merge(temp.dat, sub.prism, by.x = 'surv.year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'surv.year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(12:15,20,23,26,29)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

pdf('/Users/Avril/Desktop/pop-level_dat_winter_numsurv.pdf', width = 12, height = 6)
par(mfrow = c(2,4), mar = c(4.6, 4.1, 1.1, 1.1))
k <- 1
for(c in cols){
  plot(temp.dat[,c], temp.dat$num.surv.perf, pch = 19, col = 'springgreen4', xlab = names[k],
       ylab = 'Number of offspring per female')
  mod <- lm(temp.dat$num.surv.perf ~ temp.dat[,c])
  if(summary(mod)$coefficients[2,4] < 0.05){
    abline(mod)
  }
  k <- k+1
}
dev.off()
# 
# mod1 <- lm(temp.dat$num.surv.perf ~ temp.dat$total.precip.mm + temp.dat$mean.t.degC + temp.dat$mean.min.t.degC +
#              temp.dat$mean.max.t.degC + temp.dat$greenness.mean + temp.dat$brightness.mean + temp.dat$temp.k.mean +
#              temp.dat$wetness.mean)
# summary(mod1) ## p = 0.8
# 


##### Resampling tests for summer numsurv wetness & brightness #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'seasonal' & prism.dat$season == 1,]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 1,]

temp.dat <- pop.dat
temp.dat$surv.year <- temp.dat$year + 1
temp.dat <- merge(temp.dat, sub.prism, by.x = 'surv.year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'surv.year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(12:15,20,23,26,29)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

mod1 <- lm(num.surv.perf ~ brightness.mean, data = temp.dat)
mod2 <- lm(num.surv.perf ~ wetness.mean, data = temp.dat)
mod3 <- lm(num.surv.perf ~ brightness.mean + wetness.mean, data = temp.dat)

lrtest(mod1, mod3)
lrtest(mod2, mod3)
## combined model is better than either separate model
comb.int <- as.numeric(mod3$coefficients[1])
comb.bri <- as.numeric(mod3$coefficients[2])
comb.wet <- as.numeric(mod3$coefficients[3])

## save separate model coefficients, too
bri.int <- as.numeric(mod1$coefficients[1])
sep.bri <- as.numeric(mod1$coefficients[2])
wet.int <- as.numeric(mod2$coefficients[1])
sep.wet <- as.numeric(mod2$coefficients[2])

OUT <- NULL
for(r in 1:1000){
  ys <- sample(temp.dat$num.surv.perf, size = nrow(temp.dat), replace = FALSE)
  mod <- lm(ys ~ temp.dat$brightness.mean + temp.dat$wetness.mean)
  int <- as.numeric(mod$coefficients[1])
  bri <- as.numeric(mod$coefficients[2])
  wet <- as.numeric(mod$coefficients[3])
  save <- c(int, bri, wet)
  OUT <- rbind(OUT, save)
}

lev <- 0.05
plot(density(OUT[,1]))
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = comb.int, col = 'red')
  nrow(OUT[OUT[,1] >= comb.int,])/nrow(OUT) ## 0.003
plot(density(OUT[,2]))  
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = comb.bri, col = 'red')
  nrow(OUT[OUT[,2] >= comb.bri,])/nrow(OUT) ## 0.092
plot(density(OUT[,3]))  
  abline(v = quantile(OUT[,3], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,3], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = comb.wet, col = 'red')
  nrow(OUT[OUT[,3] >= comb.wet,])/nrow(OUT) ## 0.15
## doesn't look good for the combined model.
  
## try it for the separate models, then.
## brightness
OUT <- NULL
for(r in 1:1000){
  ys <- sample(temp.dat$num.surv.perf, size = nrow(temp.dat), replace = FALSE)
  mod <- lm(ys ~ temp.dat$brightness.mean)
  int <- as.numeric(mod$coefficients[1])
  bri <- as.numeric(mod$coefficients[2])
  save <- c(int, bri)
  OUT <- rbind(OUT, save)
}
lev <- 0.05
plot(density(OUT[,1]))
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = bri.int, col = 'red')
  nrow(OUT[OUT[,1] >= comb.int,])/nrow(OUT) ## 0
plot(density(OUT[,2]))  
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = sep.bri, col = 'red')
  nrow(OUT[OUT[,2] >= sep.bri,])/nrow(OUT) ## 0.004
summary(mod1)

pdf('/Users/Avril/Desktop/poplevel_brightness.pdf', width = 5, height = 5)
plot(0, 0, col = 'transparent', xlim = c(min(temp.dat$brightness.mean), max(temp.dat$brightness.mean)),
     ylim = c(min(temp.dat$num.surv.perf), 1.15), xlab = 'Mean summer rainy season brightness',
     ylab = 'Number offspring surviving per female')
  
  new.x <- as.data.frame(seq(min(temp.dat$brightness.mean), max(temp.dat$brightness.mean), length.out = 100))
  colnames(new.x) <- 'brightness.mean'
  pred.vals <- predict(mod1, new.x, interval = 'confidence')
  pred.vals <- pred.vals[order(pred.vals[,1]),]
  
  ## CI polygon
  polygon(x = c(new.x$brightness.mean, sort(new.x$brightness.mean, decreasing = TRUE)),
          y = c(pred.vals[,2], sort(pred.vals[,3], decreasing = TRUE)), border = NA,
          col = alpha(b.col, 0.4))
  lines(new.x$brightness.mean, pred.vals[,1], col = b.col, lwd = 1.5)
  points(temp.dat$brightness.mean, temp.dat$num.surv.perf, pch = 19, col = b.col)
dev.off()
  
## wetness
OUT <- NULL
for(r in 1:1000){
  ys <- sample(temp.dat$num.surv.perf, size = nrow(temp.dat), replace = FALSE)
  mod <- lm(ys ~ temp.dat$wetness.mean)
  int <- as.numeric(mod$coefficients[1])
  wet <- as.numeric(mod$coefficients[2])
  save <- c(int, wet)
  OUT <- rbind(OUT, save)
}
lev <- 0.05
plot(density(OUT[,1]))
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = wet.int, col = 'red')
  nrow(OUT[OUT[,1] >= wet.int,])/nrow(OUT) ## 0.008
plot(density(OUT[,2]))  
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = sep.wet, col = 'red')
  nrow(OUT[OUT[,2] >= sep.wet,])/nrow(OUT) ## 0.008

summary(mod2)

pdf('/Users/Avril/Desktop/poplevel_wetness.pdf', width = 5, height = 5)
plot(0, 0, col = 'transparent', xlim = c(min(temp.dat$wetness.mean), max(temp.dat$wetness.mean)),
     ylim = c(min(temp.dat$num.surv.perf), 1.2), xlab = 'Mean summer rainy season wetness',
     ylab = 'Number offspring surviving per female')

  new.x <- as.data.frame(seq(min(temp.dat$wetness.mean), max(temp.dat$wetness.mean), length.out = 100))
  colnames(new.x) <- 'wetness.mean'
  pred.vals <- predict(mod2, new.x, interval = 'confidence')
  pred.vals <- pred.vals[order(pred.vals[,1]),]
  
  ## CI polygon
  polygon(x = c(new.x$wetness.mean, sort(new.x$wetness.mean, decreasing = TRUE)),
          y = c(pred.vals[,2], sort(pred.vals[,3], decreasing = TRUE)), border = NA,
          col = alpha(w.col, 0.4))
  lines(new.x$wetness.mean, pred.vals[,1], col = w.col, lwd = 1.5)
  points(temp.dat$wetness.mean, temp.dat$num.surv.perf, pch = 19, col = w.col)
dev.off()


##### Resampling tests for annual mean surface temp vs. # active mounds #####
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual',]

temp.dat <- merge(act.mnds, sub.prism, by.x = 'year', by.y = 'offspring.year')
temp.dat <- merge(temp.dat, sub.tc, by.x = 'year', by.y = 'offspring.year')

## define columns with environmental data to be used as predictor variables
cols <- c(5:8,15,18,21,24)
colnames(temp.dat)[cols]
names <- c('Precipitation (mm)','Mean temperature (C)','Min temperature (C)','Max temperature (C)','Greenness','Wetness','Brightness','Temp K')

temp.dat$temp.k.mean.szneq <- temp.dat$temp.k.mean.szneq - 273.15
mod1 <- lm(num.active.mnds ~ temp.k.mean.szneq, data = temp.dat)
temp.int <- as.numeric(mod1$coefficients[1])
temp.slo <- as.numeric(mod1$coefficients[2])


OUT <- NULL
for(r in 1:10000){
  ys <- sample(temp.dat$num.active.mnds, size = nrow(temp.dat), replace = FALSE)
  mod <- lm(ys ~ temp.dat$temp.k.mean.szneq)
  int <- as.numeric(mod$coefficients[1])
  temp <- as.numeric(mod$coefficients[2])
  save <- c(int, temp)
  OUT <- rbind(OUT, save)
}
lev <- 0.05
plot(density(OUT[,1]))
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,1], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = temp.int, col = 'red')
nrow(OUT[OUT[,1] >= temp.int,])/nrow(OUT) ## 0.0215
plot(density(OUT[,2]))  
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[1], col = 'blue')
  abline(v = quantile(OUT[,2], probs = c((lev/2), 1-(lev/2)))[2], col = 'blue')
  abline(v = temp.slo, col = 'red')
nrow(OUT[OUT[,2] <= temp.slo,])/nrow(OUT) ## 0.0215

summary(mod1)

# pdf('/Users/Avril/Desktop/activmnds_annualtempk.pdf', width = 5, height = 5)
plot(0, 0, col = 'transparent', xlim = c(min(temp.dat$temp.k.mean.szneq), max(temp.dat$temp.k.mean.szneq)),
     ylim = c(55, 190), xlab = 'Mean annual surface temp (deg C)',
     ylab = 'Number of active mounds', xaxt = 'n')
axis(1, at = c(30, 32, 34, 36, 38))

new.x <- as.data.frame(seq(min(temp.dat$temp.k.mean.szneq), max(temp.dat$temp.k.mean.szneq), length.out = 100))
colnames(new.x) <- 'temp.k.mean.szneq'
pred.vals <- predict(mod1, new.x, interval = 'confidence')
pred.vals <- pred.vals[order(pred.vals[,1]),]

## CI polygon
polygon(x = c(new.x$temp.k.mean.szneq, sort(new.x$temp.k.mean.szneq, decreasing = TRUE)),
        y = c(sort(pred.vals[,2], decreasing = TRUE), sort(pred.vals[,3], decreasing = FALSE)), border = NA,
        col = alpha(t.col, 0.4))
lines(new.x$temp.k.mean.szneq, sort(pred.vals[,1], decreasing = TRUE), col = t.col, lwd = 1.5)
points(temp.dat$temp.k.mean.szneq, temp.dat$num.active.mnds, pch = 19, col = t.col)
# dev.off()

library(jtools)
library(ggplot2)
pdf('/Users/Avril/Desktop/annual_surftemp_v_activemnds_effects.pdf', width = 3, height = 4)
effect_plot(mod1, pred = temp.k.mean.szneq, interval = TRUE, plot.points = TRUE, partial.residuals = FALSE, 
            colors = t.col, point.alpha = 0.6, centered = 'all') +
  theme_bw() + 
  labs(x = 'Mean annual surface temperature (deg C)', y = 'Number of active mounds') +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_blank(), text=element_text(color="black"),
        panel.grid.minor = element_blank(), 
        # axis.line = element_line(colour = "black"),
        axis.text=element_text(size=12, colour = 'black'),
        axis.title=element_text(size=12))
dev.off()
