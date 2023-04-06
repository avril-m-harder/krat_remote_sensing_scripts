##### Read in data #####
##### !! All of these pop #s are taken from the census information, *not* from the pedigree information #####
## demographic data
pop.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_demo_data.csv')

## environmental data
prism.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/prism_annual_and_seasonal_means.csv')
tc.dat <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/occ_and_adj_cells_12monthinterval_means_seasoneq.csv')

## active moundsd
act.mnds <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/annual_active_mounds.csv')


temp <- merge(act.mnds, pop.dat, by = 'year')

k <- 1
while(k == 1){
pdf('/Users/Avril/Desktop/test_comps.pdf', width = 7.5, height = 7)
par(mar = c(5.1, 4.1, 1.1, 1.1), mfrow = c(2,2))
plot(temp$num.active.mnds, temp$tot, pch = 19, xlab = 'Number of active mounds', ylab = 'Census population size')
  mod <- lm(temp$tot ~ temp$num.active.mnds)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$num.active.mnds), max(temp$num.active.mnds)),
        c(lo, hi), col = 'grey40')
  legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                    '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')
  
plot(temp$num.active.mnds, temp$adt.fs, pch = 19, xlab = 'Number of active mounds', ylab = 'Number of adult females')
  mod <- lm(temp$adt.fs ~ temp$num.active.mnds)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$num.active.mnds), max(temp$num.active.mnds)),
        c(lo, hi), col = 'grey40')
  legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                    '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')
  
plot(temp$adt.fs, temp$num.off, pch = 19, xlab = 'Number of adult females', ylab = 'Number of offspring')
  mod <- lm(temp$num.off ~ temp$adt.fs)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$adt.fs), max(temp$adt.fs)),
        c(lo, hi), col = 'grey40')
  legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                    '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')
  
plot(temp$adt.fs, temp$num.surv, pch = 19, xlab = 'Number of adult females', ylab = 'Number of offspring surviving')
  mod <- lm(temp$num.surv ~ temp$adt.fs)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$adt.fs), max(temp$adt.fs)),
        c(lo, hi), col = 'grey40')
  legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                    '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')
  
plot(temp$adt.fs, temp$num.off.perf, pch = 19, xlab = 'Number of adult females', ylab = 'Number of offspring per female')
  mod <- lm(temp$num.off.perf ~ temp$adt.fs)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$adt.fs), max(temp$adt.fs)),
        c(lo, hi), col = 'grey40')
  legend('topright', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                     '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')
  
plot(temp$adt.fs, temp$num.surv.perf, pch = 19, xlab = 'Number of adult females', ylab = 'Number of offspring surviving per female')
  mod <- lm(temp$num.surv.perf ~ temp$adt.fs)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$adt.fs)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$adt.fs), max(temp$adt.fs)),
        c(lo, hi), col = 'grey40')
  legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                    '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')

plot(temp$num.active.mnds, temp$num.off.perf, pch = 19, xlab = 'Number of active mounds', ylab = 'Number of offspring per female')
  mod <- lm(temp$num.off.perf ~ temp$num.active.mnds)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$num.active.mnds), max(temp$num.active.mnds)),
        c(lo, hi), col = 'grey40')
  legend('topright', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                    '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')

plot(temp$num.active.mnds, temp$num.surv.perf, pch = 19, xlab = 'Number of active mounds', ylab = 'Number of offspring surviving per female')
  mod <- lm(temp$num.surv.perf ~ temp$num.active.mnds)
  summary(mod)
  shapiro.test(residuals(mod))
  ncvTest(mod)
  # abline(mod)
  lo <- min(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  hi <- max(temp$num.active.mnds)*coefficients(mod)[2] + coefficients(mod)[1]
  lines(c(min(temp$num.active.mnds), max(temp$num.active.mnds)),
        c(lo, hi), col = 'grey40')
  legend('topleft', legend = paste0('p = ',round(summary(mod)$coefficients[2,4], digits = 3),
                                    '\nAdj. R2 = ',round(summary(mod)$adj.r.squared, digits = 3)),
         bty = 'n')

dev.off()
k <- k+1
}

  
### just plot how everything changes over time
pdf('/Users/Avril/Desktop/test.pdf', width = 10, height = 6)
plot(temp$year, temp$tot, pch = 19, col = 'black', ylim = c(0,300), ylab = '', xlab = 'Year')
  lines(temp$year, temp$tot, col = 'black')
  points(temp$year, temp$adt.fs, pch = 19, col = 'darkorchid3')
  lines(temp$year, temp$adt.fs, pch = 19, col = 'darkorchid3')
  points(temp$year, temp$num.active.mnds, pch = 19, col = 'seagreen3')
  lines(temp$year, temp$num.active.mnds, col = 'seagreen3')
  points(temp$year, temp$num.off, pch = 19, col = 'lightskyblue')
  lines(temp$year, temp$num.off, col = 'lightskyblue')
  points(temp$year, temp$num.surv, pch = 19, col = 'royalblue3')
  lines(temp$year, temp$num.surv, col = 'royalblue3')
  legend('topright', legend = c('Total population','Active mounds','Adult females','Offspring','Offspring surviving'), pch = 19,
         col = c('black','seagreen3','darkorchid3','lightskyblue','royalblue3'), inset = 0.01)
  
plot(temp$year, temp$num.off.perf, pch = 19, col = 'lightskyblue', ylim = c(0, 3.5))
  lines(temp$year, temp$num.off.perf, col = 'lightskyblue')
  points(temp$year, temp$num.surv.perf, pch = 19, col = 'royalblue3')
  lines(temp$year, temp$num.surv.perf, col = 'royalblue3')
  legend('topleft', legend = c('Number offspring per female', 'Number surviving offspring per female'), pch = 19,
         col = c('lightskyblue','royalblue3'), inset = 0.01)  
  
## annual environmental means
sub.prism <- prism.dat[prism.dat$temporal.scale == 'annual',]
sub.tc <- tc.dat[tc.dat$temporal.scale == 'annual' & tc.dat$offspring.year >= 1994,]
  
plot(sub.tc$offspring.year, sub.tc$temp.k.mean.szneq, pch = 19, col = t.col,
     ylim = c(288, 314), xlab = 'Year', ylab = 'Mean active landscape surface temperature (K)')
  lines(sub.tc$offspring.year, sub.tc$temp.k.mean.szneq, col = t.col)
  
  ## winter rainy season data
  sub.tc <- tc.dat[tc.dat$temporal.scale == 'seasonal' & tc.dat$season == 2 & tc.dat$offspring.year >= 1994,]
  points(sub.tc$offspring.year, sub.tc$temp.k.mean, pch = 17, col = t.col)
  lines(sub.tc$offspring.year, sub.tc$temp.k.mean, col = t.col)
  
  
  legend('topleft', legend = c('Annual (July year t-1 -- June year t)','Winter rainy (Dec year t-1 -- March year t)'),
         pch = c(19,17), col = t.col, inset = 0.01)

dev.off()  
