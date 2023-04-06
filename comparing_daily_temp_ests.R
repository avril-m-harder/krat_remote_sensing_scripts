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
tc.dat <- tc.dat[tc.dat$year <= 2005 & tc.dat >= 1993,]

## add season to tc.dat for season equalization
tc.dat[tc.dat$doy < 60 | tc.dat$doy >= 335, 'season'] <- 1
tc.dat[tc.dat$doy >= 60 & tc.dat$doy < 152, 'season'] <- 2
tc.dat[tc.dat$doy >= 152 & tc.dat$doy < 244, 'season'] <- 3
tc.dat[tc.dat$doy >= 244 & tc.dat$doy < 335, 'season'] <- 4

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

cols <- c('dodgerblue2',
  'springgreen3',
  'gold2',
  'sienna1')

pdf('/Users/Avril/Desktop/dailyests_landsat_v_prism.pdf', width = 6, height = 5)
plot(0, 0, xlim = c(5,68), ylim = c(-5,30), col = 'transparent', xlab = 'Landsat surface temperature', ylab = 'PRISM mean temperature')
  OUT <- NULL
  for(s in unique(tc.dat$scene.id)){
    sub <- tc.dat[tc.dat$scene.id == s,]
    year <- sub$year[1]
    doy <- sub$doy[1]
    sea <- sub$season[1]
    col <- cols[sea]
    points(mean(sub$temp.k - 273.15), prism.dat[prism.dat$year == year & prism.dat$doy == doy, 'mean.t.degC'], col = col, pch = 19, cex = 0.75)
    save <- c(mean(sub$temp.k - 273.15), prism.dat[prism.dat$year == year & prism.dat$doy == doy, 'mean.t.degC'])
    OUT <- rbind(OUT, save)
  }
dat <- as.data.frame(OUT)
colnames(dat) <- c('landsat','prism')
mod <- lm(prism ~ landsat, data = dat)
summary(mod)

new.x <- as.data.frame(seq(min(dat$landsat), max(dat$landsat), length.out = 50))
colnames(new.x) <- 'landsat'
pred.vals <- predict(mod, new.x, interval = 'confidence')
pred.vals <- pred.vals[order(pred.vals[,1]),]
## CI polygon
polygon(x = c(new.x$landsat, sort(new.x$landsat, decreasing = TRUE)),
        y = c(pred.vals[,2], sort(pred.vals[,3], decreasing = TRUE)), border = NA,
        col = alpha('grey', 0.4))
lines(new.x$landsat, pred.vals[,1], col = 'grey', lwd = 2)
legend('bottomright', pch = 19, pt.cex = 0.75, legend = c('Winter','Spring','Summer','Autumn'), col = cols, bty = 'n', inset = 0.03)
legend('topleft', legend = c('p < 2.2e-16\nadj. R2 = 0.5262'), bty = 'n', inset = 0.01)
dev.off()

shapiro.test(residuals(mod)) ## significant (p = 0.02)
