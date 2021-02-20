library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
library(rgdal)
setwd('/Users/Avril/Documents/krat_remote_sensing/tc_output_tables/')

tc.fn <- 'tc_initial_test' ## for TC data
mc.fn <- 'mnd_key_initial_test' ## for mound/cell ID key

tc.dat <- read.csv(paste0(tc.fn,'.csv'))
mc.key <- read.csv(paste0(mc.fn,'.csv'))

## for a single cell, plot trends in TC metrics over year
temp <- tc.dat[which(tc.dat$cells == unique(tc.dat$cells)[1]),]
temp$year <- as.numeric(do.call(rbind, strsplit(as.character(temp$acq.date), split='-', fixed=TRUE))[,1])
plot(temp$doy, temp$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$brightness, pch=19, col=temp$year, cex=0.8)
## compare with another cell
temp1 <- tc.dat[which(tc.dat$cells == unique(tc.dat$cell)[300]),]
temp1$year <- as.numeric(do.call(rbind, strsplit(as.character(temp1$acq.date), split='-', fixed=TRUE))[,1])
plot(temp1$doy, temp1$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$brightness, pch=19, col=temp$year, cex=0.8)

par(mfrow=c(1,2))
plot(temp$doy, temp$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$greenness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$wetness, pch=19, col=temp$year, cex=0.8)
plot(temp$doy, temp$brightness, pch=19, col=temp$year, cex=0.8)
plot(temp1$doy, temp1$brightness, pch=19, col=temp$year, cex=0.8)

## plot mean values across extent for each scene/timepoint
mean.dat <- tc.dat[,-1] ## get rid of ID, because there are repeat cell values across mound IDs
mean.dat <- mean.dat[!duplicated(mean.dat),] ## get rid of duplicated rows (2,600,424 rows --> 383,616 rows)
mean.dat$year <- as.numeric(do.call(rbind, strsplit(as.character(mean.dat$acq.date), split='-', fixed=TRUE))[,1])
## loop over observations and take mean of all cell TC values for each timepoint
OUT <- NULL
for(i in unique(mean.dat$scene.id)){
  sub <- mean.dat[which(mean.dat$scene.id == i),]
  save <- c(mean(sub$greenness), mean(sub$wetness), mean(sub$brightness), sub$doy[1], sub$year[1], sub$path[1])
  OUT <- rbind(OUT, save)
}
out <- as.data.frame(OUT)
colnames(out) <- c('greenness','wetness','brightness','doy','year','path')
plot(out$doy, out$brightness, col=out$year, pch=19, cex=0.8)
## plot years separately
pdf('/Users/Avril/Desktop/test.pdf', height=18, width=9)
par(mfrow=c(9,2))
for(i in unique(out$year)){
  sub <- out[which(out$year == i),]
  plot(sub$doy, sub$wetness, col=sub$path, pch=19, cex=1, main=i)
}
dev.off()
