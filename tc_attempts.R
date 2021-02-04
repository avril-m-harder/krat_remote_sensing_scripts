# install.packages('pROC')
# install.packages('raster')
# install.packages('RStoolbox')
library(raster)
library(RStoolbox)
library(viridis)


# ## Example ----------------
# data(lsat)
# lsat_tc <- tasseledCap(lsat[[c(1:5,7)]], sat="Landsat5TM")
# lsat_tc
# plot(lsat_tc)
# ## ------------------------

## trying with Level 1 data first ( level 2 data looked weird) ------------------
setwd('/Users/Avril/Desktop/remote_sensing_data/proposal_sat_map_stuff/LE07_L1TP_035038_20020817_20160928_01_T1/')
all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"),
                                full.names = TRUE)

test <- raster(all_landsat_bands[1])
plot(test, col=gray(0:100 / 100))

## manually select the Landsat7ETM bands you need for Tasseled Cap (1,2,3,4,5,7)
crit_bands <- all_landsat_bands[c(1:5,8)]
## stack the images
ls7_stack <- stack(crit_bands)
## turn it into a brick
ls7_brick <- brick(ls7_stack)
# ## view brick attributes
# ls7_brick
# ## plot each band in the brick
# plot(ls7_brick, col=gray(0:100/100))

## apply the Tasseled Cap transformation
ls7_tc <- tasseledCap(ls7_brick, sat='Landsat7ETM')
plot(ls7_tc)

## get a natural color image
raster::plotRGB(ls7_stack,r=3,g=2,b=1)

## set coordinates
lo.x <- 663500
hi.x <- 665600
lo.y <- 3497000
hi.y <- 3499750

par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
# plot(ls7_tc$wetness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n')
# pdf(file='/Users/Avril/Desktop/test.pdf', width=3.64, height=3.19)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
plot(ls7_tc$wetness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='wetness')
plot(ls7_tc$greenness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='greenness')
plot(ls7_tc$brightness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='brightness')
  polygon(x=c())
# plot(ls7_tc$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', xlim=c(675000, 835000), ylim=c(3495773, 3500750), main='greenness')
# plot(ls7_tc$brightness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', xlim=c(675000, 835000), ylim=c(3495773, 3500750), main='brightness')
coords <- extent(lo.x, hi.x, lo.y, hi.y)
raster::plotRGB(ls7_stack,r=3,g=2,b=1, ext=coords, axes=TRUE, bty='n')
dev.off()

##### Try looking at intra-annual variation in Level 2 data for 1999 #####
setwd('/Users/Avril/Desktop/remote_sensing_data/1999_C2_L1_data/')
dirs <- list.files()

pdf(file='/Users/Avril/Desktop/1999.pdf', width=6, height=6)
par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))

for(i in dirs){
  setwd(paste0('/Users/Avril/Desktop/remote_sensing_data/1999_C2_L1_data/',i,'/'))
  all_landsat_bands <- list.files(pattern = glob2rx("*TIF$"), full.names = TRUE)
  # md.file <- list.files(pattern=glob2rx("*MTL.txt"), full.names=TRUE)
  # m.data <- stackMeta(md.file)
  
  # test <- raster(all_landsat_bands[1])
  # plot(test, col=gray(0:100 / 100))
  
  ## manually select the Landsat5ETM bands you need for Tasseled Cap (1,2,3,4,5,7)
  crit_bands <- all_landsat_bands[c(1:5,7)]
  ## stack the images
  ls5_stack <- stack(crit_bands)
  
  ##### Error with reading in metadata prevents this for now, but needs to be solved for real analyses #####
  ### --> maybe try a function from a different package that can better handle MD file weirdness? 
  ## apply TOA correction 
  # ls5_cor_stack <- radCor(ls5_stack, method='apref', bandSet='full')
  
  ## turn it into a brick
  ls5_brick <- brick(ls5_stack)
  # ## view brick attributes
  # ls5_brick
  # ## plot each band in the brick
  # plot(ls5_brick, col=gray(0:100/100))
  
  ## apply the Tasseled Cap transformation
  ls5_tc <- tasseledCap(ls5_brick, sat='Landsat5TM')
  # plot(ls5_tc)
  
  ## get a natural color image
  # raster::plotRGB(ls5_stack,r=3,g=2,b=1)
  
  ## set coordinates
  lo.x <- 663500
  hi.x <- 665600
  lo.y <- 3497000
  hi.y <- 3499750
  
  # par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
  # plot(ls5_tc$wetness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n')
  # pdf(file='/Users/Avril/Desktop/test.pdf', width=6, height=6)
  # par(mar=c(3.1,2.1,2.1,1.1), mgp=c(1.5,.75,0))
  
  ## create consistent legend/color scale across figures within a measure (w/g/b)
  pal <- viridis ## other options: viridis  magma   plasma  inferno  cividis
  
  cuts <- seq(from=0, to=250, by=10) ## covers range of wetness values for 1999
  plot(ls5_tc$wetness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main=paste0('wetness\n',i),
       breaks=cuts, col=pal(length(cuts)))
  
  cuts <- seq(from=-80, to=50, by=5) ## covers range of greenness values for 1999
  plot(ls5_tc$greenness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main=paste0('greenness\n',i),
       breaks=cuts, col=pal(length(cuts)))
  
  cuts <- seq(from=50, to=550, by=20) ## covers range of brightness values for 1999
  plot(ls5_tc$brightness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main=paste0('brightness\n',i),
       breaks=cuts, col=pal(length(cuts)))
  
  # raster::plot(ls5_tc$brightness, bty='n', xlim=c(lo.x, hi.x), ylim=c(lo.y, hi.y), main='brightness', maxpixels=1e20, useRaster=TRUE)

  # plot(ls5_tc$greenness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', xlim=c(675000, 835000), ylim=c(3495773, 3500750), main='greenness')
  # plot(ls5_tc$brightness, xlab='UTM westing coordinate (m)', ylab='UTM northing coordinate (m)', bty='n', xlim=c(675000, 835000), ylim=c(3495773, 3500750), main='brightness')
  # coords <- extent(lo.x, hi.x, lo.y, hi.y)
  # raster::plotRGB(ls5_stack,r=3,g=2,b=1, ext=coords, axes=TRUE, bty='n')
  # dev.off()
  print(i)
}
dev.off()







