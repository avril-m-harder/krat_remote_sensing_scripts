library(ggmap)
library(sp)
library(raster)
library(RStoolbox)
library(viridis)
library(ggplot2)
library(scales)
options("rgdal_show_exportToProj4_warnings"="none")
library(rgdal)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(sf)
library(ggsn)
library(smoothr)
library(tiff)
library(jpeg)
library(rgeos)
library(rasterVis)
library(dplyr)
library(geodata)


mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
mnd.locs <- mnd.locs[c('long','lat','database.name')] ## rearrange/rename to match man.locs below
colnames(mnd.locs)[c(1,3)] <- c('lon','terr')
mnd.pts <- st_as_sf(x = mnd.locs, coords = c('lon','lat'), crs = 4326)
mnd.pts <- st_transform(mnd.pts, crs = 3857)

## get landcover data into correct projection
setwd('/Users/Avril/Desktop/nlcd_download_site_only/')
r <- raster('NLCD_2001_Land_Cover_L48_20210604_LhnhY7xVlgqyaKshEcu0.tiff')
## get unique LC types
for(c in c(1:ncol(r))){
  if(c == 1){
    save <- unique(r[,c])
  } else{
    save <- unique(r[,c], save)
  }
}
save <- unique(save) ## 3 types:
table(save)          ## 21 - Developed, Open Space (roads)
                     ## 52 - Shrub/Scrub
                     ## 71 - Grasslands/Herbaceous

## create separate rasters for different cover types
dev <- r
dev[dev != 21] <- NA
shr <- r
shr[shr != 52] <- NA
gra <- r
gra[gra != 71] <- NA

## re-project the mound locations and just using the land cover projection system
reproj.mnd.pts <- st_transform(mnd.pts, crs = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_def")
temp <- reproj.mnd.pts$geometry
temp <- as.data.frame(temp)
temp$geometry <- gsub('POINT (-','',temp$geometry, fixed = TRUE)
temp$geometry <- gsub('c(', '', temp$geometry, fixed = TRUE)
temp$geometry <- gsub(')', '', temp$geometry, fixed = TRUE)
points <- do.call(rbind, strsplit(temp$geometry, split = ', ', fixed = TRUE))
colnames(points) <- c('x','y')

pdf('/Users/Avril/Desktop/landcover_mounds_zoomout.pdf', width = 9, height = 9)
plot(rasterToPolygons(gra, dissolve=TRUE), add=FALSE, border='transparent', lwd=0.25, col = 'wheat2')
  plot(rasterToPolygons(shr, dissolve=TRUE), add=TRUE, border='transparent', lwd=0.25, col = 'darkkhaki')
  points(points[,1], points[,2], pch = 19, cex = 0.15)
dev.off()

offset <- 200
points <- as.data.frame(points)
points$x <- as.numeric(points$x)
points$y <- as.numeric(points$y)
pdf('/Users/Avril/Desktop/landcover_mounds_zoomin.pdf', width = 9, height = 9)
plot(rasterToPolygons(gra, dissolve=TRUE), add=FALSE, border='transparent', lwd=0.1, col = 'wheat2',
     xlim = c(min(points$x) - offset, max(points$x + offset)),
     ylim = c(min(points$y) - offset, max(points$y + offset)))
  plot(rasterToPolygons(shr, dissolve=TRUE), add=TRUE, border='transparent', lwd=0.1, col = 'darkkhaki')
  points(points$x, points$y, pch = 19, cex = 0.5)
dev.off()
