setwd('/Users/Avril/Documents/krat_remote_sensing/range_map_materials')

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

## read in range map shapefile
range <- st_read('redlist_species_data_5d4b0dfc-6c14-467a-a3f6-5d68c8ae2371/data_0.shp')
plot(range)

## prep state + country data
mex <- ne_countries(country='Mexico', returnclass = 'sf', scale=10)
us <- ne_countries(country='United States of America', returnclass = 'sf', scale=10)
us.mex <- st_intersection(us, mex)
smooth.us.mex <- smoothr::smooth(us.mex, method='densify')
border = st_cast(smooth.us.mex, to='POLYGON')


states <- ne_states(country=c('United States of America', 'Mexico'), returnclass = 'sf')
state.list <- c('Texas','New Mexico','Arizona','Nevada','California','Oklahoma','Colorado','Utah','Kansas')
states <- states[which(states$name %in% state.list | states$admin == 'Mexico'),]
mx.states <- states[which(states$admin == 'Mexico'),]
us.states <- states[which(states$admin != 'Mexico'),]

al <- 1
line.w <- 0.5
grey <- "gray95" ## other greens: springgreen4  #B5D383  mediumseagreen #ADF2CB #76C599
blue <- 'lightblue2' ## other blues: cadetblue1 #98CAC2  lightblue2  #DFF7FF
## range map
pdf('/Users/Avril/Desktop/map.pdf', width=6, height=6)
ggplot(data = states) +
  theme_classic() +
  theme(panel.background = element_rect(fill=alpha(blue, al)),
        axis.text=element_text(size=10, colour='black'),
        axis.title=element_blank(),
        panel.border=element_rect(fill=NA)) +
  ## state outlines
  geom_sf(fill=grey, col='grey80', lwd=0.25) + ## other greens: springgreen4  #B5D383
  ## US-Mexico border
  geom_sf(data = border, col='grey30', lwd=0.5) +
  ## add range shapefile
  geom_sf(data = range, col='cadetblue4', fill=alpha('cadetblue', 0.7), lwd=line.w) + ## other blues: cadetblue1 #98CAC2
  ## add site coords
  # geom_point(y=31.6166667, x=-109.25, pch=8) +
  ## add scalebar
  ggsn::scalebar(x.min=-115, x.max=-98, y.min=21, y.max=38,
                 dist=200, dist_unit='km', st.bottom=TRUE, st.color='black',
                 transform=TRUE, location='bottomleft', st.size=3, st.dist=.03) +
  coord_sf(xlim = c(-117, -98), ylim = c(20, 38), expand = FALSE)
dev.off()

## range map with study site point or krat
## set bounds for krat image
size <- 1.5
xmin <- -109.25 - size
xmax <- -109.25 + size
ymin <- 31.6166667 - size
ymax <- 31.6166667 + size

rat <- readTIFF('IMG_1944.TIF', native=FALSE)
r <- rasterGrob(rat, interpolate=TRUE)
pdf('/Users/Avril/Desktop/map_with_site.pdf', width=5, height=5)
ggplot(data = states) +
  theme_classic() +
  theme(panel.background = element_rect(fill=alpha(blue, al)),
        axis.text=element_text(size=10, colour='black'),
        axis.title=element_blank(),
        panel.border=element_rect(fill=NA)) +
  ## state outlines
  geom_sf(fill=grey, col='grey80', lwd=0.25) + ## other greens: springgreen4  #B5D383
  ## US-Mexico border
  geom_sf(data = border, col='grey30', lwd=0.5) +
  ## add range shapefile
  geom_sf(data = range, col='cadetblue4', fill=alpha('cadetblue', 0.7), lwd=line.w) + ## other blues: cadetblue1 #98CAC2
  ## add site coords
  # geom_point(y=31.6166667, x=-109.25, pch=8) +
  ## add scalebar
  ggsn::scalebar(x.min=-115, x.max=-98, y.min=21, y.max=38,
                 dist=200, dist_unit='km', st.bottom=TRUE, st.color='black',
                 transform=TRUE, location='bottomleft', st.size=3, st.dist=.03) +
  ## add krat!
  annotation_custom(r, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax) +
  coord_sf(xlim = c(-117, -98), ylim = c(20, 38), expand = FALSE)
dev.off()
