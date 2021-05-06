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

## read in range map shapefile (downloaded from IUCN on 2/24/21)
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
##### Plot range map with site not marked #####
# pdf('/Users/Avril/Desktop/map.pdf', width=6, height=6)
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
# dev.off()

## range map with study site point or krat
## set bounds for krat image
size <- 1.5
xmin <- -109.25 - size
xmax <- -109.25 + size
ymin <- 31.6166667 - size
ymax <- 31.6166667 + size

al <- 1
line.w <- 0

rat <- readTIFF('IMG_1944.TIF', native=FALSE)
r <- rasterGrob(rat, interpolate=TRUE)
##### Plot range map with krat marking study site #####
# pdf('/Users/Avril/Desktop/map_with_site.pdf', width=5, height=5)
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
  geom_sf(data = range, col='cadetblue4', fill=alpha('cadetblue', 0.8), lwd=line.w) + ## other blues: cadetblue1 #98CAC2
  ## add site coords
  # geom_point(y=31.6166667, x=-109.25, pch=8) +
  ## add scalebar
  ggsn::scalebar(x.min=-115, x.max=-98, y.min=21, y.max=38,
                 dist=200, dist_unit='km', st.bottom=TRUE, st.color='black',
                 transform=TRUE, location='bottomleft', st.size=3, st.dist=.03) +
  ## add krat!
  annotation_custom(r, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax) +
  coord_sf(xlim = c(-117, -98), ylim = c(20, 38), expand = FALSE)
# dev.off()

al <- 0
# pdf('/Users/Avril/Desktop/just_krat.pdf', width=5, height=5)
ggplot(data = states) +
  theme_classic() +
  theme(panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  ## add krat!
  annotation_custom(r, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax) +
  coord_sf(xlim = c(-117, -98), ylim = c(20, 38), expand = FALSE)
# dev.off()

##### Plot extent of the two Landsat scenes covering the site, hiliting the site #####
## set coords for Path 035, Row 038
ul.lat.35 <- 32.70513
ul.lon.35 <- -111.05975
ur.lat.35 <- 32.68173
ur.lon.35 <- -108.57683
ll.lat.35 <- 30.79965
ll.lon.35 <- -111.05854
lr.lat.35 <- 30.77792
lr.lon.35 <- -108.62591
path35 <- data.frame(x=c(ul.lon.35, ur.lon.35, lr.lon.35, ll.lon.35), y=c(ul.lat.35, ur.lat.35, lr.lat.35, ll.lat.35))

## set coords for Path 034, Row 038
ul.lat.34 = 32.71009
ul.lon.34 = -109.53293
ur.lat.34 = 32.65702
ur.lon.34 = -107.06850
ll.lat.34 = 30.82688
ll.lon.34 = -109.56236
lr.lat.34 = 30.77755
lr.lon.34 = -107.14716
path34 <- data.frame(x=c(ul.lon.34, ur.lon.34, lr.lon.34, ll.lon.34), y=c(ul.lat.34, ur.lat.34, lr.lat.34, ll.lat.34))

## set coords for study site
ul.lat.st = 31.61744331171146
ul.lon.st = -109.27282550000456
ur.lat.st = 31.61744331171146
ur.lon.st = -109.2583401675505
ll.lat.st = 31.60365830082449
ll.lon.st = -109.27282550000456
lr.lat.st = 31.60365830082449
lr.lon.st = -109.2583401675505
site <- data.frame(x=c(ul.lon.st, ur.lon.st, lr.lon.st, ll.lon.st), y=c(ul.lat.st, ur.lat.st, lr.lat.st, ll.lat.st))

pdf('/Users/Avril/Desktop/extents_with_site_zoomout.pdf', width=5, height=5)
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
  ## add Path 035 polygon
  geom_polygon(data=path35, fill=alpha('blue', 0.3), x=path35$x, y=path35$y) +
  ## add Path 034 polygon
  geom_polygon(data=path34, fill=alpha('red', 0.3), x=path34$x, y=path34$y) +
  ## add study site polygon
  # geom_polygon(data=site, fill=alpha('black', 1), x=site$x, y=site$y) +
  ## add site coords
  geom_point(y=31.6166667, x=-109.25, pch=8) +
  ## add scalebar
  ggsn::scalebar(x.min=-115, x.max=-98, y.min=21, y.max=38,
                 dist=200, dist_unit='km', st.bottom=TRUE, st.color='black',
                 transform=TRUE, location='bottomleft', st.size=3, st.dist=.03) +
  coord_sf(xlim = c(-112, -106), ylim = c(30.5, 33), expand = FALSE)
dev.off()
