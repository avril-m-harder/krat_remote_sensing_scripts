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

##### Read in mound waypoints for later extraction #####
mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
coordinates(mnd.locs) <- c('long','lat') ## converts to SpatialPointsDataFrame object for plotting
proj4string(mnd.locs) <- CRS("+proj=longlat +datum=WGS84") 
mnd.locs <- sp::spTransform(mnd.locs, CRS("+proj=utm +zone=12 +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))

## read in range map shapefile
range <- st_read('redlist_species_data_5d4b0dfc-6c14-467a-a3f6-5d68c8ae2371/data_0.shp')
plot(range)

## prep state + country data
mex <- ne_countries(country='Mexico', returnclass = 'sf', scale=10)
us <- ne_countries(country='United States of America', returnclass = 'sf', scale=10)
us.mex <- st_intersection(us, mex)
smooth.us.mex <- smoothr::smooth(us.mex, method='densify')
us.mex <- st_intersection(us, us.mex)

states <- ne_states(country=c('United States of America', 'Mexico'), returnclass = 'sf')
state.list <- c('Chihuahua','Coahuila','Sonora','Texas','New Mexico','Arizona','Nevada','California','Oklahoma','Colorado','Utah','Kansas')
states <- states[which(states$name %in% state.list | states$admin == 'Mexico'),]

# us.can <- readOGR('/Users/Avril/Desktop/all_ch_3_dirs/mapping/Canada_and_US_Border-shp/Canada_and_US_Border.shp', stringsAsFactors = FALSE)
# us.can <- st_as_sf(us.can)

# points <- data.frame(name=c('LC','SE','LH'),
#                      x=c(44.689127, 43.889912, ),
#                      y=c(-73.348512, -70.464035, ))
al <- 1
line.w <- 0.5
grey <- "gray95" ## other greens: springgreen4  #B5D383  mediumseagreen #ADF2CB #76C599
blue <- 'lightblue2' ## other blues: cadetblue1 #98CAC2  lightblue2  #DFF7FF
# pdf('/Users/Avril/Desktop/map.pdf', width=6, height=6)
ggplot(data = states) +
  theme_classic() +
  theme(panel.background = element_rect(fill=alpha(blue, al)),
        axis.text=element_text(size=10, colour='black'),
        axis.title=element_blank(),
        panel.border=element_rect(fill=NA)) +
  ## state outlines
  geom_sf(fill=grey, col='grey', lwd=0.25) + ## other greens: springgreen4  #B5D383
  geom_sf(data = us.mex, col='grey30', lwd=0.75) +
  # ## add range shapefile
  # geom_path(data = range, aes(x=long, y=lat, group=group), fill='purple', col='blue', lwd=line.w) + ## only works to draw outline, not fill
  ## add range shapefile
  geom_sf(data = range, col='firebrick4', fill=alpha('firebrick4', 0.7), lwd=line.w) + ## other blues: cadetblue1 #98CAC2
  ## add scalebar
  ggsn::scalebar(x.min=-115, x.max=-98, y.min=21, y.max=38,
                 dist=200, dist_unit='km', st.bottom=TRUE, st.color='black',
                 transform=TRUE, location='bottomleft', st.size=3, st.dist=.03) +
  coord_sf(xlim = c(-117, -98), ylim = c(20, 38), expand = FALSE)
# dev.off()
