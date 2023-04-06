setwd('/Users/Avril/Documents/krat_remote_sensing/range_map_materials')

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

# register_google(key = "AIzaSyATHChw2jBcnmrJifDassovYlq3bxcEe7M", write = TRUE)
# 
# ## read in range map shapefile (downloaded from IUCN on 2/24/21)
# range <- st_read('redlist_species_data_5d4b0dfc-6c14-467a-a3f6-5d68c8ae2371/data_0.shp')
# ## transform to EPSG 3857 (Pseudo-Mercator, what Google uses)
# range <- st_transform(range, 3857)
# 
# ## prep state + country data
# mex <- ne_countries(country='Mexico', returnclass = 'sf', scale=10)
# us <- ne_countries(country='United States of America', returnclass = 'sf', scale=10)
# border <- st_read('shp/International_Boundary_Final.shp')
# border <- st_transform(border, 3857)
# states <- ne_states(country=c('United States of America', 'Mexico'), returnclass = 'sf')
# mx.states <- states[which(states$admin == 'Mexico'),]
# us.states <- states[which(states$admin != 'Mexico'),]
# mx.states <- st_transform(mx.states, 3857)
# us.states <- st_transform(us.states, 3857)
# 
# ## study site = 31°36'27"N, 109°15'48"W
# y <- 31 + (36/60) + (27/3600)
# x <- -(109 + (15/60) + 51/3600)
# 
# # map <- get_map(c(left = -109.784532,
# #                  bottom = 31.30,
# #                  right = -108.642000,
# #                  top = 32.243634), maptype = 'satellite', source = 'google')
# 
# map <- get_map(location = c(lon = x, lat = y), maptype = 'satellite', source = 'google', zoom = 9)
# ggmap(map)
# 
# zoom.map <- get_map(location = c(lon = x, lat = y), maptype = 'satellite', source = 'google', zoom = 15)
# ggmap(zoom.map)


# save.image('fig_01_google_map_env.RData')
load('fig_01_google_map_env.RData')

sf_use_s2(FALSE)
library(beyonce)

ggmap(map)
ggmap(zoom.map)

## study site bounds
site.xmin <- -109.273153
site.xmax <- -109.254491
site.ymin <- 31.601774
site.ymax <- 31.613652

ggmap(map) +
  geom_polygon(aes(x = c(site.xmin, site.xmax, site.xmax, site.xmin), 
                   y = c(site.ymin, site.ymin, site.ymax, site.ymax)), colour = 'transparent', 
               fill = alpha('yellow', 0.5))


## from https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
map.b <- ggmap_bbox(map)

##### Site context map (map 2) #####
site.poly.dat <- as.data.frame(rbind(c(site.xmin, site.ymin),
                                     c(site.xmin, site.ymax),
                                     c(site.xmax, site.ymin),
                                     c(site.xmax, site.ymax)))
colnames(site.poly.dat) <- c('lon', 'lat')
site.pts <- st_as_sf(x = site.poly.dat, coords = c('lon','lat'), crs = 4326)
site.pts <- st_transform(site.pts, crs = 3857)
# pdf('/Users/Avril/Desktop/map_2.pdf', width = 10, height = 10)
ggmap(map.b) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  # geom_sf(data = range, aes(fill = alpha('blue', 0.1)), inherit.aes = FALSE) +
  # scale_fill_manual(values = c(alpha('blue', 0.1))) +
  ## state outlines
  geom_sf(data = us.states, col='white', fill = 'transparent', linewidth=0.5, 
          inherit.aes = FALSE) + 
  geom_sf(data = mx.states, col='white', fill = 'transparent', linewidth=0.5, 
          inherit.aes = FALSE) + 
  geom_sf(data = site.pts, colour = beyonce_palette(75)[8],
          fill = beyonce_palette(75)[7],
          shape = 21, size = 0.2,
          inherit.aes = F) +
  theme_bw()
# dev.off()

##### State context map (map 1) #####
al <- 1
line.w <- 0.5
grey <- "gray95" ## other greens: springgreen4  #B5D383  mediumseagreen #ADF2CB #76C599
blue <- 'darkgrey' ## other blues: cadetblue1 #98CAC2  lightblue2  #DFF7FF
site <- as.data.frame(cbind(c(site.xmin, site.xmax, site.xmax, site.xmin), c(site.ymin, site.ymin, site.ymax, site.ymax)))
colnames(site) <- c('lon','lat')

# pdf('/Users/Avril/Desktop/map_1.pdf', width = 5, height = 5)
ggplot(data = states) +
  theme_classic() +
  theme(panel.background = element_rect(fill=alpha(blue, al)),
        axis.text=element_text(size=10, colour='black'),
        axis.title=element_blank(),
        panel.border=element_rect(fill=NA)) +
  ## state outlines
  geom_sf(fill=grey, col='grey80', lwd=0.25) +
  ## US-Mexico border
  geom_sf(data = border, col='grey30', lwd=0.5) +
  ## add range shapefile
  geom_sf(data = range, col=NA, fill=alpha(beyonce_palette(75)[4], 0.3), lwd=line.w) +
  ## add scalebar
  ggsn::scalebar(x.min=-115, x.max=-103.5, y.min=31, y.max=37,
                 dist=100, dist_unit='km', st.bottom=TRUE, st.color='black',
                 transform=TRUE, location='topright', st.size=3, st.dist=.03) +
  # geom_polygon(aes(x = c(xmin, xmax, xmax, xmin),
  #                  y = c(ymin, ymin, ymax, ymax)), colour = 'transparent',
  #              fill = alpha('yellow', 0.5), linewidth = 1) +
  geom_polygon(data = site, aes(x = lon, y = lat), fill = 'transparent',
               col = beyonce_palette(75)[5], linewidth = 2) +
  coord_sf(xlim = c(-115, -102.5), ylim = c(30, 37.5), expand = FALSE)
# dev.off()

##### Study site really zoomed in (map 3) #####
mnd.locs <- read.csv('/Users/Avril/Documents/krat_remote_sensing/intermediate_data/mound_GPS_coords_n188.csv')
mnd.locs$ID <- 1:nrow(mnd.locs) ## add column for later matching up with TC extracted pixel values
mnd.cells <- mnd.locs[,c(4,5)] ## save ID and db.name for saving cell names later
mnd.locs <- mnd.locs[c('long','lat','database.name')] ## rearrange/rename to match man.locs below
colnames(mnd.locs)[c(1,3)] <- c('lon','terr')

xmin <- site.xmin - 1
xmax <- site.xmax + 1
ymin <- site.ymin - 1
ymax <- site.ymax + 1

map.b <- ggmap_bbox(zoom.map)
mnd.pts <- st_as_sf(x = mnd.locs, coords = c('lon','lat'), crs = 4326)
mnd.pts <- st_transform(mnd.pts, crs = 3857)

# pdf('/Users/Avril/Desktop/map_3.pdf', width = 10, height = 10)
ggmap(map.b) + 
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = mnd.pts, colour = beyonce_palette(75)[8],
          fill = beyonce_palette(75)[7],
          shape = 21, size = 2.5,
          inherit.aes = F) +
  # ggsn::scalebar(x.min=site.xmin, x.max=site.xmax, y.min=site.ymin, y.max=site.ymax,
  #                dist=50, dist_unit='km', st.bottom=TRUE, st.color='black',
  #                transform=TRUE, location='bottomleft', st.size=3, st.dist=.03) +
  theme_bw()
# dev.off()

