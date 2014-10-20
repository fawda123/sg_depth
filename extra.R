# packages to use
library(maptools)
library(reshape2) 
library(plyr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(gridExtra)
library(sp)

# functions to use
source('funcs.R')

##
# load shapefile data
to_load <- list.files('seagrass_gis', '\\.shp$')
shps <- vector('list', length = length(to_load))
names(shps) <- to_load
for(i in to_load) 
  shps[[i]] <- readShapeSpatial(paste0('seagrass_gis/', i))

# set ggplot theme
theme_set(theme_bw())

# plotting code

# for debugging
segment <- '820'
grid_spc <- 0.02
grid_seed <- 1234
test_point <- 1
radius <- 0.04
thresh <- 0.1   	

# get data from loaded shapefiles and input segment
seg_shp <- shps[[paste0('seg_', segment, '.shp')]]
sgpts_shp <- shps[[grep(paste0('^sgpts.*', segment, '.shp$'), names(shps))]]

# random points  
set.seed(grid_seed)
pts <- grid_est(seg_shp, spacing = grid_spc) 

# point from random points for buffer
test_pt <- pts[test_point, ]

# get bathym points around test_pt
buff_pts <- buff_ext(sgpts_shp, test_pt, buff = radius)

p1 <- ggplot(seg_shp, aes(long, lat)) + 
  geom_polygon(fill = 'white') +
  geom_path(color = 'black') +
  theme_classic() +
  coord_equal() +
	xlab('Longitude') +
	ylab('Latitude') +
  geom_point(
    data = data.frame(pts), 
    aes(Var1, Var2), 
    size = 3,
    pch = 1
  ) +
  geom_point(
    data = data.frame(buff_pts),
    aes(coords.x1, coords.x2), 
    colour = 'red', 
    size = 0.3, 
    alpha = 0.7
  ) +
	geom_point(
		data = data.frame(test_pt), 
		aes(Var1, Var2), 
		size = 3, 
		pch = 1
	)

##
 # get data used to estimate depth of col
est_pts <- data.frame(buff_pts)

# data
dat <- doc_est(est_pts, thresh = thresh)

##
# plot proportion occupied curve

to_plo <- dat$data
to_plo2 <- dat$preds

p2 <- ggplot(to_plo, aes(x = Depth, y = sg_prp)) +
	geom_point(pch = 1, size = 2) +
  geom_line(data = to_plo2, 
    aes(x = Depth, y = sg_prp)
    ) +
 	ylab('Proportion of points with seagrass') +
  xlab('Depth (m)')

##
# combine all plots

grid.arrange(p1,
	arrangeGrob(p2, p3, ncol = 2), 
	ncol = 1, heights = c(1.5, 1.5),
	main = act_ests)
    
