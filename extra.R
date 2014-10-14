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
segment <- '902'
grid_spc <- 0.005
grid_seed <- 1234
test_point <- 299
radius <- 0.025
thresh <- 0.2   	

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

# actual ests
act_ests <- dat$ests

# format estimate for plot title
if(any(is.na(act_ests))){ act_ests <- 'Depth of col: Not estimable'
} else { 
	act_ests <- paste('Depth of col:', round(act_ests, 1), 'm')
	}

##
# simple plot of points by depth, all pts and those with seagrass
to_plo <- dat$data
to_plo <- melt(to_plo, id.var = 'Depth', 
	measure.var = c('dep_cum', 'sg_cum'))
to_plo$variable <- factor(to_plo$variable, levels = c('dep_cum', 'sg_cum'), 
                            labels = c('All', 'Seagrass'))

to_plo2 <- dat$preds
to_plo2 <- melt(to_plo2, id.var = 'Depth', 
	measure.var = grep('dep_cum|sg_cum', names(to_plo2), value = T)
	)
to_plo2$variable <- factor(to_plo2$variable, 
	levels = c('dep_cum', 'sg_cum'),
	labels = c('All', 'Seagrass')
)

cols  <- c('lightgreen', 'lightblue')
linesz <- 1

p2 <- ggplot(to_plo2, aes(x = Depth, y = value, group = variable,
                         colour = variable)) +
  geom_line(size = linesz) +
	geom_point(data = to_plo, aes(x = Depth, y = value, group = variable, 
		colour = variable)) +
 	ylab('Cumulative points') +
  xlab('Depth (m)') +
  scale_colour_manual('Point category', values = rev(cols)) +
  theme(legend.position = c(0, 1), legend.justification = c(0,1))

##
# plot slope of cumulative point curves

# treshold label for legend
thresh_lab <- paste0(round(100 * thresh), '% of all')

to_plo <- dat$preds
to_plo <- melt(to_plo, id.var = 'Depth', 
	measure.var = grep('dep_slo|sg_slo|Threshold', names(to_plo), value = T)
	)
to_plo$variable <- factor(to_plo$variable)
to_plo$variable <- factor(to_plo$variable, 
	levels = c('dep_slo', 'sg_slo', grep('Thresh', levels(to_plo$variable), value = T)),
	labels = c('All', 'Seagrass', thresh_lab)
)

p3 <- ggplot(to_plo, aes(x = Depth, y = value,
		colour = variable, linetype = variable)) +
	geom_line(size = linesz) +
 	ylab('CDF Slope') +
  xlab('Depth (m)') +
	scale_linetype_manual('Slope category', 
		values = c('solid', 'solid', 'dashed')
		) + 
	scale_colour_manual('Slope category', 
  	values = c(cols[2], cols[1], cols[2])
  	) +
  theme(legend.position = c(1, 1), legend.justification = c(1, 1))

##
# combine all plots

grid.arrange(p1,
	arrangeGrob(p2, p3, ncol = 2), 
	ncol = 1, heights = c(1.5, 1.5),
	main = act_ests)
    
