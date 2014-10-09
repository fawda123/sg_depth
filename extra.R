source('.Rprofile')

# segment polygon for old tampa bay
seg_shp <- readShapeSpatial('seagrass_gis/seg_902.shp')

# seagrass bathmetry intersected points for old tampa bay
sgpts_shp <- readShapeSpatial('seagrass_gis/sgpts_902_2010.shp')

# set ggplot theme
theme_set(theme_bw())

# for debugging
grid_spc <- 0.02
grid_seed <- 1234
test_pt <- 16
radius <- 0.04
thresh <- 0.1    
show_all <- F

# random points  
set.seed(grid_seed)
pts <- grid_est(seg_shp, spacing = grid_spc) 

# point from random points for buffer
test_pt <- pts[test_pt, ]

# get bathym points around test_pt
buff_pts <- buff_ext(sgpts_shp, test_pt, buff = radius)
  
##
# example w/ actual data

# get data used to estimate depth of col

est_pts <- data.frame(buff_pts)
est_pts$depth <- -1 * est_pts$depth
est_pts$depth <- pmax(0, est_pts$depth)

# data
dat <- doc_est(est_pts, thresh = thresh, 
	depth_var = 'depth', sg_var = 'Descript'
	)

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
  scale_colour_manual('Point category', values = cols) +
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

col_pts <- rep(cols[1], nrow(to_plo))
col_pts[to_plo$variable == 'All'] <- cols[2]

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
	ncol = 1, heights = c(1.25, 1),
	main = act_ests)