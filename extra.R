######
# old tampa bay test dataset

######
# load spatial data

shps <- list.files('M:/GIS/seagrass', '^.*902.*shp$', full.names = T)

# all secchi pts for old tampa bay
secchi_shp <- readShapePoints(
  shps[1],
  proj4string = CRS("+proj=longlat +datum=WGS84")
  )

# segment polygon for old tampa bay
seg_shp <- readShapeSpatial(shps[2])

# seagrass polygon coverage for old tampa bay
sgpoly_shp <- readShapePoly(shps[3])

# seagrass bathmetry intersected points for old tampa bay
sgpts_shp <- readShapePoints(shps[4])

######
# repeat depth of col analysis from Jim's paper

sg_prp <- sg_bins(sgpts_shp)

# simple plot of proportion of 'occupied' points at depth bings
to_plo <- sg_prp[40:54, ]
to_plo <- melt(to_plo, id.var = 'bins')
to_plo$growth_cat <- factor(to_plo$variable, levels = c('all', 'cont'), 
  labels = c('All', 'Continous'))
  
all_mod <- max_est(sg_prp, 'all', mod = T)
cont_mod <- max_est(sg_prp, 'cont', mod = T)

all_est <- round(max_est(sg_prp, 'all'), 2)
cont_est <- round(max_est(sg_prp, 'cont'), 2)

cols  <- c('lightgreen', 'lightblue')
linesz <- 1

ggplot(to_plo, aes(x = bins, y = value, group = variable,
  colour = growth_cat)) +
  geom_line(size = linesz) +
  geom_point(size = 4) +
  geom_abline(
    intercept = all_mod[1], 
    slope = all_mod[2], 
    linetype = 'dashed', 
    colour = cols[1], 
    size = linesz
    ) +
  geom_abline(
    intercept = cont_mod[1], 
    slope = cont_mod[2], 
    linetype = 'dashed',
    colour = cols[2],
    size = linesz
    ) +
  ylab('Prop. of  points') +
  ylab('Prop. of  points') +
  xlab('Depth bin (m)') +
  scale_colour_manual('Growth\ncategory', values = cols)

# estimates for depth of col
max_est(sg_prp, 'all')
max_est(sg_prp, 'cont')

##
# repeat by estimating depth of col using points within a random location

# random points
pts <- grid_est(seg_shp, spacing = 0.02)

# point from random points for buffer
test_pt <- pts[sample(1:length(pts), 1), ]

# get bathym points around test_pt
buff_pts <- buff_ext(sgpts_shp, test_pt)

p1 <- ggplot(seg_shp, aes(long, lat)) + 
  geom_polygon(fill = 'white') +
  geom_path(color = 'black') +
  theme_classic() +
  coord_equal() +
  geom_point(
    data = data.frame(pts), 
    aes(Var1, Var2), 
    size = 3,
    pch = 1
    ) +
  geom_point(
    data = data.frame(buff_pts),
    aes(lon, lat), 
    colour = 'red', 
    size = 1, 
    alpha = 0.7
    )

# estimate depth of col for test_pts
sg_prp <- sg_bins(buff_pts)

# simple plot of proportion of 'occupied' points at depth bins
to_plo <- sg_prp[(nrow(sg_prp) - 8):nrow(sg_prp), ]
to_plo <- melt(to_plo, id.var = 'bins')
to_plo$growth_cat <- factor(to_plo$variable, levels = c('all', 'cont'), 
  labels = c('All', 'Continous'))
  
all_mod <- max_est(sg_prp, 'all', mod = T)
cont_mod <- max_est(sg_prp, 'cont', mod = T)

all_est <- round(max_est(sg_prp, 'all'), 2)
cont_est <- round(max_est(sg_prp, 'cont'), 2)

##
# plot pt prop ests

cols  <- c('lightgreen', 'lightblue')
linesz <- 1

p2 <- ggplot(to_plo, aes(x = bins, y = value, group = variable,
  colour = growth_cat)) +
  geom_line(size = linesz) +
  geom_point(size = 4) +
  geom_abline(
    intercept = all_mod[1], 
    slope = all_mod[2], 
    linetype = 'dashed', 
    colour = cols[1], 
    size = linesz
    ) +
  geom_abline(
    intercept = cont_mod[1], 
    slope = cont_mod[2], 
    linetype = 'dashed',
    colour = cols[2],
    size = linesz
    ) +
  ylab('Prop. of  points') +
  ylab('Prop. of  points') +
  xlab('Depth bin (m)') +
  scale_colour_manual('Growth\ncategory', values = cols) +
  theme(legend.position = c(0.8, 0.8))

ests <- paste0('All: Zmax ', all_est[1], ' Z50 ', all_est[2], '\n', 
  'Continuous: Zmax ', cont_est[1], ' Z50 ', cont_est[2])

grid.arrange(p1, p2, 
  ncol = 2, 
  main = ests
  ) 

max_est(sg_bins(buff_pts), 'all')

# get depth of col ests for each point
maxd <- NULL
for(i in 1:length(pts)){
  
  cat(i, '\t')
  
  eval_pt <- pts[i, ]
  buff_pts <- buff_ext(sgpts_shp, eval_pt, buff = 0.05)
  ests <- max_est(sg_bins(buff_pts), 'all')
  
  maxd <- rbind(maxd, ests)
  
  }

sizes <- scales::rescale(maxd[, 1], c(0.5, 3))
col.fun <- colorRampPalette(brewer.pal(9, 'RdBu'))
col.pts <- col.fun(length(sizes))[rank(sizes)]

sp::plot(seg_shp)
# points(pts, cex = sizes, col = col.pts, pch = 16)
sizes <- scales::rescale(maxd[, 1], c(0.8, 2))
text(data.frame(pts)[, 1], data.frame(pts)[,2],
  labels = round(maxd[, 1], 2), cex = sizes, col = col.pts, pch = 16)

