######
# old tampa bay test dataset

######
# load spatial data

shps <- list.files('M:/GIS/seagrass', '^.*902.*shp$', full.names = T)

secchi_shp <- readShapePoints(
  shps[1],
  proj4string = CRS("+proj=longlat +datum=WGS84")
  )

seg_shp <- readShapePoly(shps[2])

sgpoly_shp <- readShapePoly(shps[3])

sgpts_shp <- readShapePoints(shps[4])

######
# repeat depth of col analysis from Jim's paper

sg_prp <- sg_bins(sgpts_shp)

# simple plot of proportion of 'occupied' points at depth bings
to_plo <- sg_prp[40:54, ]
to_plo <- melt(to_plo, id.var = 'bins')

ggplot(to_plo, aes(x = bins, y = value, group = variable,
  colour = variable)) +
    geom_line() +
    geom_point() + 
    theme_bw()

# estimates for depth of col
max_est(sg_prp, 'all')
max_est(sg_prp, 'cont')

##
# repeat by estimating depth of col using points within a random location

# random points
pts <- grid_est(seg_shp, spacing = 0.02)

test_pt <- pts[sample(1:length(pts), 1), ]

buff_pts <- buff_ext(sgpts_shp, test_pt)

plot(seg_shp)
points(pts)
points(buff_pts, col = 'red', pch = 16, cex = 0.2)

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


plot(seg_shp)
# points(pts, cex = sizes, col = col.pts, pch = 16)
sizes <- scales::rescale(maxd[, 1], c(0.8, 2))
text(data.frame(pts)[, 1], data.frame(pts)[,2],
  labels = round(maxd[, 1], 2), cex = sizes, col = col.pts, pch = 16)

