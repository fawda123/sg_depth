library(maptools)

# secchi data
load(file = 'dat/secc_seg.RData')

# segment and seagrass points data
seg <- readShapeSpatial('seagrass_gis/seg_1502.shp')
sgpts_shp <- readShapeSpatial('seagrass_gis/sgpts_2009_1502.shp')

# clip secchi by seg
# clip secchi data by segments
sel <- !is.na(secc_seg %over% seg)[, 1]
secc <- secc_seg[sel, ]

# radius around each point to estimate doc
radius <- 0.02

# get sg_depth estimates for each location with secchi data
uni_secc <- SpatialPoints(unique(coordinates(secc)))

maxd <- list()
for(i in 1:length(uni_secc)){
  
  eval_pt <- uni_secc[i, ]
  ests <- try({
    buff_pts <- buff_ext(sgpts_shp, eval_pt, buff = radius)
	  est_pts <- data.frame(buff_pts)
    doc_single <- doc_est(est_pts)[['doc_max']]
    doc_single
  })
	if('try-error' %in% class(ests)) ests <- NA
  maxd[[i]] <- ests
  
}
maxd <- data.frame(uni_secc, zmax_all = do.call('c', maxd))

# need to merge ests with secchi...