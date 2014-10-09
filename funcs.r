######
# functions for seagrass depth of col estimates

######
# function for estimating depth of colonization
# also used for plots
# 'dat_in' is data from 'buff_ext'
# 'depth_var' is name of depth column in input data
# 'sg_var' is name of seagrass column in input data
# 'thresh' is numeric threshold value for estimating depth of col
doc_est <- function(dat_in, depth_var = 'depth', sg_var = 'seagrass',
  thresh = 0.1){
  
	# order by depth, assumes column is negative
  dat_in <- dat_in[order(dat_in[, depth_var], decreasing = T), ]
	dat_in$depth <- dat_in[, depth_var]
	
	# cumulative sum of pts with all seagrass and all points
	# assumes NA is empty
	sg_pts <- table(dat_in[!is.na(dat_in[, sg_var]), depth_var])
	sg_pts <- data.frame(Depth = names(sg_pts), sg_pts = as.numeric(sg_pts),
		sg_cum = cumsum(sg_pts), row.names = 1:length(sg_pts))
	dep_pts <- table(dat_in[, depth_var])
	dep_pts <- data.frame(Depth = names(dep_pts), dep_pts = as.numeric(dep_pts), 
		dep_cum = cumsum(dep_pts), row.names = 1:length(dep_pts))
	
	# combine all pts and seagrass pts, depth as numeric
	pts <- merge(dep_pts, sg_pts, by = 'Depth', all.x = T)
	pts$Depth <- as.numeric(as.character(pts$Depth))
	pts$sg_prp <- with(pts, sg_pts/dep_pts)
	
	##
	# estimate a logistic function for the data
	resps <- c('dep_cum', 'sg_cum')
	pred_ls <- vector('list', length(resps))
	names(pred_ls) <- resps
	for(resp in resps){
		
		##
		# estimates of starting parameters
		
		# logistic growth
		Asym <- max(pts[, resp], na.rm = T)
		xmid <- median(pts$Depth, na.rm = T)
		scal <- quantile(pts$Depth, 0.75, na.rm = T) - xmid
		form_in <- substitute(x ~ SSlogis(Depth, Asym,  xmid, scal), list(x = as.name(resp)))

#			# Gompertz
# 		Asym <- max(pts[, resp], na.rm = T)
# 		b2 <- median(pts$Depth, na.rm = T)
# 		b3 <- b3
# 		form_in <- substitute(x ~ SSgompertz(Depth, Asym, b2, b3), list(x = as.name(resp)))

		# model and predictions
		mod <- nls(form_in, data = pts, na.action = na.exclude)
		
		dep_rng <- range(pts[, 'Depth'], na.rm = T)
		new.x <- seq(dep_rng[1], dep_rng[2], length = 100)
	
		pred_ls[[resp]] <- as.numeric(predict(mod, 
			newdata = data.frame(Depth = new.x)))
		
	}
	
	preds <- data.frame(Depth = new.x, do.call('cbind', pred_ls))

	# add slope ests to pts, use differences
	preds$dep_slo <- with(preds, c(NA, diff(dep_cum)/diff(Depth)))
	preds$sg_slo <- with(preds, c(NA, diff(sg_cum)/diff(Depth)))
	
	# add threshold data based on proportion of dep_slo 
	threshs <- sapply(1:length(thresh), 
		FUN = function(x) thresh[x] * preds[, 'dep_slo']
		)
	threshs <- data.frame(threshs)
	names(threshs) <- paste('Threshold', thresh)
	
	preds <- data.frame(preds, threshs)
	
	# calculate depth of col
	doc <- sapply(thresh, 
		FUN = function(x){
			
			col <- preds[, grep(x, names(preds))]
			ind <- which(with(preds,  sg_slo <= col))[1]
			preds[ind, 'Depth']
			
			}
		)
	names(doc) <- thresh

	return(list(data = pts, preds = preds, ests = doc))
	  
}

#######
# function for creating random grid of points, bounded by polygon extent
# taken from ibi sampling manuscript functions
# 'clip_poly' is shapefile input object
# 'spacing' is spacing between points, as degrees
grid_est <- function(clip_poly, spacing = 0.03){
  
  if(!'SpatialPolygonsDataFrame' %in% class(clip_poly))
    stop('clip_poly must be of class SpatialPolygonsDataFrame')
  
  library(sp) 
  
  # extent of shapefile
  extent <- summary(clip_poly)$bbox
  
  # buffer of shapefile and random starting x/y locs
  add.on <- apply(extent, 1, diff) * 0.3
  rand <- runif(2, 0, spacing)
  
  # random points within rectangular extent
  pts<-{
    x.vals<-seq(extent[1, 1] - add.on['x'], extent[1, 2] + add.on['x'], by = spacing) + rand[1]
    y.vals<-seq(extent[2, 1] - add.on['y'], extent[2, 2] + add.on['y'], by = spacing) + rand[2]
    expand.grid(x.vals, y.vals)
  }
  
  # clip by clip_poly and return
  sel <- !is.na(SpatialPoints(pts) %over% clip_poly)[, 1]
  
  return(SpatialPoints(pts)[sel, ])
  
}

######
# function extracts bathymetric seagrass pts withing a distance from a pt
# 'pts' is spatial points to extract
# 'center' is pt from which buffer extends
# 'buff' is radius of buffer in dec degrees
buff_ext <- function(pts, center, buff = 0.03){
  
  # sanity checks
  if(!any(c('SpatialPointsDataFrame', 'SpatialPoints') %in% class(pts)))
    stop('pts must be of class SpatialPointsDataFrame or SpatialPoints')
  
  if(!any(c('SpatialPointsDataFrame', 'SpatialPoints') %in% class(center)))
    stop('center must be of class SpatialPointsDataFrame or SpatialPoints')
  
  library(rgeos)
  library(sp)
  
  # create buffer
  buffer <- gBuffer(center, width = buff)
  
  # index of pts in buffer
  sel <- !is.na(pts %over% buffer)
  
  if(sum(sel) == 0) stop('No points in buffer')
  
  return(pts[sel, ])
  
}
