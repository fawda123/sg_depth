######
# functions for seagrass depth of col estimates

######
# function for estimating depth of colonization
# also used for plots
# 'dat_in' is data from 'buff_ext'
# 'depth_var' is name of depth column in input data
# 'sg_var' is name of seagrass column in input data
doc_est <- function(dat_in, depth_var = 'Depth', sg_var = 'Seagrass', sg_cat = c('Continuous', 'Discontinuous')){
  
	# order by depth, assumes column is negative
  dat_in <- dat_in[order(dat_in[, depth_var], decreasing = T), ]
	dat_in$depth <- dat_in[, depth_var]
	
	# cumulative sum of pts with all seagrass and all points
	# assumes NA is empty
	sg_pts <- table(dat_in[dat_in[, sg_var] %in% sg_cat, depth_var])
  
  # stop function if no seagrass found
  if(length(sg_pts) == 0) stop('No seagrass present')
  
	sg_pts <- data.frame(Depth = names(sg_pts), sg_pts = as.numeric(sg_pts),
		row.names = 1:length(sg_pts))
	dep_pts <- table(dat_in[, depth_var])
	dep_pts <- data.frame(Depth = names(dep_pts), dep_pts = as.numeric(dep_pts), 
		row.names = 1:length(dep_pts))
	
	# combine all pts and seagrass pts, depth as numeric
	pts <- merge(dep_pts, sg_pts, by = 'Depth', all.x = T)
	pts$Depth <- as.numeric(as.character(pts$Depth))
	# output
  pts$sg_prp <- with(pts, sg_pts/dep_pts)
	
	##
	# estimate a logistic growth function for the data
	resps <- c('sg_prp')
	pred_ls <- vector('list', length(resps))
	names(pred_ls) <- resps
  asym_ls <- pred_ls
	for(resp in resps){
		
		##
		# estimates of starting parameters
		
		# logistic growth
		Asym <- max(pts[, resp], na.rm = T)
		xmid <- median(pts$Depth, na.rm = T)
		scal <- quantile(pts$Depth, 0.75, na.rm = T) - xmid
		form_in <- substitute(x ~ SSlogis(Depth, Asym,  xmid, scal), 
      list(x = as.name(resp)))

		# model
		mod <- try(nls(form_in, data = pts, na.action = na.exclude))
		
    # values for prediction
		dep_rng <- range(pts[, 'Depth'], na.rm = T)
		new.x <- seq(dep_rng[1], dep_rng[2], length = 500)
	
    # return NAs if model fail, else get model predictions
    if('try-error' %in% class(mod)) {
      pred_ls[[resp]] <- rep(NA_real_, length = length(new.x))
      asym_ls[[resp]] <- NA_real_
    } else {
		  pred_ls[[resp]] <- as.numeric(predict(mod, 
			  newdata = data.frame(Depth = new.x)))
      asym_ls[[resp]] <- summary(mod)$coefficients['Asym', 'Estimate']
    }
		
	}
	
  # output
	preds <- data.frame(Depth = new.x, do.call('cbind', pred_ls))
	asym <- unlist(asym_ls)
  
  ##
	# calculate depth of col using inflection point
  
  # out put vals
  est_fun <- NA
  sg_max <- NA
  doc_med <- NA
  doc_max <- NA
  
  # if no curve fit
  if(sum(is.na(preds$sg_prp)) == nrow(preds)){
    
    return(list(data = pts, preds = preds, est_fun = est_fun, 
      sg_max = sg_max, doc_max = doc_max, doc_med = doc_med))
     
  }
  
  # check if curve is monotonic descending
  if(!with(preds, all(sg_prp == cummin(sg_prp)))){
    
    return(list(data = pts, preds = preds, est_fun = est_fun, 
      sg_max = sg_max, doc_max = doc_max, doc_med = doc_med))
    
  }
  
  # search for inflection point if monotonic descending and curve is fit

  # first deriv
  inflect <- diff(preds$sg_prp)/diff(preds$Depth)
  ind_min <- which.min(inflect)
    
  # get curve estimate if the minimum slope is not the last value
  if(ind_min != (nrow(preds) - 1)){
    
    inflect_val <- preds[ind_min + 1, ]
    slope_val <- inflect[ind_min]
    int_val <- inflect_val$sg_prp - slope_val * inflect_val$Depth
    est_fun <- function(x) slope_val * x + int_val
    doc_max <- -1 * int_val / slope_val
    
    # get doc_med, halfway between sg_max and doc_max
    # sg_max is based on asymptote intercept with linear reg
    # sg_max defaults to zero if value is extrapolated
    sg_max <- max(c(0, (asym - int_val)/slope_val))
    doc_med  <- sg_max + ((doc_max - sg_max)/2)

  }
    
  # all output
  return(list(data = pts, preds = preds, est_fun = est_fun, 
    sg_max = sg_max, doc_max = doc_max, doc_med = doc_med))
    
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

######
# krige results from spatial grid of seagrass depth of col ests
#
# @param maxd data frame of maximum depth estimates with Var1 and Var2 columns of coordinates
# @param seg_shp spatial polygon data frame of segment to clip kriging estimate
# @param length number of points on x or y axis for predicting based on kriging results, passed to seq
#
# @import automap gstat
sg_krige <- function(maxd, seg_shp, length = 250){
  
  # maxd as spatial data frame
  maxd <- na.omit(maxd)
  coordinates(maxd) = ~ Var1 + Var2
  
  # new coordinates to predict
  newdat <- apply(bbox(seg_shp), 1, function(x) seq(x[1], x[2], length = length))
  newdat <- expand.grid(newdat[, 1], newdat[, 2])
  gridded(newdat) = ~ Var1 + Var2
  
  # kriging results
  res <- suppressWarnings(autoKrige(zmax_all ~ 1, maxd, newdat))
  res <- res['krige_output'][[1]]
  
  # clip results by segment
  sel <- !is.na(res %over% seg_shp)[, 1]
  res <- res[sel, ]
  res <- data.frame(coordinates(res), maxd = res$var1.pred)
  
  # return results
  return(res)
  
}