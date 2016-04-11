######
# functions for seagrass depth of col estimates

######
#' Create a doc object
#' 
#' Wrapper for creating a doc object
#' 
#' @param  ls_in list input created internally within \code{\link{doc_est}}
#' 
#' @export doc
#' 
#' @return Returns a doc object to be used with S3 methods
#' 
#' @details 
#' This function is a simple wrapper to \code{\link[base]{structure}} that is used internally within other functions to create a doc object.  The function does not have to be used explicitly.    
#' 
doc <- function(ls_in){
    
  # sanity check
  if(any(!names(ls_in) %in% c('data', 'preds', 'logis_mod', 'est_fun', 'z_cmin', 'z_cmed', 'z_cmax', 'lower_est', 'upper_est'))) stop('Incorrect input for doc object')

  # create class, with multiple attributes
  structure(
    .Data = ls_in[['data']], 
    class = c('doc', 'data.frame'), 
    preds = ls_in$preds,
    logis_mod = ls_in$logis_mod, 
    est_fun = ls_in$est_fun,
    z_cmin = ls_in$z_cmin,
    z_cmed = ls_in$z_cmed, 
    z_cmax = ls_in$z_cmax,
    lower_est = NA,
    upper_est = NA
    )
  
}


######
# formatting of values in S expressions
form_fun <- function(x, rnd_val = 2, dig_val = 2, nsm_val = 2) {
  to_form <- as.numeric(as.character(x))
  format(round(to_form, rnd_val), digits = dig_val, nsmall = nsm_val)
  }

# similar as above but for numeric values in tables
formtab_fun <- function(x, rnd_val = 2, dig_val = 2, nsm_val = 2) {
  format(round(x, rnd_val), digits = dig_val, nsmall = nsm_val)
  }

######
# get logistic model from input
# dat_in is data frame of empirical estimates of seagrass occupancy by depth
# resp is name of response variable, usually sg_prp
# new_val is number of obs for predicting from fitted mod
logis_est <- function(dat_in, resp = 'sg_prp', new_vals = 500){
  
  pts <- dat_in
  
  # logistic growth
	Asym <- max(pts[, resp], na.rm = T)
	xmid <- median(pts$Depth, na.rm = T)
	scal <- quantile(pts$Depth, 0.75, na.rm = T) - xmid
	form_in <- substitute(x ~ SSlogis(Depth, Asym,  xmid, scal), 
    list(x = as.name(resp)))

	# model
#   upper_bnds <- c(Asym = 2, xmid = Inf, scal = Inf)
#   mod <- try({nls(form_in, data = pts, na.action = na.exclude,
#     upper = upper_bnds, algorithm = 'port')}, silent = T)
	mod <- try({nls(form_in, data = pts, na.action = na.exclude)}, silent = T)
	
  # values for prediction
	dep_rng <- range(pts[, 'Depth'], na.rm = T)
	new.x <- seq(dep_rng[1], dep_rng[2], length = new_vals)

  # return NAs if model fail, else get model predictions
  if('try-error' %in% class(mod)) {
    pred <- rep(NA_real_, length = length(new.x))
    asym <- NA_real_
    logis_mod <- NA
    message('Cannot fit curve to seagrass data\n')
  } else {
	  pred <- as.numeric(predict(mod, 
		  newdata = data.frame(Depth = new.x)))
    pred <- data.frame(new.x, pred)
    names(pred) <- c('Depth', resp)
    asym <- summary(mod)$coefficients['Asym', 'Estimate']
    logis_mod <- mod
  }
  
  # return output
  out <- list(pred = pred, asym = asym, logis_mod = logis_mod)
  return(out)
  
}

######
# get doc ests from fitted logistic regression curve
# dat_in is two column data frame first is depth, second is predicted proportion of points occupied by seagrass
# asym is asymptote estimate from logistic mod
get_ests <- function(dat_in, asym){
  
  if(class(dat_in) != 'data.frame') stop('Input must be data frame')
  
  # exp and resp columns from dat_in
  x_val <- dat_in[, 1]
  y_val <- dat_in[, 2]
  
  # first deriv
  inflect <- diff(y_val)/diff(x_val)
  ind_min <- which.min(inflect)
    
  est_fun <- NA
  z_cmin <- NA
  z_cmed <- NA
  z_cmax <- NA
  
  # get curve estimate if the minimum slope is not the last value
  if(ind_min != (nrow(dat_in) - 1)){
    
    inflect_val <- dat_in[ind_min + 1, ]
    slope_val <- inflect[ind_min]
    int_val <- inflect_val[, 2] - slope_val * inflect_val[, 1]
    est_fun <- function(x) slope_val * x + int_val
    z_cmax <- -1 * int_val / slope_val
    
    # get z_cmed, halfway between z_cmin and z_cmax
    # z_cmin is based on asymptote intercept with linear reg
    # z_cmin defaults to zero if value is extrapolated
    z_cmin <- max(c(0, (asym - int_val)/slope_val))
    z_cmed  <- z_cmin + ((z_cmax - z_cmin)/2)

  }
  
  # output
  out <- list(preds = dat_in, est_fun = est_fun, z_cmin = z_cmin, z_cmed = z_cmed, z_cmax = z_cmax)
  return(out)
  
}

######
# function for estimating depth of colonization
# also used for plots
# 'dat_in' is data from 'buff_ext'
# 'depth_var' is name of depth column in input data
# 'sg_var' is name of seagrass column in input data
doc_est <- function(dat_in, depth_var = 'Depth', sg_var = 'Seagrass', sg_cat = c('Continuous', 'Discontinuous')){
  
  # sanity check
  if(!'data.frame' %in% class(dat_in)) dat_in <- data.frame(dat_in)
  
	# order by depth, assumes column is negative
  dat_in <- dat_in[order(dat_in[, depth_var], decreasing = T), ]
	dat_in$depth <- dat_in[, depth_var]
  
  # bin depth values
  dat_in[, depth_var] <- round(dat_in[, depth_var], 1)
	
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
  mod_est <- logis_est(pts, 'sg_prp')

  # output
	preds <- mod_est$pred
	asym <- mod_est$asym
  logis_mod <- mod_est$logis_mod
  
  ##
	# calculate depth of col using get_est

  # if no curve fit
  if(any(is.na(logis_mod))){
    
    # create doc output
    ls_in <- list(data = pts, preds = preds, logis_mod = logis_mod, est_fun = NA, 
      z_cmin = NA, z_cmed = NA, z_cmax = NA, lower_est = NA, upper_est = NA)
    
    out <- doc(ls_in)
    return(out)
     
  }
  
  # check if curve is monotonic descending
  if(!with(preds, all(sg_prp == cummin(sg_prp)))){
    
    ls_in <- list(data = pts, preds = preds, logis_mod = logis_mod, est_fun = NA, 
      z_cmin = NA, z_cmed = NA, z_cmax = NA, lower_est = NA, upper_est = NA)
    
    out <- doc(ls_in)
    return(out)
    
  }
  
  # get doc estimates using get_ests functions
  ests <- get_ests(preds, asym)
  preds <- ests[['preds']]
  est_fun <- ests[['est_fun']]
  z_cmin <- ests[['z_cmin']]
  z_cmed <- ests[['z_cmed']]
  z_cmax <- ests[['z_cmax']]
    
  # all output
  ls_in <- list(data = pts, preds = preds, logis_mod = logis_mod, est_fun = est_fun, 
    z_cmin = z_cmin, z_cmed = z_cmed, z_cmax = z_cmax, lower_est = NA, 
    upper_est = NA)
  
  out <- doc(ls_in)
  return(out)
    
}

######
# create a plot of doc estimates from doc_est, same inputs as doc_est
# 'doc_in' doc object input from doc_est
# 'sens' logical indicating of plot includes sensitivity estimates
# 'baseonly' logical indicating of only observed data are plotted
# 'logisonly' logical indicating of only observed data and logistic curve are plotted
plot.doc <- function(doc_in, sens = F, baseonly = F, logisonly = F){
  
  to_plo <- data.frame(doc_in)
  ests <- attributes(doc_in)[c('z_cmin', 'z_cmed', 'z_cmax')]
  est_fun <- attr(doc_in, 'est_fun')
  
  # y, x axis limits
  y_lims <- 1.2 * max(na.omit(to_plo$sg_prp))
  y_lims <- c(-0.05 * y_lims, y_lims)
  x_lims <- max(1.2 * max(na.omit(to_plo)$Depth), 1.2 * ests$z_cmin, na.rm = T)
  x_lims <- c(-0.025 * x_lims, x_lims)

  # base plot if no estimate is available
  p <- ggplot(to_plo, aes(x = Depth, y = sg_prp)) +
    geom_point(pch = 1, size = 4) +
    theme(text = element_text(size=20)) +
    ylab('Proportion of points with seagrass') +
    xlab('Depth (m)') +
    coord_cartesian(xlim = x_lims, ylim = y_lims) 
  
  if(baseonly) return(p)
  
  # get y value from est_fun for z_cmin and z_cmed
  yends <- try({
    with(attributes(doc_in), est_fun(c(z_cmin, z_cmed)))
    }, silent = T)
  
  # add to baseplot if estimate is available
  if(!'try-error' %in% class(yends)){
  
    ##
  	# simple plot of points by depth, all pts and those with seagrass
    to_plo2 <- attr(doc_in, 'pred')
    est_fun <- attr(doc_in, 'est_fun')
    to_plo4 <- data.frame(
      Depth = unlist(ests), 
      yvals = c(yends, 0)
    )
    get_vals <- c('z_cmin', 'z_cmed', 'z_cmax')
    doc_in <- sens(doc_in, trace = F, remzero = F)
    lowers <- round(unlist(attr(doc_in, 'lower_est')[get_vals]), 2)
    uppers <- round(unlist(attr(doc_in, 'upper_est')[get_vals]), 2)
    
    # return fig with pts and logistic curve only
    if(logisonly){
      
      p <- p + geom_line(data = to_plo2, 
        aes(x = Depth, y = sg_prp)
        ) 
      return(p)
      
    }
    
    # some formatting crap
    pt_cols <- brewer.pal(nrow(to_plo4), 'Blues')

    # legend labels 
    leg_dep <- format(round(to_plo4$Depth, 2), nsmall = 2)
    leg_low <- format(round(lowers, 2), nsmall = 2)
    leg_ups <- format(round(uppers, 2), nsmall = 2)
    leg_labmin <- bquote(italic('Z' ['c, min']) ~ .(leg_dep[1]) ~ '(' ~ .(leg_low[1]) ~ ', ' ~ .(leg_ups[1]) ~ ')')
    leg_labmed <- bquote(italic('Z' ['c, med']) ~ .(leg_dep[2]) ~ '(' ~ .(leg_low[2]) ~ ', ' ~ .(leg_ups[2]) ~ ')')
    leg_labmax <- bquote(italic('Z' ['c, max']) ~ .(leg_dep[3]) ~ '(' ~ .(leg_low[3]) ~ ', ' ~ .(leg_ups[3]) ~ ')')
    leg_lab <- c(leg_labmin, leg_labmed, leg_labmax)
    
    # base plot including uncertainty
    p <- p +
      geom_line(data = to_plo2, 
        aes(x = Depth, y = sg_prp), colour = 'grey', size = 1.5
        ) +
      coord_cartesian(xlim = x_lims, ylim = y_lims) + 
      stat_function(fun = est_fun, colour = 'lightgreen', size = 1.5, 
        alpha = 0.8) +
      geom_segment(x = ests$z_cmin, y = 0, xend = ests$z_cmin, 
        yend = yends[1], linetype = 'dashed', colour = 'lightgreen',
        size = 1.5, alpha = 0.6) +
      geom_segment(x = ests$z_cmed, y = 0, xend = ests$z_cmed, 
        yend = yends[2], linetype = 'dashed', colour = 'lightgreen',
        size = 1.5, alpha = 0.6) +
      geom_errorbarh(data = to_plo4,
        aes(x = Depth, y = yvals), 
        xmin = lowers, xmax = uppers, height = 0.05) +
      geom_point(data = to_plo4, 
        aes(x = Depth, y = yvals, fill = factor(Depth)), 
        size = 6, pch = 21) +
      scale_fill_brewer('Depth estimate m (+/- 95%)', 
        labels = leg_lab,
        palette = 'Blues'
        ) +
      theme(legend.position = c(1, 1),
        legend.justification = c(1, 1)) 
  
    
  }
  
  p
  
}
 
#######
# function for creating random grid of points, bounded by polygon extent
# taken from ibi sampling manuscript functions
# 'clip_poly' is shapefile input object
# 'spacing' is spacing between points, as degrees
grid_est <- function(clip_poly, spacing = 0.02){
  
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
#' get seagrass depth estimates for an entire sample grid
#'
#' @param grid_in SpatialPoints object of locations to estimate seagrass depth, created using \code{\link{grid_est}}
#' @param dat_in SpatialPointsDataFrame of seagrass depth points for sampling with \code{grid_in}
#' @param buff radius of buffer in dec degrees around each sample location for estimating seagrass depth estimates
#' @param rem_miss logical indicating if unestimable points are removed from the output, default \code{TRUE}
#' @param trace logical indicating if progress is returned in console, default \code{FALSE}
#' @param out_sens logical indicating if output should include lower and upper estimates of seagrass growth limits
#' @param ... arguments passed to \code{\link{sens}}
#' 
#' @details This function estimates three seagrass depth of colonization values for each point in a sampling grid.  Functions \code{\link{buff_ext} and \code{\link{doc_est}} are used iteratively for each point in the sample grid.
#' 
#' @import sp
#' 
#' @return 
doc_est_grd <- function(grid_in, dat_in, radius = 0.06, rem_miss = TRUE, trace = FALSE, out_sens = F, ...){
      
  # get estimates for each point
  maxd <- vector('list', length = length(grid_in))
  for(i in 1:length(grid_in)){
    
    if(trace) cat(i, 'of', length(grid_in), '\n')
      
    eval_pt <- grid_in[i, ]
    ests <- try({
      buff_pts <- buff_ext(dat_in, eval_pt, buff = radius)
  	  est_pts <- data.frame(buff_pts)
      ests <- doc_est(est_pts)
      ests
    }, silent = T)
    
  	if('try-error' %in% class(ests)){ ests <- rep(NA, 9)
    } else {
      get_ests <- c('z_cmin', 'z_cmed', 'z_cmax')
      ests <- sens(ests, trace = F, ...)
      lower <- unlist(attr(ests, 'lower_est')[get_ests])
      upper <- unlist(attr(ests, 'upper_est')[get_ests])
      actual <- unlist(attributes(ests)[get_ests])
      ests <- c(actual, lower, upper)
      names(ests) <- c(get_ests, paste0('l_', get_ests), paste0('h_', get_ests))
    }
    
    maxd[[i]] <- ests
    
  }
    
	# combine results in data_frame, convert to spatialpoints
	maxd <- data.frame(do.call('rbind', maxd)) 
  maxd <- sp::SpatialPointsDataFrame(coords = coordinates(grid_in), data = maxd)
  
  # remove missing
  if(rem_miss){
    non_empty <- !is.na(maxd@data[, 1])
    if(!any(non_empty)) maxd
    else maxd <- maxd[!is.na(maxd@data[, 1]), ]
  }
 
  # remove sensitivity if T
  if(!out_sens){
    rem_nms <- !grepl('^h_|^l_', names(maxd))
    maxd <- maxd[, rem_nms]
  }
  
  return(maxd)
  
}

######
# get legend from an existing ggplot object
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

######
# get prediction intervals for doc object
#
# doc_in doc input object
# level percent level of prediction intervals
# nsim number of monte carlo simulations for each value in newdata
# trace logical for counter output
sens <- function(doc_in, ...) UseMethod('sens')
sens.doc <- function(doc_in, level = 0.05, trace = T, remzero = T, ...){
   
  # get model from doc_in for vcov matrix
  mod <- attr(doc_in, 'logis_mod')
  if(!'nls' %in% class(mod)){
    message('No model in object')
    return(doc_in)
  }

  # get linear model from doc_in
  est_fun <- attr(doc_in, 'est_fun')
  if(!'function' %in% class(est_fun)){
    message('Inadequate logistic model')
    return(doc_in)
  }
  
  # predictor value
  Depth <- coefficients(mod)['xmid']
  names(Depth) <- 'Depth'
 
  if(trace) cat("\n")

  ### get lower and upper bounds on doc estimates
  # var for zcmin, zcmed, zcmax are all different
  # all depend on variance/covariance of beta/gamma 

  vcovmod <- vcov(mod)
  betavar <- vcovmod['xmid', 'xmid']
  gammavar <- vcovmod['scal', 'scal']
  covar <- vcovmod['xmid', 'scal']
  
  # quantile to eval given level
  zquant <- qnorm(1 - (level/2))
 
  ## 
  # zcmin = beta - 2gamma
  zcmin_var <- betavar + 4 * gammavar - 4 * covar
  zcmin_print <- zquant * sqrt(zcmin_var)

  ## 
  # zcmed = beta
  zcmed_var <- betavar
  zcmed_print <- zquant * sqrt(zcmed_var)
     
  ##
  # zcmax = beta + 2gamma
  zcmax_var <- betavar + 4 * gammavar + 4 * covar
  zcmax_print <- zquant * sqrt(zcmax_var)

  ##
  # lower prediction intervals
  z_cmin <- attr(doc_in, 'z_cmin') - zcmin_print
  z_cmed <- attr(doc_in, 'z_cmed') - zcmed_print
  z_cmax <- attr(doc_in, 'z_cmax') - zcmax_print
  lower_est <- list(
    lower_shift = -1 * c(zcmin_print, zcmed_print, zcmax_print), 
    z_cmin = z_cmin, z_cmed = z_cmed, z_cmax = z_cmax
    )
    
  # upper estimates based on uncertainty
  z_cmin <- attr(doc_in, 'z_cmin') + zcmin_print
  z_cmed <- attr(doc_in, 'z_cmed') + zcmed_print
  z_cmax <- attr(doc_in, 'z_cmax') + zcmax_print
  upper_est <- list(
    upper_shift = c(zcmin_print, zcmed_print, zcmax_print), 
    z_cmin = z_cmin, z_cmed = z_cmed, z_cmax = z_cmax
    )
  
  # replace all estimates with NA if z_cmax prediction interval includes zero
  if(remzero & lower_est$z_cmax <= 0){

    attr(doc_in, 'z_cmax') <- NA
    attr(doc_in, 'z_cmin') <- NA
    attr(doc_in, 'z_cmed') <- NA
    
    return(doc_in)
    
  }
  
  # output
  # fill lower_est, upper_est attributes
  attr(doc_in, 'lower_est') <- lower_est
  attr(doc_in, 'upper_est') <- upper_est
  return(doc_in)
  
}

######
#' Get seagrass depth of colonization estimates at secchi locations
#' 
#' Get seagrass depth of colonization estimates at secchi locations using IWR database records, seagrass depth points, and segment polygon data.  Light requirements are also returned.
#'
#' @param secc_dat SpatialPointsDataFrame of secchi data, can be from any location 
#' @param sgpts_shp SpatialPointsDataFrame of seagrass depth points to sample
#' @param seg_shp SpatialPlygonsDataFrame of segment polygon data, must column named as `segment' indicating the corresponding water body id for eahc segment
#' @param kz numeric value indicating conversion factor for the product of secchi depth and light extinction coefficient
#' @param radius sampling radius for estimating seagrass depth of colonization in decimal degress
#' @param seg_pts_yr numeric indicating year of seagrass coverage data
#' @param trace logical indicating if progress is returned to console
#' 
#' @import dplyr
#' 
#' @return A four-element list where the first is a SpatialPolygonsDataFrame of the segment, the second is a data frame of all dates of all secchi data for the segment and the spatially-referenced depth of colonization estimate, the third is a summarized version of the second element for all unique locations with secchi data averaged across dates, and the third is depth of colonization data matched to the nearest date of the secchi data for the seagrass coverage.  The third and fourth elements include light requirements and segment names for each location.  
secc_doc <- function(secc_dat, sgpts_shp, seg_shp, kz = 1.7, radius = 0.2, seg_pts_yr, trace = F){
    
  if('character' %in% class(seg_pts_yr)) seg_pts_yr <- as.numeric(seg_pts_yr)
  if('factor' %in% class(seg_pts_yr)) stop('seg_pts_yr cannot be a factor')
  if(!'seg' %in% names(seg_shp)) stop('seg_shp input does not have seg column')
  
  # clip secchi by seg
  # clip secchi data by segments
  sel <- !is.na(secc_dat %over% seg_shp)[, 1]
  secc <- secc_dat[sel, ]
  
  # stop if no secchi data in segment
  if(nrow(secc) == 0) stop('No secchi data for segment')
  
  # get unique locations of secchi data
  uni_secc <- data.frame(secc)[, c('Station_ID', 'Longitude', 'Latitude')]
  uni_secc <- unique(uni_secc)
  uni_secc <- SpatialPointsDataFrame(
    coords = uni_secc[, c('Longitude', 'Latitude')], 
    data = uni_secc[, 'Station_ID', drop = F]
    )

  if(trace) cat('Estimating seagrass depth of colonization for radius', radius, 'at points...\n')
  
  # get sg doc estimates for each location with secchi data
  maxd <- list()
  maxd_conf <- list()
  for(i in 1:length(uni_secc)){
    
    if(trace) cat(length(uni_secc) - i, '\t')
    
    eval_pt <- uni_secc[i, ]
    ests <- try({
      buff_pts <- buff_ext(sgpts_shp, eval_pt, buff = radius)
      est_pts <- data.frame(buff_pts)
      doc_single <- doc_est(est_pts)
      doc_single <- sens(doc_single)
      upper_conf <- attr(doc_single, 'upper_est')$upper_shift
      lower_conf <- attr(doc_single, 'lower_est')$lower_shift
      bound_conf <- upper_conf - lower_conf
      z_cmax <- attr(doc_single, c('z_cmax'))
      c(z_cmax, bound_conf)
    },  silent = T)
  	if('try-error' %in% class(ests)) ests <- c(NA, NA)
    maxd[[i]] <- ests[1]
    maxd_conf[[i]] <- ests[2]
    
  }
  
  # dataframe of all maximum depth results
  maxd <- data.frame(uni_secc, z_cmax_all = do.call('c', maxd), maxd_conf = do.call('c', maxd_conf))
  
  # all secchi data, all dates
  all_dat <- merge(data.frame(secc), 
    maxd[, !names(maxd) %in% c('Longitude', 'Latitude')],
    by = 'Station_ID')
  
  # matched seagrass year data with closest secchi date
  near_dat <- split(all_dat, all_dat$Station_ID)
  near_dat <- llply(
    near_dat, 
    .fun = function(x){
    
      x <- x[which.max(x$Date), ]
      x$diff <- seg_pts_yr - as.numeric(strftime(x$Date, '%Y'))
      x$SD <- as.numeric(as.character(x$SD))
      x
    
  })
  near_dat <- do.call('rbind', near_dat)
  row.names(near_dat) <- 1:nrow(near_dat)
  
  # get average secchi by date
  ave_secc <- ddply(
    data.frame(secc),
    .variable = 'Station_ID',
    .fun = function(x) mean(as.numeric(x$SD), na.rm = T)
    )
  names(ave_secc)[names(ave_secc) %in% 'V1'] <- 'SD'
  ave_dat <- merge(data.frame(ave_secc), maxd, by = c('Station_ID'))

  # get light requirements for ave dat and near dat
  ave_dat <- mutate(ave_dat, 
    light = 100 * exp(-z_cmax_all * kz/SD),
    seg = NA
    )
  near_dat <- mutate(near_dat, 
    light = 100 * exp(-z_cmax_all * kz/SD),
    seg = NA
    )

  # convert ave dat and near dat to spatialpointsdataframe
  coords <- ave_dat[, c('Longitude', 'Latitude')]
  ave_dat <- ave_dat[, !names(ave_dat) %in% c('Longitude', 'Latitude')]
  coordinates(ave_dat) <- coords
  coords <- near_dat[, c('Longitude', 'Latitude')]
  near_dat <- near_dat[, !names(near_dat) %in% c('Longitude', 'Latitude')]
  coordinates(near_dat) <- coords
  
  # get segment locations for each station
  for(seg in as.character(seg_shp$seg)){
    
    to_sel <- seg_shp[seg_shp$seg %in% seg, ]
    ave_sel <- !is.na(ave_dat %over% to_sel)[, 1]
    near_sel <- !is.na(near_dat %over% to_sel)[, 1]
    ave_dat[ave_sel, 'seg'] <- as.character(seg)
    near_dat[near_sel, 'seg'] <- as.character(seg)
    
  }
    
  # output, all data and averaged secchi data
  out <- list(
    seg_shp = seg_shp,
    all_dat = all_dat, 
    ave_dat = data.frame(ave_dat, stringsAsFactors = F),
    near_dat = data.frame(near_dat, stringsAsFactors = F)
  )
  
  if(trace) cat('\n')
  
  return(out)

}

######
# formet of label precision in ggplot axes
fmt <- function(){
    function(x) format(x,nsmall = 2,scientific = FALSE)
}

######
#' Convert a data frame to raster
#'
#' @param dat_in data.frame to conver to raster, must include lon, lat columns named accordingly
#' @param fill_col chr string of column of dat_in to fill the raster
#' 
make_rast_fun <- function(dat_in, fill_col){
  
  coordinates(dat_in) <- c('lon', 'lat')

  rast <- raster()
  extent(rast) <- extent(dat_in)
  ncol(rast) <- length(unique(dat_in$lon))
  nrow(rast) <- length(unique(dat_in$lat))

  out <- rasterize(dat_in, rast, data.frame(dat_in[, fill_col]), fun = mean)  
  
  return(out)
  
}

######
#' Estimate cumulative distribution curves for in situ and satellite estimated kd values, then get linear model for corrections
#' 
#' @param dat_in data.frame with in situ and satellite measured kd values, including cumulutive sums of each vector that are scaled by the maximum.  The data.frame is ordered based on the ascending values of in situ kd
#' @param xsitu chr string of column name for in situ kd values
#' @param ysitu chr string of column name of cumulative sum of satellite kd values
#' @param xsats chr string of column for satellite kd values
#' @parma ysats chr string of column name for cumulative sum of satellite kd values
#' 
#' @return A list with two regression objects, the first is the regression for the in situe curve and the second is the regression for the satellite estimated curve
kd_mod <- function(dat_in, xsitu, ysitu, xsats, ysats){
  
  # format data for glm, needs column of denominators for each prop
  to_mod <- data.frame(dat_in)

  # create the models
  form_in1 <- substitute(y ~ x, 
    list(y = as.name(ysitu), x = as.name(xsitu)))
  form_in2 <- substitute(y ~ x, 
    list(y = as.name(ysats), x = as.name(xsats)))
  mod1 <- lm(form_in1, data = to_mod)
  mod2 <- lm(form_in2, data = to_mod)
  
  # return output
  mods <- list(mod1, mod2)
  return(mods)
  
}

######
#' Use the output of kd_mod to get corrected kd values
#'
#' @param mods_in list of models returned from kd_binomod
#' @param dat_in data.frame that was used as input to kd_binomod
#' @param xsat numeric vector of satellite kd values to correct
#' @param xrng numeric vector of range of x values for the plot
#' @param steps numeric vector of number of values for creating the logistic curve in the plot
#' @param plot logical to return a plot, otherwise the corrected values are returned
#' @param xsitu character string of column name of in situ kd values in dat_in
#' @param xsats character string of column name of satellite kd values in dat_in
#' @param ysitu character string of column name of cumulative in situ kd values in dat_in
#' @param ysats character string of column name of cumulative satellite kd values in dat_in
#' @param ylim two element numeric vector indicating the y-axis range for the plot
#'
#' @return Vector of corrected kd values if \code{plot = FALSE}, otherwise a plot showing the empirical data used to estimate each model and a pathway showing the corrected satellite data.
#'
kd_backsat <- function(mods_in, dat_in, xsat, xrng = c(0, 2), steps = 2000,
  plot = FALSE, xsitu = 'kPAR', xsats = 'layer', ysitu = 'cumkPAR',
  ysats = 'cumlayer', ylim = c(0, 1)){
  
  # get corrected kd from satellite kd
  preddat <- data.frame(xsat)
  names(preddat) <- xsats
  yback <- predict(mods_in[[2]], newdata = preddat, type = 'response')
  coefs <- coefficients(mods_in[[1]])
  xback <- (yback - coefs[1])/coefs[2]
  
  if(plot){
    
    # get predictions for plotting
    xvals <- seq(xrng[1], xrng[2], length = steps)
    xvals <- data.frame(xvals)
    names(xvals) <- xsitu
    ysitu_lns <- predict(mods_in[[1]], newdata = xvals, type = 'response')
    names(xvals) <- xsats
    ysats_lns <- predict(mods_in[[2]], newdata = xvals, type = 'response')
    bilines <- data.frame(xvals, ysitu_lns, ysats_lns)
    names(bilines) <- c('xvals', 'ysitu_lns', 'ysats_lns')
    
    # data for back transformation path
    xs <- rbind(xsat, xback)
    xs <- c(apply(xs, 2, function(x) c(x[1], x[1], x[2], x[2])))
    ys <- c(apply(matrix(yback), 1, function(x) c(0, x, x, 0)))
    backdat <- data.frame(
      x = xs,
      y = ys, 
      group = rep(seq(1, length(yback)), each = 4)
    )
    
    # rename raw variables
    p_nms <- c('xpt1', 'ypt1', 'xpt2', 'ypt2')
    names(dat_in)[names(dat_in) %in% c(xsitu, ysitu, xsats, ysats)] <- p_nms
    
    p <- ggplot(dat_in, aes(x = xpt1, y = ypt1, colour = 'In Situ')) + 
      geom_point() +
      geom_point(aes(x = xpt2, y = ypt2, colour = 'Satellite')) + 
      geom_line(data = bilines, aes(x = xvals, y = ysitu_lns, 
        colour = 'In Situ')) + 
      geom_line(data = bilines, aes(x = xvals, y = ysats_lns,
        colour = 'Satellite')) +
      geom_path(data = backdat, aes(x = x, y = y, group = group, colour = group), 
        colour = 'black') +
      geom_point(dat = backdat, aes(x = x, y = y, group = group, colour = group), 
        colour = 'black', pch = 16, size = 4) +
      scale_x_continuous(name = expression(italic(K [d]))) + 
      scale_y_continuous(name = expression(paste('Cumulative ', italic(K [d]))),
        limits = ylim) +
      theme_classic() + 
      theme(legend.title = element_blank(), 
        legend.position = c(0, 1), legend.justification = c(0, 1),
        text = element_text(size=20))
    
    return(p)
    
    }
  
  return(xback)
   
}


