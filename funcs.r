######
# functions for seagrass depth of col estimates

######
# formats bathymetric seagrass data for estimating depth of col
# output is data frame used as input to 'max_est' func
# 'sgpts' is spatial points of seagrass depth and categories
# 'bins' are depth bins to summarize data (meters)
sg_bins <- function(sgpts, bins = 0.25){
  
  # sanity checks
  if(!any(c('SpatialPointsDataFrame', 'SpatialPoints') %in% class(sgpts)))
    stop('pts must be of class SpatialPointsDataFrame or SpatialPoints')
  
  # data frame of seagrass depth soundings
  sgpts <- data.frame(sgpts)
  
  # bin the depth data into 0.25 m intervals
  bins <- -1*seq(0, -1*min(sgpts$depth), by = bins)
  sgpts$depth_bins <- cut(
    sgpts$depth, 
    breaks = bins
    )

  # proportion of points in each bin with continuous seagrass
  sg_prp_cont <- sgpts[sgpts$Descript %in% 'Continuous Seagrass', ]
  sg_prp_cont <- table(sg_prp_cont$depth_bins)/table(sgpts$depth_bins)
  sg_prp_cont[is.na(sg_prp_cont)] <- 0

  # proportion of points in each bin with seagrass, all cats
  sg_prp_all <- sgpts[!is.na(sgpts$Descript), ]
  sg_prp_all <- table(sg_prp_all$depth_bins)/table(sgpts$depth_bins)
  sg_prp_all[is.na(sg_prp_all)] <- 0
  
  # combine the above into a simple data frame
  sg_prp <- data.frame(
    cont = as.numeric(sg_prp_cont)/sum(sg_prp_all), 
    all = as.numeric(sg_prp_all)/sum(sg_prp_all),
    bins = rev(-1*bins[-c(length(bins))])
    )
  
  # return output
  return(sg_prp)
  
  }

######
# function for estimating max depth of col
# 'dat_in' is data frame of binned proportions, returned from 'sg_bins' func
# 'var' is seagrass variable to eval - 'all' or 'cont'
# 'mod' is logical specifiying if linear model used to id Z is returned
# note that the orig model is bins ~ prop, returned is coefficient for prop ~ bins
max_est <- function(dat_in, var, mod = F){

  # maximum proportion for any depth bin
  max_val <- max(dat_in[, var])
  
  # use depth bins than 1% of maximum prop
  sel <- which(dat_in[, var] < (0.01 * max_val), arr.ind = T)
  sel <- max(sel)
  reg_dat <- dat_in[sel:which.max(dat_in[, var]), ]
  
  # regression of bins vs prop 
  form_in <- formula(paste('bins', var, sep = '~'))
  reg <- lm(form_in, reg_dat)
  
  # return model coefficients if T
  if(mod){
    coeffs <- reg$coefficients
    int <- -1 * coeffs[1]/coeffs[2]
    slo <- 1/coeffs[2]
    return(c(intercept = int, slope = slo))
    }
  
  # max depth of col
  max_depth <- as.numeric(reg$coefficients[1])
  
  # get depth of col at median
  med_val <- 0.5 * max_val
  
  # use linear interp
  med_depth <- approx(reg_dat[, var], reg_dat[, 'bins'], xout = med_val)
  med_depth <- med_depth$y
  
  # median depth of col using above regression
#   med_depth <- reg$coefficients[1] + (reg$coefficients[2] * med_val)

  # return output
  out <- c(zmax = max_depth, z50 = med_depth)
  return(out)
  
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