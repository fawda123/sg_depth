library(maptools)
library(plyr)

source('funcs.r')

# function for doc ests at secchi locs
secc_doc <- function(seg_shp, sgpts_shp, radius = 0.2, trace = F){
    
  # secchi data
  load(file = 'data/secc_seg.RData')
  
  # segment and seagrass points data
  if('character' %in% class(seg_shp))
    seg_shp <- readShapeSpatial(paste0('seagrass_gis/', seg_shp))
  if('character' %in% class(sgpts_shp))
    sgpts_shp <- readShapeSpatial(paste0('seagrass_gis/', sgpts_shp))
  
  # clip secchi by seg
  # clip secchi data by segments
  sel <- !is.na(secc_seg %over% seg_shp)[, 1]
  secc <- secc_seg[sel, ]
  
  # get unique locations of secchi data
  uni_secc <- data.frame(secc)[, c('Station_ID', 'Longitude', 'Latitude')]
  uni_secc <- unique(uni_secc)
  uni_secc <- SpatialPointsDataFrame(
    coords = uni_secc[, c('Longitude', 'Latitude')], 
    data = uni_secc[, 'Station_ID', drop = F]
    )
  
  # get sg doc estimates for each location with secchi data
  maxd <- list()
  for(i in 1:length(uni_secc)){
    
    if(trace) cat(length(uni_secc) - i, '\t')
    
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
  
  all_dat <- merge(data.frame(secc), 
    maxd[, !names(maxd) %in% c('Longitude', 'Latitude')],
    by = 'Station_ID')
  
  # get average secchi by date
  ave_secc <- ddply(
    data.frame(secc),
    .variable = 'Station_ID',
    .fun = function(x) mean(as.numeric(x$SD), na.rm = T)
    )
  names(ave_secc)[names(ave_secc) %in% 'V1'] <- 'SD'
    
  # merge averaged secchi with zmax
  ave_dat <- merge(data.frame(ave_secc), maxd, by = c('Station_ID'))
  
  # output, all data and averaged secchi data
  out <- list(
    seg_shp = seg_shp,
    all_dat = all_dat, 
    ave_dat = ave_dat
  )
  return(out)

}

seg_shp <- 'seg_902.shp'
sgpts_shp <- 'sgpts_2010_902.shp'

rad <- 0.07
tmp <- secc_doc(seg_shp, sgpts_shp, radius = rad, trace = T)

# plot(tmp$seg_shp)
# with(tmp$ave_dat, points(Longitude, Latitude)) 
# 
# plot(zmax_all ~ SD, data = tmp$ave_dat)
# 
# plot(table(strftime(tmp$all_dat$Date, '%Y')))

near_date <- tmp$all_dat
near_date <- split(near_date, near_date$Station_ID)
near_date <- llply(
  near_date, 
  .fun = function(x){
    
    x <- x[which.max(x$Date), ]
    x$diff <- 2010 - as.numeric(strftime(x$Date, '%Y'))
    x
    
  })
near_date <- do.call('rbind', near_date)
near_date$wts <- scales::rescale(near_date$diff, to = c(0.5, 7))
near_date$diff <- scales::rescale(near_date$diff, to = c(1, 3))

par(mfrow = c(1, 2))

plot(zmax_all ~ SD, near_date, cex = near_date$diff, pch = 16, main = 'Nearest', ylim = c(0, 2))
mod <- lm(zmax_all ~ as.numeric(SD), near_date)
wt_mod <- lm(zmax_all ~ as.numeric(SD), near_date, weights = near_date$wts)
abline(reg = mod, lty =  2)
abline(reg = wt_mod)

plot(zmax_all ~ SD, tmp$ave_dat, pch = 16, main = 'Averaged', ylim = c(0,2))
mod <- lm(zmax_all ~ as.numeric(SD), tmp$ave_dat)
abline(reg = mod)

#####
# all data for TB

load('seagrass_gis/shps.RData')
seg_shp <- shps[[grep('seg_902', names(shps))]]
sgpts <- shps[grep('^sgpts.*902\\.shp$', names(shps))]

rad <- 0.07

out_ls <- vector('list', length(sgpts))
names(out_ls) <- names(sgpts)
for(sgpt in names(sgpts)){
  cat(sgpt, '\n')
  tmp <- secc_doc(seg_shp, sgpts[[sgpt]], radius = rad, trace = T)
  out_ls[[sgpt]] <- tmp[[2]]
}

