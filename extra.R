######
# old tampa bay test dataset

######
# load spatial data

shps <- list.files('M:/GIS/seagrass', '^.*902.*shp$', full.names = T)

secchi_shp <- readShapePoly(
  shps[1],
  proj4string = CRS("+proj=longlat +datum=WGS84")
  )

seg_shp <- readShapePoly(
  shps[2]
  )

sgpoly_shp <- readShapePoly(
  shps[3]
  )

sgpts_shp <- readOGR(
  shps[4],
  gsub('.shp', '', basename(shps[4]))
  )

######
# repeate depth of col analysis from Jim's paper

# data frame of seagrass depth soundings
sgpts <- data.frame(sgpts_shp)

# bin the depth data into 0.25 cm intervals
bins <- -1*seq(0, -1*min(sgpts$depth), by = 0.25)
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

# function for estimating max depth of col
max_est <- function(dat_in, var){

  # maximum proportion for any depth bin
  max_val <- max(dat_in[, var])
  
  # use depth bins than 1% of maximum prop

  reg <- dat_in[which(dat_in[, var] > (0.01 * max_val))[1]:nrow(dat_in), ]
  
  # regression of bins vs prop 
  reg <- lm(bins ~ all, reg)
  
  # max depth of col
  max_depth <- reg$coefficients[1]
  
  med_val <- 0.5 * max_val

  # median depth of col
  med_depth <- reg$coefficients[1] + reg$coefficients[2] * med_val

  data.frame(max_depth, med_depth)
    
  }
max_est(sg_prp, 'all')
max_est(sg_prp, 'cont')


to_plo <- sg_prp[40:54, ]
to_plo <- melt(to_plo, id.var = 'bins')

ggplot(to_plo, aes(x = bins, y = value, group = variable,
  colour = variable)) +
    geom_line() +
    geom_point() + 
    theme_bw()


