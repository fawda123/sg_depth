# ######
# # processing of seagrass data
# # Aug. 2014
# 
# ######
# # merge segment and depth data, Hagy supplementary data
# seg_dat <- read.table('dat/sg_segs.txt', sep = ',', header = T)
# dep_dat <- read.table('dat/sg_depths.txt', sep = ',', header = T)
# 
# sg_dat <- merge(seg_dat, dep_dat, by = 'SEGID', all = T)
# save(sg_dat, file = 'dat/sg_dat.RData')
# 
# ######
# # merge secchi data with location data
# stat_dat <- read.csv('dat/Stations.csv')
# secc_dat <- read.csv('dat/IWR40_Secchi_Depth.csv')
# 
# secc_dat <- merge(secc_dat, stat_dat[, c('Station_ID', 'Latitude', 'Longitude')],
#   by.x = 'STATION', by.y = 'Station_ID', all.x = T)
# save(secc_dat, file = 'dat/secc_dat.RData')
# 
# # for shapefile, slipped by segment in Arc
# write.csv(secc_dat, 'dat/secc_dat.csv', quote = F, row.names = F)
# 
# ######
# # subset segments shapefile by those that have depth of col data from manu
# 
# segs_shp <- readShapePoly(
#   'M:/GIS/seagrass/segs.shp',
#   proj4string = CRS("+proj=longlat +datum=WGS84")
#   )
# load('dat/sg_dat.RData')
# 
# sel_vec <- as.numeric(as.character(segs_shp$SEGID)) %in% unique(sg_dat$SEGID)
# segs_shp <- segs_shp[sel_vec, ]
# 
# # save subset shapefile
# # note this does not have projection
# writePolyShape(
#   x = segs_shp, 
#   fn = 'M:/GIS/seagrass/sg_segs.shp'
#   )
# 
######
# renaming columns for shiny app
# convert to positive depth values
# floor depth values at zero

to_load <- list.files('seagrass_gis', '\\.shp$')
shps <- vector('list', length = length(to_load))
names(shps) <- to_load
for(i in to_load) 
  shps[[i]] <- readShapeSpatial(paste0('seagrass_gis/', i))

shps <- shps[grep('^sgpts', names(shps))]

# which file to convert
ind <- 4
x <- shps[[ind]]

# rename depth, seagrass columns, specific to each file
# names(x)
# names(x)[names(x) %in% 'depth'] <- 'Depth'
# names(x)[names(x) %in% 'SEAGRASS'] <- 'Seagrass'

# depth as positive, floor to zero
# x$Depth <- pmax(0, x$Depth)
# x$Depth <- pmax(0, -1 * x$Depth)

# retain only relevant columns
x <- x[, names(x) %in% c('Depth', 'Seagrass')]

# convert seagrass values to 'Continuous', 'Discontinuous'
# for IRL, 9113 is Patchy, 9116 is continuous, see docs on L drive
# newlevs <- c('Continuous', 'Discontinuous')
# levels(x$Seagrass) <- newlevs

# save 
writeSpatialShape(
  x = x, 
  fn = paste0('seagrass_gis/', names(shps)[ind])
  )

######
# get secchi data from IWR40 database that correspond to my segments
# all segs was created in ArcMap
# 'iwr_40_stations.RData' and 'iwr40_secchi.RData' are raw data from IWR40 access db
# 
# # load secchi, station data
# load(file = 'dat/iwr40_stations.RData')
# load(file = 'dat/iwr40_secchi.RData')
# 
# # merge secchi with lat/long stations info
# stat <- stat[, c('Station_ID', 'Latitude', 'Longitude')]
# dat <- merge(secc, stat, by = 'Station_ID', all.x = T)
# 
# # remove QAQC flags, retain relevant columns
# dat <- dat[dat$RCode == '', ]
# dat <- dat[, c('Station_ID', 'Date_Time', 'Secchi', 'Latitude', 'Longitude')]
# 
# # datetimestamp to posix, eastern time, no dst
# dat$Date_Time <- as.POSIXct(as.character(dat$Date_Time), 
#   format = '%m/%d/%Y %H:%M:%S', tz = 'America/Jamaica')
# 
# ##
# # load segments from widget, clip stations accordingly
# library(maptools)
# library(sp)
# 
# # load segs
# segs <- readShapeSpatial('seagrass_gis/all_segs.shp')
# 
# # secchi as spatial points
# coordinates(dat) <- dat[, c('Longitude', 'Latitude')]
# dat <- SpatialPoints(dat)
# 
# # clip secchi data by segments
# sel <- !is.na(dat %over% segs)[, 1]
# dat <- dat[sel, ]
# 
# # save
# secc_seg <- dat
# save(secc_seg, file = 'dat/secc_seg.RData')