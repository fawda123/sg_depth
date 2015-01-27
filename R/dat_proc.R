# ######
# # processing of seagrass data
# # Aug. 2014
# 
# ######
# # merge segment and depth data, Hagy supplementary data
# seg_dat <- read.table('data/sg_segs.txt', sep = ',', header = T)
# dep_dat <- read.table('data/sg_depths.txt', sep = ',', header = T)
# 
# sg_dat <- merge(seg_dat, dep_dat, by = 'SEGID', all = T)
# save(sg_dat, file = 'data/sg_dat.RData')
# 
# ######
# # merge secchi data with location data
# stat_dat <- read.csv('data/Stations.csv')
# secc_dat <- read.csv('data/IWR40_Secchi_Depth.csv')
# 
# secc_dat <- merge(secc_dat, stat_dat[, c('Station_ID', 'Latitude', 'Longitude')],
#   by.x = 'STATION', by.y = 'Station_ID', all.x = T)
# save(secc_dat, file = 'data/secc_dat.RData')
# 
# # for shapefile, slipped by segment in Arc
# write.csv(secc_dat, 'data/secc_dat.csv', quote = F, row.names = F)
# 
# ######
# # subset segments shapefile by those that have depth of col data from manu
# 
# segs_shp <- readShapePoly(
#   'M:/GIS/seagrass/segs.shp',
#   proj4string = CRS("+proj=longlat +datum=WGS84")
#   )
# load('data/sg_dat.RData')
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

#####
# get secchi data from IWR40 database that correspond to my segments
# all segs was created in ArcMap
# 'iwr_40_stations.RData' is station locations from IWR40 access db
# 'SD.east.txt', 'SD.nw.txt', 'SD.sw.txt' are secchi text files from Jim's work 

# load station data
load(file = 'data/iwr40_stations.RData')

# load secchi data
secc1 <- read.table('data/SD.east.txt', header = T, sep = '\t', 
  stringsAsFactors = F)
secc2 <- read.table('data/SD.nw.txt', header = T, sep = '\t', 
  stringsAsFactors = F)
secc3 <- read.table('data/SD.sw.txt', header = T, sep = '\t', 
  stringsAsFactors = F)
secc <- rbind(secc1, secc2, secc3)
names(secc)[1] <- 'Station_ID'

# merge secchi with lat/long stations info
stat <- stat[, c('Station_ID', 'Latitude', 'Longitude')]
dat <- merge(secc, stat, by = 'Station_ID', all.x = T)

# retain relevant columns
dat <- dat[!dat$SD.nond, ]
dat <- dat[, c('Station_ID', 'DateT', 'SD',
  'Latitude', 'Longitude')]

# remove stations w/o locations
dat <- dat[!is.na(dat$Latitude), ]

# datetimestamp to posix, eastern time, no dst
dat$Date <- as.Date(as.character(dat$DateT), format = '%m/%d/%Y')
dat$DateT <- NULL

##
# load segments from widget, clip stations accordingly
library(maptools)
library(sp)

# load segs
segs <- readShapeSpatial('seagrass_gis/all_segs.shp')

# secchi as spatial points data frame
coords <- dat[, c('Longitude', 'Latitude')]
dat <- dat[, !names(dat) %in% c('Longitude', 'Latitude')]
coordinates(dat) <- coords

# clip secchi data by segments
sel <- !is.na(dat %over% segs)[, 1]
dat <- dat[sel, ]

# save
secc_seg <- dat
save(secc_seg, file = 'data/secc_seg.RData')

######
# secchi data for all of Tampa Bay
# uses Jim's secchi data

rm(list = ls())

# load station data
load(file = 'data/iwr40_stations.RData')

# load secchi data
secc1 <- read.table('data/SD.east.txt', header = T, sep = '\t', 
  stringsAsFactors = F)
secc2 <- read.table('data/SD.nw.txt', header = T, sep = '\t', 
  stringsAsFactors = F)
secc3 <- read.table('data/SD.sw.txt', header = T, sep = '\t', 
  stringsAsFactors = F)
secc <- rbind(secc1, secc2, secc3)
names(secc)[1] <- 'Station_ID'

# merge secchi with lat/long stations info
stat <- stat[, c('Station_ID', 'Latitude', 'Longitude')]
dat <- merge(secc, stat, by = 'Station_ID', all.x = T)

# retain relevant columns
dat <- dat[!dat$SD.nond, ]
dat <- dat[, c('Station_ID', 'DateT', 'SD',
  'Latitude', 'Longitude')]

# remove stations w/o locations
dat <- dat[!is.na(dat$Latitude), ]

# datetimestamp to posix, eastern time, no dst
dat$Date <- as.Date(as.character(dat$DateT), format = '%m/%d/%Y')
dat$DateT <- NULL

# tampa bay shapefile
tb_seg <- readRDS('M:/docs/manuscripts/sgdepth_manu/data/tb_seg.rds')


######
# format all shapefiles for tb old tampa bay

# Tampa Bay year folders on L drive were copied to desktop 

# segment polygon
seg <- readShapeSpatial('seagrass_gis/seg_902.shp')
seg_buff <- rgeos::gBuffer(seg, width = 0.02)

# list of shapefiles in the directory
path <- 'C:/Users/mbeck/Desktop/gis_tmp'
ptfls <- list.files(path, '*\\.shp$', recursive = T, full.names = T)

# manually went through and ided which were sgpts
inds <- c(4, 8, 11, 14, 17, 20, 23, 27, 31, 35, 38)

# go through each ind
ind <- 38
orig <- readShapeSpatial(ptfls[ind])

tmp <- orig
head(data.frame(tmp))
  
# columns to keep
dep_col <- 'depth'
sg_col <- 'Descript'
keep_cols <- c(dep_col, sg_col)
tmp <- tmp[, keep_cols]

# rename depth, seagrass columns, specific to each file
names(tmp)
names(tmp)[names(tmp) %in% dep_col] <- 'Depth'
names(tmp)[names(tmp) %in% sg_col] <- 'Seagrass'

# depth as positive, floor to zero
# x$Depth <- pmax(0, x$Depth)
tmp$Depth <- pmax(0, -1 * tmp$Depth)

# convert seagrass values to 'Continuous', 'Discontinuous'
levels(tmp$Seagrass)
newlevs <- c('Continuous', 'Discontinuous')
levels(tmp$Seagrass) <- newlevs

# extract by buffer
sel <- !is.na(tmp %over% seg_buff)
tmp <- tmp[sel, ]

plot(seg_buff)
points(tmp, col = sample(colors(), 1))

# save 
ptfls[ind]
writeSpatialShape(
  x = tmp, 
  fn = 'seagrass_gis/sgpts_2010_902.shp'
  )

######
# format tampa bay seagrass depht points for whole bay
# used in manuscript, maybe

# load original file
sgpts_2010_tb <- foreign::read.dbf('L:/lab/FloridaCriteria/Seagrass_vs_Depth/09-Tampa_Bay/2010/Tampa_2010_Segments.dbf')

x <- sgpts_2010_tb[, c('lat', 'lon', 'depth', 'Descript')]

# rename depth, seagrass columns, specific to each file
names(x)
names(x)[names(x) %in% 'depth'] <- 'Depth'
names(x)[names(x) %in% 'Descript'] <- 'Seagrass'

# depth as positive, floor to zero
x$Depth <- pmax(0, -1 * x$Depth)

# convert seagrass values to 'Continuous', 'Discontinuous'
# for IRL, 9113 is Patchy, 9116 is continuous, see docs on L drive
newlevs <- c('Continuous', 'Discontinuous')
levels(x$Seagrass) <- newlevs

# convert to spatialpointsdataframe
coords <- x[, c('lon', 'lat')]
names(coords) <- c('coords.x1', 'coords.x2')
x <- x[, c('Depth', 'Seagrass')]
x <- SpatialPointsDataFrame(coords, x)

sgpts_2010_tb <- x

saveRDS(sgpts_2010_tb, 'data/sgpts_2010_tb.rds')

######
# save all shapefiles to RData for quicker load
# used in shiny
to_load <- list.files('seagrass_gis', '\\.shp$')
shps <- vector('list', length = length(to_load))
names(shps) <- to_load
for(i in to_load) 
  shps[[i]] <- readShapeSpatial(paste0('seagrass_gis/', i))
save(shps, file = 'seagrass_gis/shps.RData')

