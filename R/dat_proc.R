######
# processing of seagrass data
# Aug. 2014

######
# use this to standardize data for manuscript/shiny, use manually
# renaming columns for shiny app
# convert to positive depth values
# floor depth values at zero

library(maptools)

to_load <- list.files('M:/GIS/seagrass', '^sgpts.*\\.shp$')
shps <- vector('list', length = length(to_load))
names(shps) <- to_load
for(i in to_load) 
  shps[[i]] <- readShapeSpatial(paste0('M:/GIS/seagrass/', i))

# which file to convert
ind <- 1
x <- shps[[ind]]

# rename depth, seagrass columns, specific to each file
names(x)
names(x)[names(x) %in% 'depth'] <- 'Depth'
names(x)[names(x) %in% 'SEAGRASS'] <- 'Seagrass'

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
stat <- iwr40_Stations[, c('Station_ID', 'Latitude', 'Longitude')]
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

# secchi as spatial points data frame
coords <- dat[, c('Longitude', 'Latitude')]
dat <- dat[, !names(dat) %in% c('Longitude', 'Latitude')]
coordinates(dat) <- coords

secc_all <- dat
save(secc_all, file = 'data/secc_all.RData')

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
raw <- foreign::read.dbf('M:/GIS/seagrass/sgpts_2010_tb.dbf')

x <- raw[, c('lat', 'lon', 'depth', 'Descript')]

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

save(sgpts_2010_tb, file = 'data/sgpts_2010_tb.RData')
save(sgpts_2010_tb, file = 'M:/docs/manuscripts/sgdepth_manu/data/sgpts_2010_tb.RData')

######
# format irl seagrass points for entire lagoon
# used in manuscript, maybe

# load original file
raw <- foreign::read.dbf('M:/GIS/seagrass/sgpts_2009_irl.dbf')

x <- raw[, c('Lat', 'Lon', 'Depth', 'HABITAT')]

# rename depth, seagrass columns, specific to each file
names(x)
names(x) <- c('lat', 'lon', 'Depth', 'Seagrass')

# depth as positive, floor to zero
x$Depth <- pmax(0, -1 * x$Depth)

# convert seagrass values to 'Continuous', 'Discontinuous'
# for IRL, 9113 is Patchy, 9116 is continuous, see docs on L drive
newlevs <- c(NA, 'Discontinuous', 'Continuous', NA, NA)
levels(x$Seagrass) <- newlevs

# convert to spatialpointsdataframe
coords <- x[, c('lon', 'lat')]
names(coords) <- c('coords.x1', 'coords.x2')
x <- x[, c('Depth', 'Seagrass')]
x <- SpatialPointsDataFrame(coords, x)

sgpts_2009_irl <- x

save(sgpts_2009_irl, file = 'data/sgpts_2009_irl.RData')
save(sgpts_2009_irl, file = 'M:/docs/manuscripts/sgdepth_manu/data/sgpts_2009_irl.RData')

######
# format choc bay seagrass points for bay
# used in manuscript

# load original file
raw <- foreign::read.dbf('M:/GIS/seagrass/sgpts_2007_choc.dbf')

x <- raw[, c('lat', 'lon', 'depth', 'SEAGRASS')]

# rename depth, seagrass columns, specific to each file
names(x)
names(x) <- c('lat', 'lon', 'Depth', 'Seagrass')

# depth as positive, floor to zero
x$Depth <- pmax(0, -1 * x$Depth)

# convert seagrass values to 'Continuous', 'Discontinuous'
newlevs <- c('Continuous', 'Discontinuous', 'Discontinuous', 'Discontinuous', 'Discontinuous')
levels(x$Seagrass) <- newlevs

# convert to spatialpointsdataframe
coords <- x[, c('lon', 'lat')]
names(coords) <- c('coords.x1', 'coords.x2')
x <- x[, c('Depth', 'Seagrass')]
x <- SpatialPointsDataFrame(coords, x)

sgpts_2007_choc <- x

save(sgpts_2007_choc, file = 'data/sgpts_2009_choc.RData')
save(sgpts_2007_choc, file = 'M:/docs/manuscripts/sgdepth_manu/data/sgpts_2007_choc.RData')

######
# save all shapefiles to RData for quicker load
# used in shiny
to_load <- c('seg_303.shp', 'seg_820.shp', 'seg_902.shp', 'seg_1502.shp', 'sgpts_2007_303.shp', 'sgpts_2006_820.shp', 'sgpts_2010_902.shp', 'sgpts_2009_1502.shp')
shps <- vector('list', length = length(to_load))
names(shps) <- to_load
for(i in to_load) 
  shps[[i]] <- readShapeSpatial(paste0('M:/GIS/seagrass/', i))
save(shps, file = 'data/shps.RData')
save(shps, file = 'M:/docs/manuscripts/sgdepth_manu/data/shps.RData')

######
# processing satellite derived water clarity (clarity, not kd) for Tampa Bay

rm(list = ls())

source('R/funcs.r')
library(magrittr) 
  
files <- list.files('data/satellite/Tampa_Bay', '_clarity\\.txt$|lat|lon', full.names = TRUE)

# get files
sats <- vector('list', length = length(files))
names(sats) <- files
for(fl in files){
  
  cat(fl, '\n')
  
  # import 
  tmp <- readLines(fl) %>% 
    strsplit(' .') %>% 
    unlist
  tmp <- tmp[nchar(tmp) > 0] %>% 
    as.numeric
  
  # remove outliers
  if(!grepl('lat|lon', fl)) tmp[tmp > 4] <- NA
  
  sats[[fl]] <- tmp
  
}
names(sats) <- gsub('\\.txt$', '', basename(names(sats)))
sats[['lat']] <- rev(sats[['lat']])
sats[['lon']] <- -1 * sats[['lon']]

sats <- do.call('cbind', sats)
sats <- data.frame(sats)
names(sats) <- gsub('^X', 'clarity_', names(sats))
names(sats) <- gsub('_clarity$', '', names(sats))

# clarity only, remove 2011, 2012, 2013
locs <- sats[, grepl('lat|lon', names(sats))]
sats <- sats[, !grepl('lat|lon|2003$|2004$|2005$|2011$|2012$|2013$', names(sats))]
sats_all <- apply(sats, 1, function(x) mean(x, na.rm = TRUE))

sats_all <- data.frame(lon = locs$lon, lat = locs$lat, clarity_ave = sats_all, sats)

##
# conver to raster
library(raster)
library(maptools)

sats_ave <- sats_all[, c('lon', 'lat', 'clarity_ave')]
sat_rast <- make_rast_fun(sats_ave, 'clarity_ave')

tb_sats <- list(ave_rast = sat_rast, sats_all = sats_all)
save(tb_sats, file = 'data/tb_sats_rast.RData')
save(tb_sats, file = 'M:/docs/manuscripts/sgdepth_manu/data/tb_sats.RData')

######
# processing satellite derived water clarity (kd) for Choctawhatchee Bay
# this is different from TB because it has not been validated with in situ

rm(list = ls())

library(magrittr) 
library(raster)
library(maptools)
library(sp)
source('R/funcs.R')

# files to import w/ choc kd values
files <- list.files('data/satellite/Choctawhatchee_Bay/', full.names = TRUE)

# get files
sats <- vector('list', length = length(files))
names(sats) <- files
for(fl in files){
  
  cat(fl, '\n')
  
  # import 
  tmp <- readLines(fl) %>% 
    strsplit(' .') %>% 
    unlist
  tmp <- tmp[nchar(tmp) > 0] %>% 
    as.numeric
  
  # remove outliers, zero values
  # remove very large kd values
  if(!grepl('lat|lon', fl)){
    tmp[tmp > 2] <- NA
    tmp[tmp == 0] <- NA
  }
  
  sats[[fl]] <- tmp
  
}
names(sats) <- gsub('\\.txt$', '', basename(names(sats)))
sats[['lat']] <- rev(sats[['lat']])
sats[['lon']] <- -1 * sats[['lon']]

# organize results into a data frame
sats <- do.call('cbind', sats)
sats <- data.frame(sats)
names(sats) <- gsub('^X', 'kd_', names(sats))
names(sats) <- gsub('_kd_cbay$', '', names(sats))

# clip the data frame by the spatial extent of choc segments
data(choc_seg)
coordinates(sats) <- c('lon', 'lat')
sel <- !is.na(sats %over% choc_seg)
sats <- data.frame(sats[c(sel), ])

# rearrange to put lon/lat first, do not take average
locs <- sats[, grepl('lat|lon', names(sats))]
sats <- sats[, !grepl('lat|lon', names(sats))]
sats_all <- data.frame(lon = locs$lon, lat = locs$lat, sats)

# save the uncorrected data for comparison w/ corrected
choc_sats_unc <- sats_all
save(choc_sats_unc, file = 'data/choc_sats_unc.RData')
save(choc_sats_unc, file = 'M:/docs/manuscripts/sgdepth_manu/data/choc_sats_unc.RData')

##
# develop correction for kd using in situ data

# ## the following gets in situ kd data from florida estuaries database
# # actual correction is applied below
#
# rm(list = ls())
# 
# library(dplyr)
# library(tidyr)
# library(raster)
# library(sp)
# library(ggplot2)
#
# ##
# # process raw data that was exported from florida estuaries database
#
# # L:\lab\FloridaEstuaries\yates\working\FL_Estuaries.mdb
# # kpar and meta as separate files
# # processed data saved as 'choc_situ.RData'
#
# kpar <- read.table('data/CTD_YSI.txt', sep = ',', header = T,
#   stringsAsFactors = F)
# events <- read.table('data/ch_events.txt', sep ='\t', header = T,
#   stringsAsFactors = F)
# 
# # process metadata
# # date to posix, remove exta cols, remove dups (only one), sort
# events <- select(events, Cruise, Station, Latitude, Longitude) %>% 
#   unique %>% 
#   arrange(Station, Cruise)
# 
# # process kpar
# # get choc sites and relevant columns
# # format date and kpar as positive values, sort
# kpar <- filter(kpar, grepl('^CH', Station)) %>% 
#   select(Cruise, Station, Rundate, Depth, kPAR) %>% 
#   na.omit %>% 
#   mutate(
#     Rundate = as.POSIXct(Rundate, format = '%m/%d/%Y %H:%M:%S'),
#     kPAR = -kPAR
#     ) %>% 
#   filter(kPAR > 0) %>% 
#   mutate(year = as.numeric(strftime(Rundate, '%Y'))) %>% 
#   group_by(Cruise, Station, year) %>% 
#   summarise(kPAR = mean(kPAR, na.rm = T)) %>% 
#   left_join(., events, by = c('Station', 'Cruise')) %>% 
#   filter(c(2010) %in% year) %>% # incomplete data for 2009, 2012, no kd for 2011
#   group_by(Station, year,  Latitude, Longitude) %>% 
#   summarise(kPAR = mean(kPAR, na.rm = T)) %>% 
#   filter(Station != 'CH10') %>% # out of segment limits
#   data.frame %>% 
#   arrange(kPAR) %>% 
#   mutate(cumkPAR = cumsum(kPAR)/max(cumsum(kPAR)))
# choc_situ <- kpar
# save(choc_situ, file = 'data/choc_situ.RData')

##
# sample 2010 data using choc_situ (processed kd from above) to get correction

rm(list = ls())

source('R/funcs.R')
library(dplyr)
library(raster)
library(sp)
library(ggplot2)

# load data - uncorrecte satellite data and in situ kd
data(choc_sats_unc)
data(choc_situ)

# make a raster file from 2010
sats_all <- choc_sats_unc
sats_2010 <- sats_all[, c('lon', 'lat', 'kd_2010')]
sat_rast_2010 <- make_rast_fun(sats_2010, 'kd_2010')

# sample the raster with in situ data locations
samp_dat <- choc_situ
coordinates(samp_dat) <- c('Longitude', 'Latitude')
samp_dat <- extract(sat_rast_2010, samp_dat) %>% 
  data.frame  %>% 
  mutate(
    kPAR = choc_situ$kPAR,
    cumkPAR = choc_situ$cumkPAR,
    layer = kd_2010,
    cumlayer = cumsum(layer)/max(cumsum(layer))
    )
samp_dat <- samp_dat[, !names(samp_dat) %in% 'kd_2010']

# use kd_binomod and kd_backsat to get models for correcting and to implement
# models for transformation
kd_mods <- kd_mod(samp_dat, 'kPAR', 'cumkPAR', 'layer', 'cumlayer')

# an example
# kd_backsat(kd_mods, samp_dat, xsat = 0.5, plot = T)

# correct satellite data
to_crct <- grep('^kd_', names(sats_all), value = T)
crc <- apply(
  sats_all[, to_crct], 2, 
  function(x) kd_backsat(kd_mods, samp_dat, xsat = x)
)
sats_all[, to_crct] <- crc

# replace old kd_ave with new from corrected data
# only 2003 to 2007 data (seagrass is 2007)
sats <- sats_all
locs <- sats[, grepl('lat|lon', names(sats))]
sats <- sats[, !grepl('lat|lon|2008$|2009$|2010$|2011$|2012$|2013$', names(sats))]
sats_all <- apply(sats, 1, function(x) mean(x, na.rm = TRUE))
sats_all <- data.frame(lon = locs$lon, lat = locs$lat, kd_ave = sats_all, sats)

# make a raster object from the averaged data
sats_ave <- sats_all[, c('lon', 'lat', 'kd_ave')]
sat_rast <- make_rast_fun(sats_ave, 'kd_ave')

# save the new corrected data for comparison w/ uncorrected
choc_sats_crc <- list(ave_rast = sat_rast, sats_all = sats_all)
save(choc_sats_crc, file = 'data/choc_sats_crc.RData')
save(choc_sats_crc, file = 'M:/docs/manuscripts/sgdepth_manu/data/choc_sats_crc.RData')


