# processing tb pts 
library(foreign)
library(tidyverse)
library(lubridate)
library(maptools)

data(tb_seg)

yrs <- c(1988, 1990, 1992, 1994, 1996, 1999, 2001, 2004, 2006, 2008, 2010)

# EPC routine monitoring stations
stats <- read.dbf('M:/GIS/tb_sta.dbf') %>% 
  select(sta, Actual_Lat, Actual_Lon) %>% 
  rename(
    StationID = sta, 
    lat = Actual_Lat, 
    long = Actual_Lon
  ) %>% 
  mutate(
    StationID = as.character(StationID)
  )
    
##
# process secchi data
# secc_tb_raw was output from http://www.tampabay.wateratlas.usf.edu/datadownload
# searched for all secchi data in tampa bay wbids
data(secc_tb_raw)

# get secchi data with good QA codes
# select the stations that are routine EPC locations
# select stations in same years as seagrass data and at least 10 samples per year
# average by location and year if fit criteria
secc <- secc_tb_raw %>% 
  filter(QACode == '') %>%
  select(StationID, SampleDate, Result_Value) %>% 
  mutate(
    secchi_m = Result_Value * 0.3048, # verified that all were in ft
    date = as.character(SampleDate),
    date = as.Date(date, format = '%m/%d/%Y %H:%M', tz = 'America/Jamaica'), 
    yr = year(date),
    mo = month(date),
    StationID = as.character(StationID)
  ) %>% 
  filter(StationID %in% stats$StationID) %>% 
  group_by(StationID, yr) %>% 
  filter(length(unique(mo)) > 9) %>% # select min of 10 months at each loc, year
  unique %>% # sometimes replicates in the same day
  summarise(
    secchi_m = mean(secchi_m)
  ) %>% 
  ungroup %>% 
  filter(
    yr %in% yrs
  ) %>% 
  arrange(yr, StationID) %>% 
  left_join(., stats, by = 'StationID')

# split by year, make spatial points data frame list, clip by tb boundaries
secc_all_tb <- split(secc, secc$yr) %>% 
  lapply(., function(x){
    
    x <- select(x, -StationID) %>% 
      rename(
        SD = secchi_m, 
        Longitude = long, 
        Latitude = lat
      )
    
    coords <- x[, c('Longitude', 'Latitude')]
    x <- SpatialPointsDataFrame(coords, x)

    sel <- !is.na(x %over% tb_seg)[, 1]
    x <- x[sel, ]
    
    return(x)
    
  })

save(secc_all_tb, file = 'data/secc_all_tb.RData', compress = 'xz')
save(secc_all_tb, file = 'M:/docs/manuscripts/sgdepth_manu/data/secc_all_tb.RData', compress = 'xz')

##
# seagrass points

rm(list = ls())

roots <- 'L:/lab/FloridaCriteria/Seagrass_vs_Depth/09-Tampa_Bay'
yrs <- c(1988, 1990, 1992, 1994, 1996, 1999, 2001, 2004, 2006, 2008, 2010)

# sg depth point files to import
fls <- list(
  'Tampa_1988_Segments.dbf', 
  'Tampa_1990_Segments.dbf', 
  'Tampa_1992_Segments.dbf', 
  'Tampa_1994_Segments.dbf',
  'Tampa_1996_Segments.dbf',
  'Tampa_1999_Segments.dbf',
  'Tampa_2001_Segments.dbf',
  'Tampa_2004_Segments.dbf',
  'Tampa_2006_SG_Segments.dbf',
  'Tampa_2008_Segments.dbf',
  'Tampa_2010_Segments.dbf'
)

# import all dbf files
# select relevant columns
out_ls <- vector('list', length = length(yrs))
names(out_ls) <- paste0('sgpts_', yrs, '_tb')
for(i in seq_along(yrs)){
  
  cat(i, '\t')

  yr <- yrs[i]
  fl <- fls[i]
  tmp <- paste(roots, yr, fl, sep = '/') %>% 
    read.dbf %>% 
    mutate(yr = yr)
  names(tmp) <- tolower(names(tmp))
  tmp <- tmp[, names(tmp) %in% c('yr', 'lat', 'lon', 'depth', 'descript', 'seagrass', 'fluccscode')]
  
  if(sum(c('fluccscode', 'descript') %in% names(tmp)) == 2)
    tmp <- select(tmp, -fluccscode)
  
  names(tmp)[names(tmp) %in% c('seagrass', 'fluccscode', 'descript')] <- 'seagrass'
  
  out_ls[[i]] <- tmp
  
}

# combine data
sgpts_all_tb <- bind_rows(out_ls) %>% 
  mutate(
    seagrass = gsub('^DSG$|^Patchy Seagrass$', 'Discontinuous', seagrass),
    seagrass = gsub('^CSG$|^Continuous Seagrass$', 'Continuous', seagrass), 
    seagrass = ifelse(seagrass %in% c('Continuous', 'Discontinuous'), seagrass, NA),
    seagrass = factor(seagrass), 
    depth = pmax(0, -1 * depth)
  ) %>% 
  rename(
    Depth = depth, 
    Seagrass = seagrass,
    coords.x1 = lon, 
    coords.x2 = lat
  ) %>% 
  filter(Depth < 30) %>% 
  select(coords.x1, coords.x2, Depth, Seagrass, yr)

# save tp data file, in format to be used by vdatum
write.table(data.frame(sgpts_all_tb), 'data/sgpts_all_tb/sgpts_all_tb.txt', sep = ',', quote = F, row.names = F)

# run vdatum tool on exported text file
# vdatum in M:/GIS/vdatum
# can run .bat file from command line, see test.bat in M:/GIS/vdatum
# or gui with vdatum.jar
# for TB, vertical datum is in navd88, must convert to lmsl (local mean sea level), m to m
# vdatum command line info here: https://vdatum.noaa.gov/docs/userguide_cmd.html#fileconv
# also see M:/docs/manuscripts/sgdepth_manu/R/vdatum.R for some more info

dat <- read.table('data/sgpts_all_tb/results/sgpts_all_tb.txt', sep = ',', header = T)
sgpts_all_tb <- filter(dat, Depth > -999999) %>% 
  mutate(Depth = pmax(0, Depth)) %>% 
  split(., .$yr) %>% 
  lapply(., function(x){
    
    coords <- x[, c('coords.x1', 'coords.x2')]
    x <- x[, c('Depth', 'Seagrass')]
    x <- SpatialPointsDataFrame(coords, x)

    return(x)
    
  })

save(sgpts_all_tb, file = 'data/sgpts_all_tb.RData', compress = 'xz')
save(sgpts_all_tb, file = 'M:/docs/manuscripts/sgdepth_manu/data/sgpts_all_tb.RData', compress = 'xz')









