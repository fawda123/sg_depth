# processing tb pts 
library(foreign)
library(tidyverse)
library(lubridate)

data(tb_seg)

yrs <- c(1988, 1990, 1992, 1994, 1996, 1999, 2001, 2004, 2006, 2008, 2010)

##
# process secchi data

data(secc_tb_raw)

# get secchi data with good QA codes
# average multiple obs at same location within a year
# select stations in same years as seagrass data and at least 6 samples per year
secc <- secc_tb_raw %>% 
  filter(QACode == '') %>% 
  select(Actual_Latitude, Actual_Longitude, SampleDate, Result_Value) %>% 
  mutate(
    secchi_m = Result_Value * 0.3048, # verified that all were in ft
    date = as.character(SampleDate),
    date = as.Date(date, format = '%m/%d/%Y %H:%M', tz = 'America/Jamaica'), 
    yr = year(date),
    mo = month(date),
    lat = Actual_Latitude, 
    long = Actual_Longitude
  ) %>% 
  select(lat, long, secchi_m, date, yr) %>% 
  group_by(lat, long, yr) %>% 
  summarise(
    n = length(secchi_m), 
    secchi_m = mean(secchi_m)
  ) %>% 
  ungroup %>% 
  filter(
    yr %in% yrs &
    n > 6
    )

# conver to spatial points data frame, clip by tb segs
coordinates(secc) <- secc[, c('long', 'lat')]
sel <- !is.na(secc %over% tb_seg)[, 1]
secc <- secc[sel, ]

# toplo <- data.frame(secc)
# ggplot(toplo, aes(x = long, y = lat)) + 
#   geom_text(aes(label = n)) +
#   facet_wrap(~yr) + 
#   coord_equal()

##
# seagrass points

rm(list = ls())

roots <- 'L:/lab/FloridaCriteria/Seagrass_vs_Depth/09-Tampa_Bay'
yrs <- c(1988, 1990, 1992, 1994, 1996, 1999, 2001, 2004, 2006, 2008, 2010)

# these are not right.....
fls <- list(
  'Tampa_1988_SG_Pts_SpatialJoin.dbf', 
  'Tampa_1990_1000m.dbf', 
  'Tampa_1992_1000m.dbf', 
  'Tampa_1994_1000m.dbf',
  'Tampa_1996_1000m.dbf',
  'Tampa_1999_1000m.dbf',
  'Tampa_2001_1000m.dbf',
  'Tampa_2004_1000m.dbf',
  'Tampa_2006_Pts_1000M.dbf',
  'Tampa_2008_1000m.dbf',
  'Tampa_2010_1000m.dbf'
)

out_ls <- vector('list', length = length(yrs))
names(out_ls) <- paste0('sgpts_', yrs, '_tb')
for(i in seq_along(yrs)){
  
  cat(i, '\t')

  yr <- yrs[i]
  fl <- fls[i]
  tmp <- paste(roots, yr, fl, sep = '/') %>% 
    read.dbf
  
  out_ls[[i]] <- tmp
  
}

