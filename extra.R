######
# find shapefiles in directory that have SG somewhere in the file name

root <- 'L:/lab/FloridaCriteria/Seagrass_vs_Depth'
sg_dirs <- grep(
  '^[0-1][0-9]',
  dir(root),
  value = T
  )
sg_dirs <- paste0(root, '/', sg_dirs)

sg_shp <- list.files(path = sg_dirs, 
  pattern = '^.*SG.*shp$',
  recursive = T,
  full.names = T
)