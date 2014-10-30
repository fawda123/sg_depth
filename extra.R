# from browser in server

library(gstat)
library(maptools)
library(automap)

load('dat/maxd.RData')
maxd <- na.omit(maxd)
coordinates(maxd) = ~ Var1 + Var2

newdat <- apply(bbox(maxd), 1, function(x) seq(x[1], x[2], by = 0.002))
newdat <- expand.grid(newdat)
gridded(newdat) = ~ Var1 + Var2

res <- autoKrige(zmax_all ~ 1, maxd, newdat)
res <- res['krige_output'][[1]]

