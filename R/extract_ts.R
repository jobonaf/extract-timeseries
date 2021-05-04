extr_ts <- function(filein = "data/PM10_20200101-20201231.nc",
                    xx = 345136,
                    yy = 5095992) {
  library(raster)
  bb <- brick(filein)
  dd <- getValues(bb)
  cc <- coordinates(bb)
  dx <- abs(cc[1,1]-cc[2,1])
  ii <- which.min((xx-cc[,1])^2+(yy-cc[,2])^2)
  Dist <- sqrt((xx-cc[ii,1])^2+(yy-cc[ii,2])^2)
  if(Dist>dx/2*sqrt(2)) stop("Point outside domain")
  ts <- unname(dd[ii,])
  library(lubridate)
  tt <- as.Date(unname(ymd_hm(names(bb@z))+days(bb@z[[1]])))
  out <- data.frame(Day=unname(tt), Value=unname(ts))
  library(dplyr)
  out <- left_join(data.frame(Day=seq.Date(tt[1],tt[length(tt)],by = "1 day"), 
                              x.UTM33=cc[ii,1], y.UTM33=cc[ii,2]), out)
  return(out)
}

convert_coords <- function(x,y,crs.in="+init=epsg:4326",crs.out="+init=epsg:32633") {
  library(rgdal)
  d.in <- data.frame(lon=x, lat=y)
  coordinates(d.in) <- c("lon", "lat")
  proj4string(d.in) <- CRS(crs.in) # WGS 84
  d.out <- spTransform(d.in, CRS(crs.out))
  P <- coordinates(d.out)
  list(x=P[1,1], y=P[1,2])
}
