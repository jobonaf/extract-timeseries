list.of.packages <- c("shiny","leaflet","leaflet.extras","glue","xts","dygraphs",
                      "raster","lubridate","dplyr","rgdal","futile.logger","zen4R")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)