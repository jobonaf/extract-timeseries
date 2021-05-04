list.of.packages <- c("shiny","leaflet","leaflet.extras","glue","xts","dygraphs","zen4R",
                      "raster","lubridate","dplyr","rgdal","futile.logger")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)