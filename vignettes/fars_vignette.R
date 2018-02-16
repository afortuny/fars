## ---- echo = FALSE, include = FALSE--------------------------------------
library(fars)
library(dplyr)
library(maps)

## ---- eval=FALSE---------------------------------------------------------
#  filename <- system.file("extdata/accident_2015.csv.bz2", package = "fars")
#  fars_read(filename)

## ---- eval=FALSE---------------------------------------------------------
#  make_filename(2015)

## ---- eval=FALSE---------------------------------------------------------
#  setwd(system.file("extdata", package = "fars"))
#  fars_read_years(2013)

## ---- eval=FALSE---------------------------------------------------------
#  setwd(system.file("extdata", package = "fars"))
#  fars_summarize_years(2013:2015)

## ---- eval=FALSE---------------------------------------------------------
#  setwd(system.file("extdata", package = "fars"))
#  fars_map_state(45, 2015)

