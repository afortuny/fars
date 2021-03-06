---
title: "Fars"
author: "Alan Fortuny"
date: "February 16, 2018"
output: html_document
vignette: >
  %\VignetteEngine{knitr::knitr}
  %\VignetteIndexEntry{Title of your vignette}
  %\usepackage[UTF-8]{inputenc}
---



The fars package contains a set of functions for analysis of data from FARS - Fatality Analysis Reporting System. FARS is a nationwide census providing NHTSA, Congress and the American public yearly data regarding fatal injuries suffered in motor vehicle traffic accidents. [[1](http://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS))]

## Package functions

There are five exported functions available:

- `fars_read()`
- `make_filename()`
- `fars_read_years()`
- `fars_summarize_years()`
- `fars_map_state()`

Now we give a short description with examples how to use the functions. For the purposes of these examples we will use data from FARS, years 2013-2015, that can be found in the package directory under `\extdata` folder.

## Read FARS data files

The function `fars_read()` provides a way how to read data from a file path provided as an argument. It returns a `tibble` with loaded data:


```r
filename <- system.file("extdata/accident_2015.csv.bz2", package = "fars")
fars_read(filename)
```


## Get the file name per year

The function `make_filename()` create the string filename based on the year from which we want to extract the information 


```r
make_filename(2015)
```

## Months and years of each observarion

The function `fars_read_years()` read and store the month and year column for each of the years provided as argument.


```r
setwd(system.file("extdata", package = "fars"))
fars_read_years(2013)
```



## Summarize number of accidents

The next function `fars_summarize_years()` takes a vector or list of years (numeric/integer values) as an argument and if the corresponding files with data are available, it returns a pivot table with number of accidents per year and month. The data files need to be located in the working directory.


```r
setwd(system.file("extdata", package = "fars"))
fars_summarize_years(2013:2015)
```

## Plot accident locations

The last function available in the package, `fars_map_state()` takes a state index and year as arguments and returns a plot of the state with accident locations. The state indices need to correspond available indeces from the data set. Here are some examples:


```r
setwd(system.file("extdata", package = "fars"))
fars_map_state(45, 2015)
```

