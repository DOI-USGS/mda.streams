mda.streams
===========

Backend tools for powstreams: data and model management for the Powell Center working group on stream metabolism

[![Windows Build Status](https://travis-ci.org/USGS-R/mda.streams.svg?branch=master)](https://travis-ci.org/USGS-R/mda.streams)
[![Linux Build Status](https://ci.appveyor.com/api/projects/status/d87dfg4fu7wwiuo7?svg=true)](https://ci.appveyor.com/project/jread-usgs/mda-streams)
[![Test Coverage Status](https://coveralls.io/repos/USGS-R/mda.streams/badge.svg?branch=master&service=github)](https://coveralls.io/github/USGS-R/mda.streams?branch=master)
[![Issues Ready to Address](https://badge.waffle.io/USGS-R/mda.streams.png?label=ready&title=Ready)](https://waffle.io/USGS-R/mda.streams)

### First-time installation:
```r
install.packages("mda.streams", dependencies = TRUE, 
  repos = c("http://owi.usgs.gov/R","https://cran.rstudio.com"))
```
### Updates (do this often after installation):
```r
update.packages(oldPkgs=c(
  "mda.streams","dataRetrieval","geoknife","sbtools","smwrQW","streamMetabolizer","unitted",
  "dplyr","foreign","stringr","lubridate","jsonlite","httr","lazyeval"),
  dependencies = TRUE, repos=c("http://owi.usgs.gov/R", "https://cran.rstudio.com"))
```

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."
