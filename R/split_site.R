#'@title splot site name into siteID (used for NWIS site numbers)
#'@param site a valid powstreams site (see \code{\link{get_sites}})
#'@return a site identifier. Splits on "_"
#'@import stringr
#'@export
split_site <- function(site){
  
  grab_end <- function(str){
    tail(str, 1)
  }
  site <- vapply(strsplit(x = site, split = '_'), grab_end, vector('character', length = 1), USE.NAMES = FALSE)
  
  return(site)
}