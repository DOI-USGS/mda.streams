#' Translate a timeseries name from mda.streams to ScienceBase
#' @param variable a timeseries name[s] in mda.streams lingo (e.g., \code{wtr})
#' @return timseries name[s] in ScienceBase lingo (e.g., "\code{ts_wtr})
make_ts_name <- function(variable){
  paste0(pkg.env$ts_prefix, variable)
}

#' Translate a timeseries name from ScienceBase to mda.streams
#' @param ts_name timeseries name[s] in ScienceBase lingo (e.g., "\code{ts_wtr})
#' @param use_names logical. Should the return vector be named according to the input values?
#' @return timeseries name[s]in mda.streams lingo (e.g., \code{wtr})
parse_ts_name <- function(ts_name, use_names=TRUE) {
  vapply(strsplit(ts_name, '_'), '[', vector('character', length = 1), 2, USE.NAMES=use_names)
}

#' Translate a site name from database and site number to a 
#' ScienceBase+mda.streams site ID
#' @param database a character or character vector of databases, probably in 
#'   \code{c("nwis","nldas")}.
#' @param sitenum integer or character, coercible to character, representing the
#'   site code as used by the database.
#' @return site ID in ScienceBase and mda.streams lingo
make_site_name <- function(database=c("nwis","nldas"), sitenum) {
  paste0(database, as.character(sitenum), sep='_')
}

#' Split site name into siteID (used for NWIS site numbers)
#' @param site a valid powstreams site (e.g., "nwis_0338102")
#' @param out character, length 1 or 2 selected from 
#'   \code{c("database","sitenum")} indicating whether you want to get back the 
#'   database, the site number, or both.
#' @param use_names logical. Should the return vector be named according to the
#'   input values?
#' @return the database, sitenum, or both. If both, the return value is a 
#'   data.frame; otherwise it's a vector.
#' @import dplyr
parse_site_name <- function(site, out="sitenum", use_names=TRUE){
  parsed <- sapply(strsplit(site, '_'), '[', match(out, c("database","sitenum")))
  if(!is.null(dim(parsed))) {
    parsed <- parsed %>% t %>% as.data.frame %>% setNames(c("database","sitenum"))
  }
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- site; .}) 
      } else {
        parsed %>% setNames(site)
      }
  }
  parsed
}