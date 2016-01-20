#' Search the sites
#' 
#' @param everything text to search for within \code{paste(site_name,
#'   long_name)}
#' @param site_name text to search for within the [short] site_name
#' @param long_name text to search for within the long_name
#' @param database text to search for within the database portion of the 
#'   site_name
#' @param sitenum text to search for within the sitenum portion of the 
#'   site_name
#' @param site_names vector of site_names to search (with long_name added). The 
#'   default is to pull this list from ScienceBase
#' @inheritParams search_dataframe
#' @import dplyr
#' @importFrom unitted v
#' @export
#' 
#' @examples 
#' search_sites('indy')
#' search_sites('connecticut river')
#' search_sites('NE$|Nebr.$', match_case=TRUE, fixed=FALSE)
search_sites <- function(everything=NA, site_name=NA, long_name=NA, database=NA, sitenum=NA, 
                         site_names=list_sites(), match_case=FALSE, fixed=TRUE) {
  . <- lat <- lon <- alt <- '.dplyr.var'
  constraints <- 
    c('everything','site_name','long_name','database','sitenum') %>% setNames(.,.) %>%
    sapply(function(x) eval(as.symbol(x)))
  
  site_info <- v(get_meta('basic'))
  parsed <- parse_site_name(site_names, out=c("database","sitenum")) %>% add_rownames('site_name') %>%
    left_join(select(site_info, site_name, long_name, lat, lon, alt), by='site_name') %>%
    mutate(everything=paste(site_name, long_name))
  
  matches <- search_dataframe(parsed, constraints, match_case=match_case, fixed=fixed)
  
  parsed[matches, ] %>% select(site_name, long_name, lat, lon, alt)
}

