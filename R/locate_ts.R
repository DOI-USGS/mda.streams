#' Identify a time series from a variable name, data type, and specific data
#' source (i.e., as specified in a metab model config file)
#' 
#' @param var character. The variable name, e.g. "doobs", "disch", or "wtr"
#' @param type character. The source type (e.g., "local", "proxy", "model")
#' @param src character. The specific source (e.g., "nwis_01474000", "bird")
#' @return URL pointer to a ScienceBase item
#' 
#' locate_ts("nwis_13206400", )
locate_ts <- function(site, var, type, src) {
  # Map inputs to SB query terms
  sbkey <- switch(
    type,
    local=site,
    model=site, # for local, site and src should be identical. just pick one.
    proxy=src)
  sbtype <- switch(
    type,
    local=paste0(pkg.env$ts_prefix, var),
    proxy=paste0(pkg.env$ts_prefix, var),
    model=paste0(pkg.env$ts_prefix, var, ".", src))
  
  query_item_identifier(scheme=get_scheme(), type=sbtype, key=sbkey)
}