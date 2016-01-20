#' Read timeseries data into a data.frame
#' 
#' Reads a ts file (with format specific to mda.streams) into a data.frame
#' 
#' @param file a valid ts file path
#' @param on_invalid character in \code{c("stop","warn")} indicating how to
#'   handle invalid timeseries
#' @seealso \code{\link{download_ts}}
#' @return a timeseries unitted data.frame
#' @author Jordan S. Read, Alison Appling
#' @examples
#' \dontrun{
#' file <- download_ts(var_src = "baro_nldas", site_name = "nwis_06893820")
#' baro_pressure <- read_ts(file)
#' }
#' @import tools
#' @importFrom unitted read_unitted get_units
#' @export
read_ts = function(file, on_invalid=c("stop","warn")) {
  on_invalid <- match.arg(on_invalid)
  if (length(file) != 1)
    stop('read_ts only supported for a single file')
  
  df <- read_unitted(file, sep=pkg.env$ts_delim)
  
  # convert units to tz field for suntime before verify_ts
  if(names(df)[2] %in% c("sitetime", "suntime")) {
    df[,2] <- u(as.POSIXct(df[,2], tz=get_units(df[,2])), NA)
  } else if(names(df)[2] %in% c("sitedate")) {
    df[,2] <- u(as.Date(df[,2]), NA)
  }
  
  # check the data for mda.streams validity
  if (!verify_ts(df, parse_ts_path(file, 'var'))) {
    msg <- paste0('timeseries in file ', file, ' is not valid')
    if(on_invalid=="stop") stop(msg) else warning(msg)
  }
  
  # convert units to tz field for DateTime
  df$DateTime <- u(as.POSIXct(df$DateTime, tz=get_units(df$DateTime)), NA)
  
  return(df)
}

