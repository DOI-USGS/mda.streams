#' @title read ts format (timeseries) for mda.streams into data.frame
#' @param file a valid ts file path
#' @seealso \code{\link{download_ts}}
#' @return a timeseries unitted data.frame
#' @author Jordan S. Read
#' @examples
#' \dontrun{
#' file <- download_ts(var_src = "baro_nldas", site_name = "nwis_06893820")
#' baro_pressure <- read_ts(file)
#' }
#' @import tools
#' @importFrom unitted read_unitted get_units
#' @export
read_ts = function(file){
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
  if (!verify_ts(df, parse_ts_path(file, 'var')))
    stop('timeseries in file ', file, ' is not valid')
  
  # convert units to tz field for DateTime
  df$DateTime <- u(as.POSIXct(df$DateTime, tz=get_units(df$DateTime)), NA)
  
  return(df)
}

