#'@title read ts format (timeseries) for mda.streams into data.frame
#'@param file a valid ts file path
#' @seealso \code{\link{download_ts}}
#'@return a timeseries unitted data.frame
#'@author Jordan S. Read
#'@examples
#'\dontrun{
#'file <- download_ts(site = "nwis_06893820", variable = "baro")
#'baro_pressure <- read_ts(file)
#'}
#'@import tools 
#'@importFrom unitted read_unitted get_units
#'@export
read_ts = function(file){
  if (length(file) != 1)
    stop('read_ts only supported for a single file')
  
  df <- read_unitted(file, sep=pkg.env$ts_delim)
  
  site <- parse_ts_path(file, 'site')
  variable <- parse_ts_path(file, 'variable')
  if (!verify_ts(df, variable))
    stop('timeseries input for site',site,'and variable',variable,'is not valid')
  
  df[, 1] <- as.POSIXct(df[, 1], tz = get_units(df[,1]))
  
  # -- to do: check that output format is the same needed for input of write_ts() --
  return(df)
}

