#'@title read ts format (timeseries) for mda.streams into data.frame
#'@param file a valid ts file path
#' @seealso \code{\link{download_ts}}
#'@return a timeseries data.frame
#'@author Jordan S. Read
#'@examples
#'\dontrun{
#'file <- download_ts(site = "nwis_06893820", variable = "baro")
#'baro_pressure <- read_ts(file)
#'}
#'@import tools 
#'@importFrom unitted read_unitted
#'@export
read_ts = function(file){
  if (length(file) != 1)
    stop('read_ts only supported for a single file')
  
  df <- read_unitted(file, sep=pkg.env$ts_delim)
  
  return(df)
}