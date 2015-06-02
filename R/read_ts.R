#'@title read ts format (timeseries) for mda.streams into data.frame
#'@param file_handle a valid ts file handle
#'@return a timeseries data.frame
#'@author Jordan S. Read
#'@examples
#'\dontrun{
#'file <- download_ts(site = "nwis_06893820", variable = "baro", overwrite_file = TRUE)
#'dissolved_oxygen <- read_ts(file)
#'}
#'@import tools 
#'@importFrom unitted read_unitted
#'@export
read_ts = function(file){
  
  if (length(file) != 1)
    stop('read_ts only supported for a single file')
  
  gz_con <- gzfile(file, open = "rb")
  df <- read_unitted(data,  file = gz_con, sep=pkg.env$ts_delim)
  close(gz_con)
  return(df)
}