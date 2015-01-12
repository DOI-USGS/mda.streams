#'@title read ts format (timeseries) for mda.streams into data.frame
#'@param file_handle a valid ts file handle
#'@return a timeseries data.frame
#'@author Jordan S. Read
#'@examples
#'\dontrun{
#'file_handle <- download_ts(site = 'nwis_01018035', variable = 'doobs')
#'dissolved_oxygen <- read_ts(file_handle)
#'}
#'@import tools
#'@export
read_ts = function(file_handle){
  
  
  ts_extension <- get_ts_extension()
  file_extension <- file_ext(file_handle)
  if (file_extension != ts_extension){
    stop(file_handle, ' cannot be read by this function')
  }
  
  ts_delim <- get_ts_delim()
  df <- read.table(file_handle, header = TRUE, sep = ts_delim)
  return(df)
}