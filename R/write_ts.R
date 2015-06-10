#' @title write timeseries format compressed file
#' @param data unitted data.frame
#' @param site the full site name, e.g., 'nwis_06893820'
#' @param var the variable name of the output, e.g., 'doobs'
#' @param src the source of the data, e.g., 'nwis' or 'nldas'
#' @param folder the folder to write the file in
#' @keywords internal
#' @importFrom lubridate tz
#' @importFrom unitted u
#' @export
write_ts <- function(data, site, var, src, folder){
  
  # store the timezone code in the units field; read_ts will pull it back out
  if(!("" == get_units())) stop("timeseries DateTime units should be empty on ")
  data$DateTime <- u(data$DateTime, tz(data$DateTime))
  
  if (!verify_ts(data, var))
    stop('timeseries input for site ',site,', var ',var,', and src ', src, ' is not valid')
  
  if (nrow(data) == 0)
    invisible(NULL)
  
  fpath <- make_ts_path(site, make_ts_name(var, src), folder)
  gz_con <- gzfile(fpath, "w")
  write_unitted(data,  file = gz_con, sep=pkg.env$ts_delim)
  close(gz_con)
  invisible(fpath)
}