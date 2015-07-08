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
  
  if (!verify_ts(data, var, checks=c('ncol','names')))
    stop('timeseries input for site ',site,', var ',var,', and src ', src, ' is not valid')
  
  # store the timezone code[s] in the units field[s]; read_ts will pull it back out
  if(!("" == get_units(data$DateTime))) stop("timeseries DateTime units should be empty on ")
  data$DateTime <- u(data$DateTime, tz(data$DateTime))
  
  if (!verify_ts(data, var, checks=c('tz','units')))
    stop('timeseries input for site ',site,', var ',var,', and src ', src, ' is not valid')
  
  # store the timezone code in the units field for suntime, too, now that we've checked
  if(names(data)[2] %in% c("sitetime", "suntime")) {
    if(tz(data[,2]) != "UTC") stop("tz of ",names(data)[2]," must be 'UTC'")
    if(get_units(data[,2]) != "") stop(names(data)[2], " units should be empty on call to write_ts")
    data[,2] <- u(data[,2], 'UTC')
  }
  
  if (nrow(data) == 0)
    invisible(NULL)
  
  fpath <- make_ts_path(site, make_ts_name(var, src), folder)
  gz_con <- gzfile(fpath, "w")
  write_unitted(data,  file = gz_con, sep=pkg.env$ts_delim)
  close(gz_con)
  invisible(fpath)
}