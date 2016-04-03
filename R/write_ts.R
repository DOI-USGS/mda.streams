#' Write timeseries data into a compressed file
#' 
#' Writes a ts file (with format specific to mda.streams) a data.frame into a
#' compressed file suitable for posting to ScienceBase
#' 
#' @param data unitted data.frame
#' @param site the full site name, e.g., 'nwis_06893820'
#' @param var the variable name of the output, e.g., 'doobs'
#' @param src the source of the data, e.g., 'nwis' or 'nldas'
#' @param folder the folder to write the file in
#' @param version the output version for the file (tsv, rds)
#' @keywords internal
#' @importFrom lubridate tz
#' @importFrom unitted u
#' @export
write_ts <- function(data, site, var, src, folder, version=c('rds','tsv')){
  
  # quick failure/return if appropriate
  version <- match.arg(version)
  verify_var_src(var, src, on_fail=warning)
  if (!verify_ts(data, var))
    stop('timeseries input for site ',site,', var ',var,', and src ', src, ' is invalid')
  if (nrow(data) == 0)
    invisible(NULL)
  
  # for tsv store the timezone code[s] in the units field[s]; read_ts will
  # retrieve it
  if (version=='tsv'){
    # always do this for DateTime
    data$DateTime <- u(data$DateTime, tz(data$DateTime))

    # also do it if the var is a datetime
    if(names(data)[2] %in% c("sitetime", "suntime")) {
      if(tz(data[,2]) != "UTC") stop("tz of ",names(data)[2]," must be 'UTC'")
      if(get_units(data[,2]) != "") stop(names(data)[2], " units should be empty on call to write_ts")
      data[,2] <- u(data[,2], 'UTC')
    }
  }
  
  # do the file writing. gz1 seems pretty efficient; see
  # https://github.com/USGS-R/mda.streams/issues/229
  fpath <- make_ts_path(site, make_ts_name(var, src), folder, version)
  if (version == 'tsv'){
    gz_con <- gzfile(fpath, "w", compression=1)
    write_unitted(data,  file = gz_con, sep=pkg.env$ts_delim)
    close(gz_con)
  } else if (version == 'rds'){
    saveRDS(data, file = fpath, compress=pkg.env$rds_compression)
  }
  
  invisible(fpath)
}