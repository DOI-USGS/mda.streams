#' @title write timeseries format compressed file
#' @param data unitted data.frame
#' @param site the full site name, e.g., 'nwis_06893820'
#' @param var the variable name of the output, e.g., 'doobs'
#' @param src the source of the data, e.g., 'nwis' or 'nldas'
#' @param folder the folder to write the file in
#' @keywords internal
#' @export
write_ts <- function(data, site, var, src, folder){
  
  var_src <- make_var_src(var, src)
  if (!verify_ts(data, var_src))
    stop('timeseries input for site',site,'and variable',var_src,'is not valid')
  
  if (nrow(data) == 0)
    invisible(NULL)
  
  fpath <- make_ts_path(site, make_ts_name(var_src), folder)
  gz_con <- gzfile(fpath, "w")
  write_unitted(data,  file = gz_con, sep=pkg.env$ts_delim)
  close(gz_con)
  invisible(fpath)
}