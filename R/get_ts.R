#' Bring one or more timeseries into R and merge them
#' 
#' Downloads the file from SB if either (1) the file has not yet been downloaded
#' to the code{tempdir()} during this R session, or (2) 
#' \code{on_local_exists='replace'}. There's a small risk that the resulting ts
#' will be out of date relative to ScienceBase, but the benefit is faster
#' ts-getting.
#' 
#' @inheritParams download_ts
#' @inheritParams combine_ts
#' @export
get_ts <- function(var_src, site_name, method='approx', approx_tol=as.difftime(3, units="hours"), on_local_exists='skip') {
  
  if(length(site_name) > 1) stop("only one site_name is allowed")
  
  data_list <- lapply(var_src, function(vs) {
    file <- download_ts(var_src=vs, site_name=site_name, on_local_exists=on_local_exists)
    read_ts(file)
  })
  
  if(length(var_src) > 1) {
    combo <- do.call(combine_ts, c(data_list, list(method='approx', approx_tol=as.difftime(3, units="hours"))))
  } else {
    combo <- data_list[[1]]
  }
  combo
}