#' Bring one or more timeseries into R and merge them
#' 
#' The timeseries are iteratively joined, starting by joining the second element
#' of \code{var_src} to the first, then adding in the third, etc. This method 
#' means you have control, not just through the \code{method} and
#' \code{approx_tol} arguments but also through how you order the elements of
#' \code{var_src}, with consequences for the size and contents of the resulting
#' data.frame.
#' 
#' Downloads each file from SB if either (1) the file has not yet been downloaded
#' to the code{tempdir()} during this R session, or (2) 
#' \code{on_local_exists='replace'}. There's a small risk that the resulting ts 
#' will be out of date relative to ScienceBase, but the benefit is faster 
#' ts-getting.
#' 
#' \code{get_ts} returns a data.frame, where the first column is the \code{DateTime} 
#' and subsequent columns are the timeseries defined in \code{var_src}. The names of
#' the variable columns are equal to the string in each \code{var_src} variable 
#' before the underscore. E.g. if \code{var_src} had a variable \code{"disch_nwis"}, 
#' then the corresponding column name would be \code{disch}. 
#' 
#' @inheritParams download_ts
#' @inheritParams combine_ts
#' @inheritParams read_ts
#' @export
get_ts <- function(var_src, site_name, method='approx', approx_tol=as.difftime(3, units="hours"), 
                   on_local_exists='skip', on_invalid='stop', expand_on = "leftmost") {

  if(length(site_name) > 1) stop("only one site_name is allowed")
  
  data_list <- lapply(var_src, function(vs) {
    file <- download_ts(var_src=vs, site_name=site_name, on_local_exists=on_local_exists)
    read_ts(file, on_invalid=on_invalid)
  })
  
  if(length(var_src) > 1) {
    
    df_order <- c("DateTime", gsub("\\_.*","",var_src))
    
    if(expand_on == "leftmost"){
      var_index <- 1
      data_list_ordered <- data_list
    } else {
      var_index <- which(var_src == expand_on)
      not_var_index <- which(var_src != expand_on)
      data_list_ordered <- data_list[c(var_index, not_var_index)]
    }
    
    num_obs <- unlist(lapply(data_list, nrow))
    longer_than_var <- which(num_obs > num_obs[var_index])
    if(any(longer_than_var)){
      resolution_msg <- paste0("Results reflect the resolution of the ",
                               var_src[var_index], 
                               " timeseries. Some variables (", 
                               paste(var_src[longer_than_var], collapse=", "),
                               ") will lose resolution. Consider setting expand_on to set the preferred resolution or condense_stat to set the statistic used when resolution is lost.")
      warning(resolution_msg)
    } 
      
    combo <- do.call(combine_ts, c(data_list_ordered, list(method=method, approx_tol=approx_tol)))
    combo <- combo[, df_order]
    
  } else {
    combo <- data_list[[1]]
  }
  combo
}