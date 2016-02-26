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
#' @return 
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
                   on_local_exists='skip', on_invalid='stop', match_var = "leftmost") {

  if(length(site_name) > 1) stop("only one site_name is allowed")
  if(length(match_var) > 1) stop("only one match_var is allowed")
  if(match_var != "leftmost" & !match_var %in% var_src) stop("match_var must be come from var_src")
  
  data_list <- lapply(var_src, function(vs) {
    file <- download_ts(var_src=vs, site_name=site_name, on_local_exists=on_local_exists)
    read_ts(file, on_invalid=on_invalid)
  })
  
  if(length(var_src) > 1) {
    
    df_order <- c("DateTime", gsub("\\_.*","",var_src))
    
    if(match_var == "leftmost"){
      var_index <- 1
      not_var_index <- 2:length(var_src)
      data_list_ordered <- data_list
    } else {
      var_index <- which(var_src == match_var)
      not_var_index <- which(var_src != match_var)
      data_list_ordered <- data_list[c(var_index, not_var_index)]
    }
    
    get_feature  <- function(data, feature_nm){
      all_dates <- unitted::v(data$DateTime)
      match_feature <- switch(feature_nm,
                              startDate = all_dates[1], 
                              endDate = tail(all_dates, 1))
    }
    
    startDates <- do.call("c", lapply(data_list, get_feature, feature_nm = "startDate"))
    endDates <- do.call("c", lapply(data_list, get_feature, feature_nm = "endDate"))
    resolutions_df <- summarize_ts(var_src[c(var_index, not_var_index)], site_name, out="modal_timestep") %>% 
      unitted::v() 
    resolutions <- resolutions_df$modal_timestep
    
    startDate_warning <- which(startDates < startDates[var_index])
    endDate_warning <- which(endDates > endDates[var_index])
    resolution_warning <- which(resolutions < resolutions[var_index])
    
    warn_msg <- function(var_src, var_index, warning_index, reflect_val, lost_val){
      if(any(warning_index)){
        warning_msg <- paste0("Results reflect the ", reflect_val ," of the ",
                              var_src[var_index], 
                              " timeseries. Some variables (",
                              paste(var_src[warning_index], collapse=", "),
                              ") will lose ", lost_val, ". Consider selecting match_var to set the preferred ",
                              reflect_val, " or condense_stat to set the statistic used when ", 
                              lost_val, " is lost.")
        warning(warning_msg)
      } 
    }
    
    startDate_warning_msg <- warn_msg(var_src, var_index, startDate_warning, "start date and time", "data")
    endDate_warning_msg <- warn_msg(var_src, var_index, endDate_warning, "end date and time", "data")
    resolution_warning_msg <- warn_msg(var_src, var_index, resolution_warning, "resolution", "resolution")
    
    combo <- do.call(combine_ts, c(data_list_ordered, list(method=method, approx_tol=approx_tol)))
    combo <- combo[, df_order]
    
  } else {
    combo <- data_list[[1]]
  }
  combo
}