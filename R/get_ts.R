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
                   on_local_exists='skip', on_invalid='stop', match_var = "leftmost", condense_stat = "mean") {

  if(length(site_name) > 1) stop("only one site_name is allowed")
  if(length(match_var) > 1) stop("only one match_var is allowed")
  if(length(condense_stat) > 1) stop("only one condense_stat is allowed")
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
    
    startDates <- do.call("c", lapply(data_list, function(data) {
        all_dates <- unitted::v(data$DateTime)
        return(all_dates[1]) 
      }))
    endDates <- do.call("c", lapply(data_list, function(data) {
        all_dates <- unitted::v(data$DateTime)
        return(tail(all_dates, 1)) 
      }))
    resolutions_df <- summarize_ts(var_src, site_name, out="modal_timestep") %>% 
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

    #condense_stat
    if(resolutions[var_index] != 60 & length(resolution_warning) > 0){
      siteName <- site_name #hacky
      site_meta <- get_meta("basic") %>% v() %>% filter(site_name == siteName)
      site_lon <- site_meta$lon
      
      #matching dates: start/end dates + gaps in the middle
      match_dates <- unitted::v(data_list_ordered[[var_index]]$DateTime)
      diff_time <- diff(match_dates)
      gap_index <- which(abs(diff_time) > 1) 
      
      data_list_filtered <- lapply(data_list_ordered, 
                                   function(data, start, end, before_gap, after_gap){
                                      data_nounits <- unitted::v(data)
                                      data_filtered <- data_nounits %>% 
                                        filter(DateTime >= start) %>% 
                                        filter(DateTime <= end) %>% 
                                        filter(DateTime <= before_gap | DateTime >= after_gap)
                                    }, 
                                    start = match_dates[1], 
                                    end = tail(match_dates,1),
                                    before_gap = match_dates[gap_index], 
                                    after_gap = match_dates[gap_index + 1])
      

      data_list_condensed <- lapply(data_list_filtered[resolution_warning], condense_by_stat, 
                                    condense_stat = condense_stat, site_lon = site_lon)
    }
    
    combo <- do.call(combine_ts, c(data_list_ordered, list(method=method, approx_tol=approx_tol)))
    combo <- combo[, df_order]
    
  } else {
    combo <- data_list[[1]]
  }
  combo
}

condense_by_stat <- function(ts, condense_stat, site_lon){
  solar.time <- convert_UTC_to_solartime(ts$DateTime, site_lon, time.type = "mean solar")
  ts_solar <- ts %>% v() %>% mutate(solar.time = solar.time)
  
  ts_condensed <- streamMetabolizer:::mm_model_by_ply(
    model_fun=condense_by_ply, # should look like mm_model_by_ply_prototype
    data=unitted::v(ts_solar), #include sitetime
    data_daily=NULL,
    day_start=4,
    day_end=28,
    stat_func=condense_stat
  )
  
  return(ts_condensed)
}

condense_by_ply <- function(data_ply, data_daily_ply, ..., day_start, day_end, ply_date) {
  args <- list(...)
  
  dates_col <- which(names(data_ply) == "solar.time")
  data_col <- which(!names(data_ply) %in% c("DateTime", "solar.time"))
  
  summarize_stat <- do.call(args$stat_func, list(data_ply[, data_col]))
  
  return(data.frame(summarize_stat))
}
