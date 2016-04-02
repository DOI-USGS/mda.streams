#' Bring one or more timeseries into R and merge them
#' 
#' The timeseries are iteratively joined, starting by joining the second element
#' of \code{var_src} to the first, then adding in the third, etc. This method 
#' means you have control, not just through the \code{method} and 
#' \code{approx_tol} arguments but also through how you order the elements of 
#' \code{var_src}, with consequences for the size and contents of the resulting 
#' data.frame.
#' 
#' Downloads each file from SB if either (1) the file has not yet been 
#' downloaded to the code{tempdir()} during this R session, or (2) 
#' \code{on_local_exists='replace'}. There's a small risk that the resulting ts 
#' will be out of date relative to ScienceBase, but the benefit is faster 
#' ts-getting.
#' 
#' @return \code{get_ts} returns a data.frame, where the first column is the 
#'   \code{DateTime} and subsequent columns are the timeseries defined in 
#'   \code{var_src}. The names of the variable columns are equal to the string 
#'   in each \code{var_src} variable before the underscore. E.g. if 
#'   \code{var_src} had a variable \code{"disch_nwis"}, then the corresponding 
#'   column name would be \code{disch}.
#'   
#' @param method character specifying the method to use to combine timeseries 
#'   datasets
#' @param approx_tol difftime. Ignored if method != 'approx'. If method == 
#'   'approx', the maximum time interval over which an approximation will be 
#'   used to fill in data gaps (relative to the variable identified in 
#'   \code{match_var})
#' @param match_var character string indicating which variable's timesteps the 
#'   resulting data.frame should match. The string must also be in `var_src`. 
#'   The default chooses the first variable listed in `var_src`.
#' @param condense_stat function name used to condense observations to 
#'   `match_var`'s timestep (only for variables with more frequent observations 
#'   than `match_var`), or the term `match` to indicate that the function 
#'   defined in `method` will be used to match the timestep of `match_var`. 
#'   Function names should be unquoted, where as `match` should be string. 
#'   Examples of what to use: mean (default), median, max, and min. A custom 
#'   function can also be used, but it's input must be a numeric vector and 
#'   output must be a single numeric value.
#' @param day_start start time (inclusive) of a day's data in number of hours 
#'   from the midnight that begins the date. For example, day_start=-1.5 
#'   indicates that data describing 2006-06-26 begin at 2006-06-25 22:30, or at 
#'   the first observation time that occurs after that time if day_start doesn't
#'   fall exactly on an observation time. For metabolism models working with 
#'   single days of input data, it is conventional/useful to begin the day the 
#'   evening before, e.g., -1.5, and to end just before the next sunrise, e.g., 
#'   30. For multiple consecutive days, it may make the most sense to start just
#'   before sunrise (e.g., 4) and to end 24 hours later. For nighttime 
#'   regression, the date assigned to a chunk of data should be the date whose 
#'   evening contains the data. The default is therefore 12 to 36 for 
#'   metab_night, of which the times of darkness will be used.
#' @param day_end end time (exclusive) of a day's data in number of hours from 
#'   the midnight that begins the date. For example, day_end=30 indicates that 
#'   data describing 2006-06-26 end at the last observation time that occurs 
#'   before 2006-06-27 06:00. See day_start for recommended start and end times.
#' @param quietly logical. if one or more timeseries will be truncated, padded 
#'   with NAs, or condensed, should a warning message be given?
#'   
#' @inheritParams download_ts
#' @inheritParams read_ts
#' @export
get_ts <- function(var_src, site_name, method=c('approx', 'full_join', 'left_join', 'inner_join'), 
                   approx_tol=as.difftime(3, units="hours"), 
                   on_local_exists='skip', on_invalid='warn', match_var = "leftmost", 
                   condense_stat = mean, day_start = 4, day_end = 28, quietly=FALSE) {

  method <- match.arg(method)
  if(length(site_name) > 1) stop("only one site_name is allowed")
  if(length(match_var) > 1) stop("only one match_var is allowed")
  if(length(condense_stat) > 1) stop("only one condense_stat is allowed")
  if(match_var == "leftmost") match_var <- var_src[1]
  if(!match_var %in% var_src) stop("match_var must come from var_src")
  
  data_list <- lapply(var_src, function(vs) {
    file <- download_ts(var_src=vs, site_name=site_name, on_local_exists=on_local_exists)
    read_ts(file, on_invalid=on_invalid)
  })
  
  if(length(var_src) > 1) {
    
    # vector of column names in order that user specified
    df_order <- c("DateTime", gsub("\\_.*","",var_src))
    
    var_index <- which(var_src == match_var)
    not_var_index <- which(var_src != match_var)
    data_list_ordered <- data_list[c(var_index, not_var_index)] #ordered with match_var on far left for use in combine_ts
    var_src_ordered <- var_src[c(var_index, not_var_index)]

    condense_stat_nm <- as.character(substitute(condense_stat))[1] 
    if(condense_stat_nm == 'function'){condense_stat_nm <- 'custom function'} 
    
    warning_info <- warning_table(var_src_ordered, condense_stat_nm, data_list_ordered, site_name, method, quietly)

    # applying condense_stat
    to_condense <- grep("Condensed", warning_info$result)
    
    . <- '.dplyr.var'
    longitude <- ifelse(length(to_condense) != 0, 
                        get_meta("basic") %>% v() %>% filter(site_name == get('site_name')) %>% .$lon,
                        NA)
    
    #distinguish NA observations from NAs added during the full_join
    if(length(to_condense) > 0) {
      original_obs_list <- lapply(data_list_ordered[to_condense], function(d){
        nm <- paste0("has.", names(d)[2])
        d[[nm]] <- TRUE
        return(d)
      })
      if(length(to_condense) > 1) {
        to_condense_df <- do.call("full_join", c(original_obs_list, list(by='DateTime'))) %>%
          {.[order(.$DateTime),]}
      } else {
        to_condense_df <- original_obs_list[[1]]
      }
      
      vars_condensed_list <- condense_by_stat(to_condense_df, condense_stat = condense_stat, site_lon = longitude, 
                                              day_start = day_start, day_end = day_end)
      data_list_condensed <- append(data_list_ordered[-to_condense], vars_condensed_list)
    } else {
      data_list_condensed <- data_list_ordered
    }
    
    combo <- do.call(combine_ts, c(data_list_condensed, list(method=method, approx_tol=approx_tol)))
    combo <- combo[, df_order] #put columns back as user specified
    
  } else {
    combo <- data_list[[1]]
  }
  combo
}

#' Condense a data.frame to daily resolution
#' 
#' Cndense according to day_start, day_end, and the local solar mean time of the
#' site
#' 
#' @param ts the ts dataframe to condense
#' @inheritParams get_ts
#' @param site_lon the longitude of the site
#' @keywords internal
condense_by_stat <- function(ts, condense_stat, site_lon, day_start, day_end){
    
  solar.time <- convert_UTC_to_solartime(ts$DateTime, site_lon, time.type = "mean solar")
  ts_solar <- ts %>% v() %>% mutate(solar.time = solar.time)
  
  ts_condensed <- mm_model_by_ply(
    model_fun=condense_by_ply, # should look like mm_model_by_ply_prototype
    data=ts_solar, #include sitetime
    data_daily=NULL,
    day_start=day_start,
    day_end=day_end,
    day_tests = c(),
    timestep_days=FALSE,
    stat_func=condense_stat)
  
  DateTime <- everything <- . <- '.dplyr.var'
  ts_condensed <- ts_condensed %>% 
    mutate(DateTime = convert_solartime_to_UTC(as.POSIXct(paste(as.character(date), "12:00:00"), tz='UTC'),
                                                 longitude=site_lon, time.type="mean solar")) %>% 
    select(DateTime, everything(), -date) %>% 
    u(get_units(ts)[names(.)])
  
  ts_condensed_list <- lapply(2:length(ts_condensed), function(var_col, df_all){
    df <- df_all[, c(1, var_col)]
  }, df_all = ts_condensed)
  
  return(ts_condensed_list)
}

#' Condense function called by mm_model_by_ply
#' 
#' Following mm_model_by_ply_prototype
#' 
#' @param data_ply as in mm_model_by_ply_prototype
#' @param data_daily_ply as in mm_model_by_ply_prototype
#' @param day_start as in mm_model_by_ply_prototype
#' @param day_end as in mm_model_by_ply_prototype
#' @param ply_date as in mm_model_by_ply_prototype
#' @param ply_validity as in mm_model_by_ply_prototype
#' @param timestep_days as in mm_model_by_ply_prototype
#' @param ... as in mm_model_by_ply_prototype
#' @keywords internal
condense_by_ply <- function(data_ply, data_daily_ply, day_start, day_end, ply_date, ply_validity, timestep_days, ...) {
  args <- list(...)
  
  dates_col <- which(names(data_ply) == "solar.time")
  data_col <- which(!names(data_ply) %in% c("DateTime", "solar.time"))
  vars <- grep("has", names(data_ply[data_col]), invert = TRUE, value = TRUE)
  
  vars_list <- lapply(setNames(vars, vars), function(var, df, stat_func){
    has_nm <- paste0("has.", var)
    has <- which(!is.na(df[,has_nm]))
    condensed_var <- stat_func(df[has, var])
    return(condensed_var)
  }, df = data_ply, stat_func = args$stat_func)
  
  summarize_stat <- as.data.frame(vars_list)
  
  return(summarize_stat)
}

#' Describe data changes that will occur on combining multiple tses
#' 
#' Throw a warning containing a table of resolution & extent changes when
#' var_srces are condensed. Return a smaller table containing the timestep and
#' expected resolution of each var_src after condensing.
#' 
#' @inheritParams get_ts
#' @param data list of downloaded timeseries data.frames, as named in var_src
#' @keywords internal
warning_table <- function(var_src, condense_stat, data, site_name, method, quietly){
  timestep_df <- summarize_ts(var_src=setNames(data, var_src), site_name, out="modal_timestep") %>% unitted::v()

  all_dates <- do.call(rbind, lapply(data, function(data) {
    all_dates <- unitted::v(data$DateTime)
    return(data.frame(start_date = all_dates[1], 
                      end_date = tail(all_dates, 1)))  
  }))
    
  timestep_df <- timestep_df %>% bind_cols(., all_dates) 
  
  get_timestep_info <- function(v, t, s, e, condense_stat, t_match, s_match, e_match){
    
    res_words <- switch(as.character(t),
                        "60" = "hourly",
                        "1440" = "daily",
                        paste(t, "min"))
    
    if(t == t_match){
      res_result <- "As is"
    }else if(t > t_match){
      res_result <- "NAs added"
    }else {
      if(condense_stat == "match"){
        res_result <- paste("Matched by", method)
      } else {
        if(t_match < 1440){
          res_result <- paste("Matched by", method, "(condense_stat not supported for sub-daily)")
        } else {
          res_result <- paste("Condensed by", condense_stat)
        }
      }
    }
    
    if(s < s_match){start_result <- "Earlier (truncated)"}
    if(s > s_match){start_result <- "Later (NAs added)"}
    if(s == s_match){start_result <- "Equal"}

    if(e < e_match){end_result <- "Earlier (NAs added)"}
    if(e > e_match){end_result <- "Later (truncated)"}
    if(e == e_match){end_result <- "Equal"}

    return(data.frame(var_src = v, resolution_change = res_words, resolution_result = res_result, 
                      start_date = start_result, end_date = end_result, stringsAsFactors = FALSE))
  }
  
  . <- resolution_change <- resolution_result <- '.dplyr.var'
  warning_df <- timestep_df %>% 
    rowwise() %>% 
    do(get_timestep_info(v = .$var_src, t = .$modal_timestep, s = .$start_date, e = .$end_date,
                         condense_stat = condense_stat, t_match = timestep_df$modal_timestep[1],
                         s_match = all_dates$start_date[1], e_match = all_dates$end_date[1]))

  match_res <- warning_df$resolution_change[1]
  match_var <- var_src[1]
  
  warning_df <- warning_df %>% 
    mutate(resolution_change = paste(resolution_change, "to", match_res)) %>% 
    mutate(resolution_change = replace(resolution_change, resolution_result == "As is", "No change"))

  # if there are any irregularities whatsoever, throw a warning
  if(!quietly) {
    if(any(warning_df$resolution_change != 'No change') ||
       any(warning_df$resolution_result != 'As is') ||
       any(warning_df$start_date != 'Equal') ||
       any(warning_df$end_date != 'Equal')) {
      warning(paste0(capture.output(print(warning_df, row.names=FALSE)), collapse='\n'))
    }
  }
  
  return(data.frame(timestep = timestep_df$modal_timestep, 
                    result = warning_df$resolution_result,
                    stringsAsFactors = FALSE))
}
