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
                   on_local_exists='skip', on_invalid='stop', match_var = "leftmost", 
                   condense_stat = mean, day_start = 4, day_end = 28) {

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

    condense_stat_nm <-as.character(substitute(condense_stat))[1] 
    if(condense_stat_nm == 'function'){condense_stat_nm <- 'custom function'} 
    
    warning_info <- warning_table(var_src_ordered, condense_stat_nm, data_list_ordered, site_name, method)

    # applying condense_stat
    to_condense <- grep("Condensed", warning_info$result)
    
    longitude <- ifelse(length(to_condense) != 0, 
                        get_meta("basic") %>% v() %>% filter(site_name == get('site_name')) %>% .$lon,
                        NA)
    
    #distinguish NA observations from NAs added during the full_join
    original_obs_list <- lapply(data_list_ordered[to_condense], function(d){
      nm <- paste0("has.", names(d)[2])
      d[[nm]] <- TRUE
      return(d)
    })
    original_obs_list$by <- "DateTime"
    to_condense_df <- do.call("full_join", original_obs_list)
    
    vars_condensed_list <- condense_by_stat(to_condense_df, condense_stat = condense_stat, site_lon = longitude, 
                          day_start = day_start, day_end = day_end)
    data_list_condensed <- append(data_list_ordered[-to_condense], vars_condensed_list)
    
    combo <- do.call(combine_ts, c(data_list_condensed, list(method=method, approx_tol=approx_tol)))
    combo <- combo[, df_order] #put columns back as user specified
    
  } else {
    combo <- data_list[[1]]
  }
  combo
}

condense_by_stat <- function(ts, condense_stat, site_lon, day_start, day_end){
    
  solar.time <- convert_UTC_to_solartime(ts$DateTime, site_lon, time.type = "mean solar")
  ts_solar <- ts %>% v() %>% mutate(solar.time = solar.time)
  
  ts_condensed <- streamMetabolizer:::mm_model_by_ply(
    model_fun=condense_by_ply, # should look like mm_model_by_ply_prototype
    data=ts_solar, #include sitetime
    data_daily=NULL,
    day_start=day_start,
    day_end=day_end,
    #day_tests = c(),
    stat_func=condense_stat
  )
  
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

condense_by_ply <- function(data_ply, data_daily_ply, ..., day_start, day_end, ply_date) {
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

warning_table <- function(var_src, condense_stat, data, site_name, method){
  timestep_df <- summarize_ts(var_src, site_name, out="modal_timestep") %>% unitted::v()

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
      res_result <- "NAs introduced"
    }else {
      if(condense_stat == "match"){
        res_result <- paste("Matched by", method)
      } else {
        if(t_match < 1440){
          res_result <- "Matched by approx (condense_stat not supported for sub-daily)"
        } else {
          res_result <- paste("Condensed by", condense_stat)
        }
      }
    }
    
    if(s < s_match){start_result <- "Earlier (unused data)"}
    if(s > s_match){start_result <- "Later (missing data)"}
    if(s == s_match){start_result <- "Equal"}

    if(e < e_match){end_result <- "Earlier (missing data)"}
    if(e > e_match){end_result <- "Later (unused data)"}
    if(e == e_match){end_result <- "Equal"}

    return(data.frame(var_src = v, resolution_change = res_words, result = res_result, start_date = start_result, 
                      end_date = end_result, stringsAsFactors = FALSE))
  }
  
  warning_df <- timestep_df %>% 
    rowwise() %>% 
    do(get_timestep_info(v = .$var_src, t = .$modal_timestep, s = .$start_date, e = .$end_date,
                         condense_stat = condense_stat, t_match = timestep_df$modal_timestep[1],
                         s_match = all_dates$start_date[1], e_match = all_dates$end_date[1]))

  match_res <- warning_df$resolution_change[1]
  match_var <- var_src[1]
  
  warning_df <- warning_df %>% 
    mutate(resolution_change = paste(resolution_change, "to", match_res)) %>% 
    mutate(resolution_change = replace(resolution_change, result == "As is", "No change"))

  warning(print(warning_df))
  
  return(data.frame(timestep = timestep_df$modal_timestep, 
                    result = warning_df$result,
                    stringsAsFactors = FALSE))
}
