#' Bring one or more timeseries into R, merge them, and compute a daily
#' statistic
#' 
#' Downloads each file from SB if either (1) the file has not yet been
#' downloaded to the code{tempdir()} during this R session, or (2) 
#' \code{on_local_exists='replace'}. There's a small risk that the resulting ts 
#' will be out of date relative to ScienceBase, but the benefit is faster 
#' ts-getting.
#' 
#' @param stat the function to apply to get the desired statistic. mean, sum,
#'   max, function(x) {mean(x, na.rm=TRUE)}, etc. are all valid.
#' @param day_start the time (as numeric hours, possibly negative) relative to 
#'   each date from which to collect dates and possibly daily doinit (DO.mod.1) 
#'   values
#' @param day_end the time (as numeric hours, possibly >24) relative to each 
#'   date from which to collect dates
#' @inheritParams get_ts
get_ts_daily <- function(
  var_src, site_name, stat=mean, day_start=4, day_end=28, 
  method='approx', approx_tol=as.difftime(3, units="hours"), on_local_exists='skip', on_invalid='stop') {
  
#   new_fit_rows <- streamMetabolizer:::mm_model_by_ply(
#     function(data_ply, data_daily_ply, day_start, day_end, local_date, tests, model_specs) {
#       which_night <- which(data_ply$light < 0.1) #v(u(0.1, "umol m^-2 s^-1")))
#       has_night <- length(which_night) > 0
#       data.frame(
#         row.first = if(has_night) which_night[1] else NA,
#         row.last = if(has_night) which_night[length(which_night)] else NA)
#     },
#     data=v(get_data(old_mm)), data_daily=NULL,
#     day_start=get_args(old_mm)$day_start, day_end=get_args(old_mm)$day_end,
#     tests=c(), model_specs=c()
#   )
  
  warning("function is under construction")
}