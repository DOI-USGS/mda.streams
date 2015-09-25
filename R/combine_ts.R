#' Combine ts/const variables into a single data.frame
#' 
#' Combines one or more ts and/or const data.frames, joining on DateTime. The 
#' data.frames are iteratively added to the first one, moving left to right in 
#' the list of inputs. The first element must be a time series. If a subsequent 
#' element is a constant (1 row, with DateTime=NA) then that constant is added 
#' for all rows.
#' 
#' @param ... one or more ts and/or const data.frames to be combined
#' @param method character specifying the combination method to use
#' @param approx_tol difftime. if method='approx', the maximum time interval 
#'   over which an approximation will be used to fill in data gaps in the 
#'   2nd:nth data argument to combine_ts (relative to the first argument)
#' @import dplyr
#' @importFrom unitted u v get_units
#' @importFrom stats setNames approx
#' @export
#' @examples 
#' \dontrun{
#' xy <- download_ts(c('suntime_calcLon', 'wtr_nwis'), 'nwis_01467087', on_local_exists="replace")
#' suntime <- read_ts(xy[1])[410:520,]
#' wtr <- read_ts(xy[2])[400:545,]
#' fulljoined <- combine_ts(suntime, wtr, method='full_join')
#' approxjoined <- combine_ts(suntime, wtr, method='approx')
#' library(ggplot2)
#' ggplot(unitted::v(approxjoined), aes(x=DateTime, y=wtr)) + 
#'   geom_line(color="blue") + theme_bw() +
#'   geom_line(data=unitted::v(fulljoined), color="red")
#' }
combine_ts <- function(..., method=c('full_join', 'left_join', 'inner_join', 'approx'), approx_tol=as.difftime(3, units="hours")) {
  # determine the function with which to combine the dots args
  method <- match.arg(method)
  if(method %in% c('full_join', 'left_join', 'inner_join')) {
    # use requested dplyr join method
    combine_fun <- combine_dplyr(method, by='DateTime')
    
  } else if(method == 'approx') {
    # define function to approximate y for the dates in x
    approx_toln <- as.numeric(approx_tol, units="secs")
    combine_fun <- function(x, y) {
      # figure out what the time gap is between a given x datetime and the
      # nearest y datetime
      x_date_num <- as.numeric(x$DateTime)
      y_date_num <- as.numeric(y$DateTime)
      prev_y <- approx(x=y_date_num, y=y_date_num, xout=x_date_num, method="constant", f=0, rule=2)
      next_y <- approx(x=y_date_num, y=y_date_num, xout=x_date_num, method="constant", f=1, rule=2) 
      min_gap <- pmin(abs(prev_y$y-prev_y$x), abs(next_y$y-next_y$x))
      
      # approximate y
      if(length(y_date_num) == 1) {
        #  expand 1-row, non-const y dfs (e.g., just one date of daily data) to 2 rows so approx will work
        y_date_num <- y_date_num + c(0,0.01)
        y <- rbind(y,y)
      }
      y_approx <- approx(x=y_date_num, y=as.numeric(y[,2]), xout=x_date_num, rule=2)$y
      if(names(y)[2] == 'suntime') {
        posix_origin <- as.POSIXct("1970-01-01 00:00:00", tz="UTC") # in ?as.POSIXct | Note
        y_approx <- as.POSIXct(y_approx, origin=posix_origin, tz="UTC")
      }
      # remove the value if the gap is beyond our tolerance
      y_approx[min_gap > approx_toln] <-  NA
      
      # combine x and y_approx into a data.frame
      df <- data.frame(x, y=u(y_approx, get_units(y[,2]))) %>%
        setNames(c(names(x), names(y)[2])) %>%
        u()
      DateTime <- '.dplyr.var'
      df %>% v() %>% arrange(DateTime) %>% u(get_units(df))
    }
  }
  
  # do the join left to right using the specified combine_fun
  combine_tables(..., by='DateTime', fun=combine_fun, allow_constants=TRUE)
}