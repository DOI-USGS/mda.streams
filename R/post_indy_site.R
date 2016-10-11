#' Create a indy site and post a list of timeseries files to it
#' 
#' This function is easiest to use when applied to the output of 
#' \code{\link{stage_indy_site}}.
#' 
#' @param file_list a list of files as given by stage_indy_site. The site name 
#'   will be determined from the first item (a metadata vector) in this list.
#' @param on_site_exists see \code{on_exists} argument to
#'   \code{\link{post_site}}
#' @examples 
#' \dontrun{
#' staged <- stage_indy_site(info="help file example for post_indy_site")
#' login_sb()
#' post_indy_site(staged)
#' }
#' @importFrom unitted u
#' @export
post_indy_site <- function(file_list, on_site_exists=c("clear", "skip", "stop", "replace"), summarize=FALSE) {
  
  # check for completeness - if gpp, er, etc. were imperfectly specified, they
  # won't exist, and we should error here.
  if('data_daily_template' %in% names(file_list)) {
    stop("file_list contains 'data_daily_template' and so is probably",
         "incomplete. check for warnings from preceding stage_indy_site call")
  }
  
  # figure out which indy site this is
  site <- file_list$site_name
  
  # site item
  post_site(site, on_exists=match.arg(on_site_exists), verbose=TRUE)
  Sys.sleep(1) # otherwise might get Error in FUN(X[[i]], ...) : no site folder available for site indy_000000 on first post_ts call
  
  # site timeseries
  lapply(file_list[-(1:2)], post_ts)
  
  # update the indy metadata
  post_meta(file_list$metadata, on_exists="replace")
  
  # update the basic metadata
  mbf <- stage_meta_basic(on_exists='replace')
  post_meta(mbf, on_exists='replace')
  
  # report on what we've created
  if(isTRUE(summarize)) {
    bind_rows(lapply(site, function(s) {
      summarize_ts(list_tses(s), s)
    }))
  } else {
    invisible(NULL)
  }
}
