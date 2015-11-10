#' Create a styx site and post a list of timeseries files to it
#' 
#' This function is easiest to use when applied to the output of 
#' \code{\link{stage_styx_site}}.
#' 
#' @param file_list a list of files as given by stage_styx_site. The site name 
#'   will be determined from the first item (a metadata vector) in this list.
#' @examples 
#' \dontrun{
#' staged <- stage_styx_site(info="help file example for post_styx_site")
#' login_sb()
#' post_styx_site(staged)
#' }
#' @importFrom unitted u
#' @export
post_styx_site <- function(file_list) {
  
  # check for completeness - if gpp, er, etc. were imperfectly specified, they
  # won't exist, and we should error here.
  if('data_daily_template' %in% names(file_list)) {
    stop("file_list contains 'data_daily_template' and so is probably",
         "incomplete. check for warnings from preceding stage_styx_site call")
  }
  
  # figure out which Styx site this is
  site <- file_list$metadata["site_name"]
  basedon <- file_list$metadata["basedon"]
  info <- file_list$metadata["info"]
  
  # site item
  post_site(site, on_exists="clear", verbose=TRUE)
  Sys.sleep(1) # otherwise might get Error in FUN(X[[i]], ...) : no site folder available for site styx_000000 on first post_ts call
  
  # site timeseries
  lapply(file_list[-1], post_ts)
  
  # update the styx metadata
  meta_styx_file <- stage_meta_styx(rows=u(data.frame(site_name=site, basedon=basedon, info=info, stringsAsFactors=FALSE)), on_exists="replace")
  post_meta(meta_styx_file, on_exists="replace")
  Sys.sleep(1)
  
  # report on what we've created
  summarize_ts(list_tses(site), site)
}