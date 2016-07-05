#' Attempt to stage and post a calc_ts for all available sites
#' 
#' @param var_src the var_src to stage+post
#' @param sb_user the sciencebase username
#' @param sb_pass the sciencebase password
#' @param done_after posixct datetime - sites will not be considered 'done' 
#'   unless there is a timeseries file available from after this datetime
#' @param done_version character in c('rds','tsv') - sites will not be
#'   considered 'done' unless the timeseries file already constrained by
#'   done_after is also one of the specified version
#' @import dplyr
#' @importFrom unitted v
#' @export
try_calc_ts <- function(
  var_src, sb_user, sb_pass, done_after=Sys.time()-as.difftime(3,units='days'), done_version='rds') {
  
  . <- upload_date <- '.dplyr.var'
  
  # look up the necessary inputs
  calc_ts_needs <- build_calc_ts_needs(parse_var_src(var_src, out='var'), parse_var_src(var_src, out='src'))
  
  # determine which sites have necessary inputs available
  avail_sites <- (if(calc_ts_needs$var_src_needs=='') list_sites() else list_sites(strsplit(calc_ts_needs$var_src_needs, ' ')[[1]])) %>%
    grep("^styx", ., invert=TRUE, value=TRUE)
  if(calc_ts_needs$coord_needs != '') {
    with_coord <- get_meta('basic') %>% v() %>%
    {.$site_name[complete.cases(.[, strsplit(calc_ts_needs$coord_needs, ' ')[[1]], drop=FALSE])]}
    avail_sites <- intersect(avail_sites, with_coord)
  }
  if(calc_ts_needs$dvq_needs != '') {
    with_dvq <- get_meta('dvqcoefs') %>% v() %>%
    {.$site_name[complete.cases(.[, paste0('dvqcoefs.',strsplit(calc_ts_needs$dvq_needs, ' ')[[1]]), drop=FALSE])]}
    avail_sites <- intersect(avail_sites, with_dvq)
  }
  num_avail <- length(avail_sites)
  message("found ", num_avail, " available sites")
  
  # exclude sites we've already completed for this var, src
  done_sites <- summarize_ts_files(var_src) %>%
    filter(version %in% done_version & upload_date > done_after) %>%
    .$site_name
  num_done <- length(done_sites)
  message("found ", num_done, " completed sites")
  sites <- setdiff(avail_sites, done_sites)
  message("attempting the ", length(sites), " remaining sites")
  
  # build var, src for the still-needed sites. if it fails this time, just keep
  # chugging. we can run this function again
  for(i in seq_len(length(sites))) {
    message("site ", sites[i], " (", num_done+i, " of ", num_avail, "):")
    tryCatch({
      staged <- stage_calc_ts(sites[i], var=parse_var_src(var_src, out='var'), src=parse_var_src(var_src, out='src'))
      if(!sbtools::is_logged_in()) sbtools::authenticate_sb(sb_user, sb_pass)
      post_ts(staged)
    }, error=function(e) {
      message("failed to stage/post ts ", var_src, " for site ", sites[i], ": ", e$message)
      if(grepl("already exists", e$message)) {
        message("attempting ts repair")
        repair_ts(var_src, sites[i])
      }
    })
    if(is.null(staged)) message("failure in stage_calc_ts: ts is NULL or has 0 non-NA values")
  }
}
