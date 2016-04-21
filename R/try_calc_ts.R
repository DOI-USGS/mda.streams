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
    {.$site_name[!complete.cases(.[, paste0('dvqcoefs.',strsplit(calc_ts_needs$dvq_needs, ' ')[[1]]), drop=FALSE])]}
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
  }
}

#' Build the table calc_ts_needs.Rds and store it in extdata
#' 
#' @import dplyr
#' @keywords internal
build_calc_ts_needs <- function(var, src) {
  
  . <- var_src <- var_target <- src_target <- '.dplyr.var'
  
  # create data_frame of var_target, src_target in an order that can be run top 
  # to bottom (dependencies of row x are always in rows 1:(x-1) or not calc). OR
  # values are not currently well represented; we actually just pick one as the
  # requirement
  calc_needs <- # calc dependencies | other dependencies
    c('sitetime_calcLon', # NA | doobs_nwis lon
      'sitedate_calcLon', # sitetime_calcLon | lon
      'dosat_calcGGbts', # NA | wtr_nwis baro_nldas
      'baro_calcElev', # NA | alt
      'dosat_calcGGbconst', # baro_calcElev | wtr_nwis 
      'dopsat_calcObsSat', # dosat_calcGGbts OR dosat_calcGGbconst | doobs_nwis
      'depth_calcDischRaymond', # NA | disch_nwis
      'veloc_calcDischRaymond', # NA | disch_nwis
      'depth_calcDischHarvey', # NA | disch_nwis dvqcoefs.c dvqcoefs.f
      'veloc_calcDischHarvey', # NA | disch_nwis dvqcoefs.k dvqcoefs.m
      'suntime_calcLon', # NA | doobs_nwis lon
      'par_calcLat', # suntime_calcLon | lat
      'par_calcSw' # NA | sw_nldas
    ) %>%
    {data_frame(var_src = .)} %>% 
    mutate(
      var_target=parse_var_src(var_src, out='var'),
      src_target=parse_var_src(var_src, out='src'),
      var_src_needs='',
      coord_needs='',
      dvq_needs='')
  
  # plug in needs specific to each var, src
  add_need <- function(calc_needs, var_src, type=c('vs','c','d'), need) {
    rows <- calc_needs$var_src %in% var_src
    cols <- c(vs='var_src_needs',c='coord_needs',d='dvq_needs')[[type]]
    new_needs <- trimws(paste(unlist(unname(calc_needs[rows, cols])), paste(need, collapse=' ')))
    calc_needs[rows, cols] <- new_needs
    calc_needs
  }
  calc_needs <- calc_needs %>% 
    add_need('sitetime_calcLon', 'vs', 'doobs_nwis') %>%
    add_need('sitedate_calcLon', 'vs', 'sitetime_calcLon') %>%
    add_need('dosat_calcGGbts', 'vs', c('wtr_nwis','baro_nldas')) %>%
    add_need('dosat_calcGGbconst', 'vs', c('wtr_nwis','baro_calcElev')) %>%
    add_need('dopsat_calcObsSat', 'vs', c('dosat_calcGGbts','doobs_nwis')) %>%
    add_need(c('depth_calcDischRaymond', 'veloc_calcDischRaymond', 'depth_calcDischHarvey', 'veloc_calcDischHarvey'), 
             'vs', 'disch_nwis') %>%
    add_need('suntime_calcLon', 'vs', 'doobs_nwis') %>%
    add_need('par_calcLat', 'vs', 'suntime_calcLon') %>%
    add_need('par_calcSw', 'vs', 'sw_nldas') %>%
    
    add_need(c('sitetime_calcLon', 'sitedate_calcLon', 'suntime_calcLon'), 'c', 'lon') %>%
    add_need('par_calcLat', 'c', 'lat') %>%
    add_need('baro_calcElev', 'c', 'alt') %>%
    add_need('depth_calcDischHarvey', 'd', c('c','f')) %>%
    add_need('veloc_calcDischHarvey', 'd', c('k','m'))
  
  # return the table, filtering by var and src if requested
  if(!missing(var)) calc_needs <- filter(calc_needs, var_target %in% var)
  if(!missing(src)) calc_needs <- filter(calc_needs, src_target %in% src)
  as.data.frame(calc_needs)
}