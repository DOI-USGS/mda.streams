#' Build and return the table calc_ts_needs
#' 
#' @param var if given, the var by which to filter the table
#' @param src if given, the src by which to filter the table
#' @import dplyr
#' @import tibble
#' @export
build_calc_ts_needs <- function(var, src) {
  
  . <- var_src <- var_target <- src_target <- '.dplyr.var'
  
  # create data frame of var_target, src_target in an order that can be run top 
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
      'par_calcSw', # NA | sw_nldas
      'doamp_calcDAmp', # sitedate_calcLon dopsat_calcObsSat | NA
      'dischdaily_calcDMean', # sitedate_calcLon | disch_nwis
      'velocdaily_calcDMean' # sitedate_calcLon | veloc_best
    ) %>%
    {tibble(var_src = .)} %>% 
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
    add_need('doamp_calcDAmp', 'vs', c('sitedate_calcLon', 'dopsat_calcObsSat')) %>%
    add_need('dischdaily_calcDMean', 'vs', c('sitedate_calcLon', 'disch_nwis')) %>%
    add_need('velocdaily_calcDMean', 'vs', c('sitedate_calcLon', 'veloc_calcDischRaymond')) %>%
    
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
