#' Build and return the table calc_ts_needs
#' 
#' @param var if given, the var by which to filter the table
#' @param src if given, the src by which to filter the table
#' @import dplyr
#' @import tibble
#' @export
build_calc_ts_needs <- function(var, src) {
  
  . <- priority <- var_src <- var_target <- src_target <- '.dplyr.var'
  
  ## helper functions
  
  # plug in needs specific to each var, src
  add_need <- function(calc_needs, var_src, type=c('vs','c','d'), need) {
    rows <- calc_needs$var_src %in% var_src
    cols <- c(vs='var_src_needs',c='coord_needs',d='dvq_needs')[[type]]
    new_needs <- trimws(paste(unlist(unname(calc_needs[rows, cols])), paste(need, collapse=' ')))
    calc_needs[rows, cols] <- new_needs
    calc_needs
  }
  # look up the possible var_src combos, excepting specific srcs
  vs_options <- function(varname, except) {
    vs <- if(missing(except)) {
      get_var_src_codes(var==varname, out=c('var_src','priority'))
    } else {
      get_var_src_codes(var==varname, !grepl(except, src), out=c('var_src','priority'))
    }
    arrange(vs, priority) %>% { paste0(.$var_src, collapse='|') }
  }
  
  # create data frame of var_target, src_target in an order that can be run top 
  # to bottom (dependencies of row x are always in rows 1:(x-1) or not calc). OR
  # values are not currently well represented; we actually just pick one as the
  # requirement
  calc_needs <- # calc dependencies | other dependencies
    c('dosat_calcGGbts', # NA | wtr_best baro_best!=baro_calcElev
      'baro_calcElev', # NA | alt
      'dosat_calcGGbconst', # baro_calcElev | wtr_best
      'dopsat_calcObsSat', # dosat_best | doobs_best
      'depth_calcDischRaymond', # NA | disch_best
      'veloc_calcDischRaymond', # NA | disch_best
      'depth_calcDischHarvey', # NA | disch_best dvqcoefs.c dvqcoefs.f
      'veloc_calcDischHarvey', # NA | disch_best dvqcoefs.k dvqcoefs.m
      'sitetime_calcLon', # NA | doobs_best lon
      'suntime_calcLon', # NA | doobs_best lon
      'par_calcLat', # suntime_calcLon | lat
      'par_calcSw', # NA | sw_best
      'par_calcLatSw', # par_calcLat | sw_best
      'sitedate_calcLon', # sitetime_calcLon | lon
      'doamp_calcDAmp', # sitedate_calcLon dopsat_calcObsSat | NA
      'dischdaily_calcDMean', # sitedate_calcLon | disch_best
      'velocdaily_calcDMean', # sitedate_calcLon | veloc_best
      'mfootdaily_calc3vK' # sitedate_calcLon K600_estBest veloc_best | wtr_best
    ) %>%
    {tibble(var_src = .)} %>% 
    mutate(
      var_target=parse_var_src(var_src, out='var'),
      src_target=parse_var_src(var_src, out='src'),
      var_src_needs='',
      coord_needs='',
      dvq_needs='')
  
  calc_needs <- calc_needs %>%
    add_need('dosat_calcGGbts', 'vs', c(vs_options('wtr'),vs_options('baro', 'calcElev'))) %>%
    add_need('baro_calcElev', 'c', 'alt') %>%
    add_need('dosat_calcGGbconst', 'vs', c('baro_calcElev',vs_options('wtr'))) %>%
    add_need('dopsat_calcObsSat', 'vs', c(vs_options('dosat'),vs_options('doobs'))) %>%
    add_need('depth_calcDischRaymond', 'vs', vs_options('disch')) %>%
    add_need('veloc_calcDischRaymond', 'vs', vs_options('disch')) %>%
    add_need('depth_calcDischHarvey', 'vs', vs_options('disch')) %>% add_need('depth_calcDischHarvey', 'd', c('c','f')) %>%
    add_need('veloc_calcDischHarvey', 'vs', vs_options('disch')) %>% add_need('veloc_calcDischHarvey', 'd', c('k','m')) %>% 
    add_need('sitetime_calcLon', 'vs', vs_options('doobs')) %>% add_need('sitetime_calcLon', 'c', 'lon') %>%
    add_need('suntime_calcLon', 'vs', vs_options('doobs')) %>% add_need('suntime_calcLon', 'c', 'lon') %>%
    add_need('par_calcLat', 'vs', 'suntime_calcLon') %>% add_need('par_calcLat', 'c', 'lat') %>%
    add_need('par_calcSw', 'vs', vs_options('sw')) %>%
    add_need('par_calcLatSw', 'vs', c('doobs_nwis', 'par_calcSw')) %>% add_need('par_calcLatSw', 'c', c('lat','lon')) %>% 
    add_need('sitedate_calcLon', 'vs', 'sitetime_calcLon') %>% add_need('sitedate_calcLon', 'c', 'lon') %>%
    add_need('doamp_calcDAmp', 'vs', c('sitedate_calcLon', 'dopsat_calcObsSat')) %>%
    add_need('dischdaily_calcDMean', 'vs', c('sitedate_calcLon', vs_options('disch'))) %>%
    add_need('velocdaily_calcDMean', 'vs', c('sitedate_calcLon', vs_options('veloc'))) %>%
    add_need('mfootdaily_calc3vK', 'vs', c('sitedate_calcLon', 'K600_estBest', vs_options('veloc'), vs_options('wtr')))
  
  # return the table, filtering by var and src if requested
  if(!missing(var)) calc_needs <- filter(calc_needs, var_target %in% var)
  if(!missing(src)) calc_needs <- filter(calc_needs, src_target %in% src)
  as.data.frame(calc_needs)
}
