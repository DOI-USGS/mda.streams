#' Make a metabolism config file
#' 
#' Quick-make function for manual making of a model config file
#' 
#' @section Data Source Format:
#'   
#'   If a data source is given as NA, the 'priority local' timeseries option 
#'   will be selected.
#'   
#'   If a data source is given as a character, the character will be used to 
#'   specify the \code{src} argument to \code{choose_data_source} with 
#'   code{type='ts'}.
#'   
#'   If a data source is given as NULL, the variable will be set to 'unused
#'   var'.
#'   
#'   For the most flexibility, each parameter whose definition begins with Data 
#'   Source may be supplied as a 4-column data.frame with column names 
#'   c('type','site','src','logic'). These specify where to find the data for a 
#'   given variable. The easiest way to create such a data.frame is usually with
#'   \code{\link{choose_data_source}}, though it may also be created manually.
#'   
#'   The variables requiring Data Source specification, along with their 
#'   expected units, are defined in the help file for \code{\link{mm_data}}.
#'   
#' @inheritParams stage_metab_config
#' @export
#' @examples 
#' make_metab_config('nwis_08062500')
#' make_metab_config('nwis_08062500', sitetime='calcLon', doobs='nwis', 
#'   dosat='calcGGbts', depth='calcDischHarvey', wtr='nwis', par='calcSw')
make_metab_config <- function(
  site, 
  sitetime=NA, doobs=NA, dosat=NA, depth=NA, wtr=NA, par=NA,
  disch=NULL, veloc=NULL, 
  sitedate=NULL, doinit=NULL, 
  gpp=NULL, er=NULL, K600=NULL, K600lwr=NULL, K600upr=NULL,
  dischdaily=NULL, velocdaily=NULL,
  start_date=NA, end_date=NA,
  tag='0.0.0', strategy='get_metab_data', date=Sys.time(), model='metab_mle', model_args='list()') {
  
  # helper function for translating the above simpler args into args suitable
  # for stage_metab_config
  # 
  # @param var a variable, passed in as a named object, whose name will be used
  #   as the variable name and whose value will be used (1) as the output of 
  #   choose_data_source if a data.frame, (2) as a request for an unused var if
  #   NULL, (3) as a request for a priority local selection if NA, and (4) as a
  #   specific src value if character
  choose_src <- function(var) {
    varname <- as.character(substitute(var))
    if(is.data.frame(var)) {
      var 
    } else {
      if(is.null(var)) {
        choose_data_source(varname, site=site, logic='unused var')
      } else if(is.na(var)) {
        choose_data_source(varname, site=site, logic='priority local', src=var)
      } else {
        choose_data_source(varname, site=site, logic='specific src', type='ts', src=var)
      }
    }
  }
  
  # run stage_metab_config with the prepared arguments
  stage_metab_config(
    tag=tag, strategy=strategy, date=date,
    site=site, model=model, model_args=model_args,
    sitetime=choose_src(sitetime),
    doobs=choose_src(doobs),
    dosat=choose_src(dosat),
    depth=choose_src(depth),
    wtr=choose_src(wtr),
    par=choose_src(par),
    disch=choose_src(disch),
    veloc=choose_src(veloc),
    sitedate=choose_src(sitedate),
    doinit=choose_src(doinit),
    gpp=choose_src(gpp),
    er=choose_src(er),
    K600=choose_src(K600),
    K600lwr=choose_src(K600lwr),
    K600upr=choose_src(K600upr),
    dischdaily=choose_src(dischdaily),
    velocdaily=choose_src(velocdaily),
    start_date=NA, end_date=NA,
    omit_incomplete=FALSE,
    filename=NULL)  
  
}