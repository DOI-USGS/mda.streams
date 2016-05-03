#' Get a data.frame of metabolism input data
#' 
#' Quick-get function for manual getting of metabolism data
#' @inheritParams stage_metab_config
#' @param out character vector of one or more elements of
#'   \code{c('data','data_daily','info')} to return
#' @param simplify_config logical. If the implied config file has only one row,
#'   should the list be reduced to one element?
#' @param simplify_out If \code{out} has only one element, should the list of
#'   lists be reduced to a list of data.frames?
#' @param verbose logical. give progress messages?
#' @export
#' @examples 
#' md <- get_metab_prep('nwis_08062500') # list of $data, $data_daily, $info
#' md <- get_metab_prep('nwis_08062500', simplify_config=FALSE) # list of lists
#' md <- get_metab_prep('nwis_08062500', out='data') # single data.frame
#' md <- get_metab_prep(c('nwis_08062500','nwis_01646500'), out='data') # list of data.frames
get_metab_prep <- function(
  site, out=c('data','data_daily','info'),
  sitetime=NA, doobs=NA, dosat=NA, depth=NA, wtr=NA, par=NA,
  disch=NULL, veloc=NULL, 
  sitedate=NULL, doinit=NULL, 
  gpp=NULL, er=NULL, K600=NULL, K600lwr=NULL, K600upr=NULL,
  dischdaily=NULL, velocdaily=NULL,
  start_date=NA, end_date=NA,
  tag='0.0.0', strategy='get_metab_data', date=Sys.time(), model='metab_mle', model_args='list()',
  simplify_config=TRUE, simplify_out=TRUE, verbose=TRUE) {
  
  # arg checks
  out <- match.arg(out, several.ok=TRUE)
  
  # make a config file
  config <- make_metab_config(
    site=site,
    sitetime=sitetime, doobs=doobs, dosat=dosat, depth=depth, wtr=wtr, par=par,
    disch=disch, veloc=veloc, sitedate=sitedate, doinit=doinit,
    gpp=gpp, er=er, K600=K600, K600lwr=K600lwr, K600upr=K600upr,
    dischdaily=dischdaily, velocdaily=velocdaily,
    start_date=start_date, end_date=end_date,
    tag=tag, strategy=strategy, date=date, model=model, model_args=model_args)
  
  # make a data list from the config file
  metab_data_list <- config_to_metab(
    config=config, rows=seq_len(nrow(config)), verbose=verbose, prep_only=TRUE)
  
  # simplify the output if requested
  if(length(out) < 3) {
    metab_data_list <- lapply(metab_data_list, function(mdl) {
      if(length(out) == 1 && simplify_out) 
        mdl[[out]] 
      else 
        mdl[out]
    })
  }
  if(length(metab_data_list) == 1 && simplify_config) 
    metab_data_list <- metab_data_list[[1]]
  
  # return
  metab_data_list
}
