#' Get a data.frame of metabolism input data
#' 
#' Quick access to metabolism input data (\code{data} arg only)
#' 
#' @inheritParams get_metab_prep
#' @param simplify logical. If there's only one data.frame to retrieve, remove 
#'   it from the enclosing list?
#' @return a list of data.frames, or if simplify=TRUE and there's only one
#'   data.frame, a single data.frame of input data
#' @export
#' @examples
#' \dontrun{
#' md <- get_metab_data('nwis_08062500')
#' md <- get_metab_data('styx_004001')
#' login_sb()
#' md <- get_metab_data('styx_004001', doobs=choose_data_source('doobs','styx_004001','sim',type='pred',src='160411 0.0.18'))
#' }
get_metab_data <- function(
  site,
  sitetime=NA, doobs=NA, dosat=NA, depth=NA, wtr=NA, par=NA,
  disch=NULL, veloc=NULL, 
  sitedate=NULL, doinit=NULL, 
  gpp=NULL, er=NULL, K600=NULL, K600lwr=NULL, K600upr=NULL,
  dischdaily=NULL, velocdaily=NULL,
  start_date=NA, end_date=NA,
  tag='0.0.0', strategy='get_metab_data', date=Sys.time(), model='metab_mle', model_args='list()',
  simplify=TRUE, verbose=TRUE) {
  
  get_metab_prep(
    site=site, out='data',
    sitetime=sitetime, doobs=doobs, dosat=dosat, depth=depth, wtr=wtr, par=par,
    disch=disch, veloc=veloc, sitedate=sitedate, doinit=doinit,
    gpp=gpp, er=er, K600=K600, K600lwr=K600lwr, K600upr=K600upr,
    dischdaily=dischdaily, velocdaily=velocdaily,
    start_date=start_date, end_date=end_date,
    tag=tag, strategy=strategy, date=date, model=model, model_args=model_args,
    simplify_config=simplify, simplify_out=TRUE, verbose=verbose)
}
