#' Write a metabolism modeling configuration file
#' 
#' Write a table (tsv) of configuration information for individual metabolism 
#' modeling jobs (one row/job per site-strategy combination). This tsv should 
#' reflect the full information needed to re-run a set of jobs. The jobs will 
#' probably, but not necessarily, be run on a Condor cluster.
#' 
#' @section Data Source Format:
#'   
#'   Every parameter whose definition begins with Data Source should be supplied
#'   as a 4-column data.frame with column names c('type','site','src','logic'). 
#'   These specify where to find the data for a given variable. The easiest way 
#'   to create such a data.frame is usually with 
#'   \code{\link{choose_data_source}}, though it may also be created manually.
#'   
#'   The variables requiring Data Source specification, along with their 
#'   expected units, are defined in the help file for \code{\link{mm_data}}.
#'   
#' @param tag character of form "1.0.2" that uniquely identifies this set of 
#'   modeling runs.
#' @param strategy character, or vector of length sites, describing this set of 
#'   modeling runs in concise English.
#' @param date POSIXct indicating the date of config construction. It is 
#'   strongly recommended to use the default.
#' @param model character. the name of the metabolism model to construct
#' @param model_args character, in R language, specifying any arguments to pass 
#'   to the model function
#' @param site site names
#' @param sitetime Data Source for mean solar time. See Data Source Format 
#'   below.
#' @param doobs Data Source for dissolved oxygen concentrations. See Data Source
#'   Format below.
#' @param dosat Data Source for dissolved oxygen saturation concentrations. See 
#'   Data Source Format below.
#' @param depth Data Source for mean stream depth. See Data Source Format below.
#' @param wtr Data Source for water temperature. See Data Source Format below.
#' @param par Data Source for light (photosynthetically available radiation, 
#'   PAR). See Data Source Format below.
#' @param disch Data Source for unit-value stream discharge, for use in 
#'   identifying daily priors or fixed values for K600. See Data Source Format 
#'   below.
#' @param veloc Data Source for unit-value flow velocity, for use in identifying
#'   daily priors or fixed values for K600. See Data Source Format below.
#' @param sitedate Data Source for the dates of interest. See Data Source Format
#'   below.
#' @param doinit Data Source for the first DO observation on each date to model,
#'   for use in data simulation. See Data Source Format below.
#' @param gpp Data Source for daily gross primary productivity rates for use in 
#'   data simulation. See Data Source Format below.
#' @param er Data Source for ecosystem respiration rates for use in data 
#'   simulation. See Data Source Format below.
#' @param K600 Data Source for reaeration rates for use in data simulation. See 
#'   Data Source Format below.
#' @param dischdaily Data Source for daily mean stream discharge, for use in 
#'   identifying daily priors or fixed values for K600. See Data Source Format 
#'   below.
#' @param velocdaily Data Source for daily mean flow velocity, for use in 
#'   identifying daily priors or fixed values for K600. See Data Source Format 
#'   below.
#' @param start_date NA or datetime, to be coerced with 
#'   \code{as.POSIXct(start_date, tz="UTC")}, at which to start the data passed 
#'   to the metab model
#' @param end_date NA or datetime, to be coerced with \code{as.POSIXct(end_date,
#'   tz="UTC")}, at which to end the data passed to the metab model
#' @param omit_incomplete logical. If one or more datasets required for the 
#'   specified config row is unavailable, should that row be omitted?
#' @param filename character or NULL. If NULL, the function returns a 
#'   data.frame, otherwise it writes that data.frame to the file specified by 
#'   filename.
#' @return file name of the config file
#' @import streamMetabolizer
#' @import dplyr
#' @importFrom utils write.table
#' @export
#' @examples
#' \dontrun{
#' login_sb()
#' site="nwis_01646000"
#' cfg <- stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'   model="metab_mle", site=site, filename=NULL,
#'   sitetime=choose_data_source("sitetime", site, logic="manual", src="calcLon", type="ts"),
#'   doobs=choose_data_source("doobs", site, logic="unused var"),
#'   dosat=choose_data_source("dosat", site, logic="unused var"),
#'   depth=choose_data_source("depth", site, logic="unused var"),
#'   wtr=choose_data_source("wtr", site, logic="unused var"),
#'   par=choose_data_source("par", site, logic="unused var"),
#'   K600=choose_data_source("K600", site, logic="nighttime", src="0.0.6", type="pred"),
#'   dischdaily=choose_data_source("dischdaily", site, logic="manual", src="calcDMean", type="ts"),
#'   velocdaily=choose_data_source("velocdaily", site, logic="manual", src="calcDMean", type="ts"),
#'   omit_incomplete=FALSE)
#' stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_01646000", filename=NULL)
#' stage_metab_config(tag="0.0.1", strategy="test write_metab_config", 
#'   site=list_sites()[24:33], filename=NULL,
#'   omit_incomplete=FALSE)
#' stage_metab_config(tag="0.0.1", strategy="test write_metab_config", 
#'   site=list_sites()[24:33], filename=NULL)
#' styxsites <- c("styx_001001","styx_001002","styx_001003")
#' mc <- stage_metab_config(tag="0.0.1", strategy="test styx config", 
#'   model="metab_sim", site=styxsites, filename=NULL, 
#'   doobs=choose_data_source("doobs", styxsites, logic="unused var"), omit_incomplete=FALSE)
#' }
stage_metab_config <- function(
  tag, strategy, date=Sys.time(), 
  model="metab_mle", model_args="list()",
  site=list_sites(c("doobs_nwis","disch_nwis","wtr_nwis")), 
  sitetime=choose_data_source("sitetime", site),
  doobs=choose_data_source("doobs", site),
  dosat=choose_data_source("dosat", site),
  depth=choose_data_source("depth", site),
  wtr=choose_data_source("wtr", site),
  par=choose_data_source("par", site),
  disch=choose_data_source("disch", site, logic="unused var"),
  veloc=choose_data_source("veloc", site, logic="unused var"),
  sitedate=choose_data_source("sitedate", site, logic="unused var"),
  doinit=choose_data_source("doobs1", site, logic="unused var"),
  gpp=choose_data_source("gpp", site, logic="unused var"),
  er=choose_data_source("er", site, logic="unused var"),
  K600=choose_data_source("K600", site, logic="unused var"),
  K600lwr=choose_data_source("K600lwr", site, logic="unused var"),
  K600upr=choose_data_source("K600upr", site, logic="unused var"),
  dischdaily=choose_data_source("dischdaily", site, logic="unused var"),
  velocdaily=choose_data_source("velocdaily", site, logic="unused var"),
  start_date=NA, end_date=NA,
  omit_incomplete=TRUE,
  filename="./config.tsv") {
  
  # Create the config table
  config <- data.frame(
    tag=tag, strategy=strategy, date=as.character(date, format="%Y-%m-%d %H:%M:%S %z"), 
    model=model, model_args=model_args,
    site=site, 
    sitetime=sitetime, 
    doobs=doobs, dosat=dosat, depth=depth, wtr=wtr, par=par, disch=disch, veloc=veloc,
    sitedate=sitedate, doinit=doinit, gpp=gpp, er=er, K600=K600, K600lwr=K600lwr, K600upr=K600upr, 
    dischdaily=dischdaily, velocdaily=velocdaily,
    start_date=as.POSIXct(start_date, tz="UTC"), end_date=as.POSIXct(end_date, tz="UTC"),
    stringsAsFactors=FALSE)
  
  # Filter to only those rows that might work
  if(omit_incomplete) {
    incomplete <- sapply(1:nrow(config), function(row) {
      metab_fun <- config[row, "model"]
      # get a list of vars for which we expect complete info
      arg_data <- eval(formals(metab_fun)$data)
      arg_data_daily <- eval(formals(metab_fun)$data_daily)
      needs_data <- if(attr(arg_data,'optional')[1]=='all') NULL else colnames(arg_data)[!(colnames(arg_data) %in% attr(arg_data,'optional'))]
      needs_data_daily <- if(attr(arg_data_daily,'optional')[1]=='all') NULL else colnames(arg_data_daily)[!(colnames(arg_data_daily) %in% attr(arg_data_daily,'optional'))]
      data_needs <- c(needs_data, needs_data_daily)
      var_lookup <- unique(get_var_src_codes(out=c("metab_var","var")))
      var_needs <- var_lookup[match(data_needs, var_lookup$metab_var),"var"]
      
      # determine whether we have a specified src for each
      unmet_needs <- is.na(config[row,paste0(var_needs, ".src")])
      any(unmet_needs)
    })
    config <- config[!incomplete,]
  }  
  
  # Add a row index; this could go out of date if the user modifies the config
  # file, but better than relying on fragile rownames
  config$config.row <- seq_len(nrow(config))
  
  # Write the table to file if requested
  if(!is.null(filename)) {
    write_config(config, filename)
    return(filename)
  } else {
    return(config)
  }
}


