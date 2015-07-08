#' Write a metabolism modeling configuration file
#' 
#' Write a table (tsv) of configuration information for individual metabolism 
#' modeling jobs (one row/job per site-strategy combination). This tsv should 
#' reflect the full information needed to re-run a set of jobs. The jobs will 
#' probably, but not necessarily, be run on a Condor cluster.
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
#' @param sitetime 2-column data.frame with names "type" and "src" describing 
#'   where apparent solar time data should come from
#' @param doobs 2-column data.frame with names "type" and "src" describing where
#'   DO data should come from
#' @param dosat 2-column data.frame with names "type" and "src" describing where
#'   DO saturation data should come from
#' @param depth 2-column data.frame with names "type" and "src" describing where
#'   stream depth data should come from
#' @param wtr 2-column data.frame with names "type" and "src" describing where 
#'   water temperature data should come from
#' @param par 2-column data.frame with names "type" and "src" describing where 
#'   light (photosynthetically available radiation, PAR) data should come from
#' @param omit_incomplete logical. If one or more datasets required for the
#'   specified config row is unavailable, should that row be omitted?
#' @param filename character or NULL. If NULL, the function returns a 
#'   data.frame, otherwise it writes that data.frame to the file specified by 
#'   filename.
#' @return file name of the config file
#' @import streamMetabolizer
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_01646000", filename=NULL)
#' stage_metab_config(tag="0.0.1", strategy="test write_metab_config", 
#'   site=list_sites()[24:33], filename=NULL,
#'   omit_incomplete=FALSE)
#' stage_metab_config(tag="0.0.1", strategy="test write_metab_config", 
#'   site=list_sites()[24:33], filename=NULL)
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
  omit_incomplete=TRUE,
  filename="./condor_config.tsv") {
  
  # Create the config table
  config <- data.frame(tag=tag, strategy=strategy, date=date, 
                       model=model, model_args=model_args,
                       site=site, 
                       sitetime=sitetime, doobs=doobs, 
                       dosat=dosat, depth=depth,
                       wtr=wtr, par=par,
                       stringsAsFactors=FALSE)
  
  # Filter to only those rows that might work
  if(omit_incomplete) {
    incomplete <- sapply(1:nrow(config), function(row) {
      metab_fun <- config[row, "model"]
      # get a list of vars for which we expect complete info
      data_needs <- colnames(eval(formals(metab_fun)$data))
      var_lookup <- unique(get_var_src_codes(out=c("metab_var","var")))
      var_needs <- var_lookup[match(data_needs, var_lookup$metab_var),"var"]
      
      # determine whether we have each
      unmet_needs <- is.na(config[row,paste0(var_needs, ".src")])
      any(unmet_needs)
    })
    config <- config[!incomplete,]
  }  
  
  # Write the table to file if requested
  if(!is.null(filename)) {
    write.table(config, file=filename, sep="\t", row.names=FALSE)
    return(filename)
  } else {
    return(config)
  }
}


