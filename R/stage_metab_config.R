#' Write a table (tsv) of configuration information for individual metabolism 
#' modeling jobs (one row/job per site-strategy combination). This tsv should 
#' reflect the full information needed to re-run a set of jobs. The jobs will 
#' probably, but not necessarily, be run on a Condor cluster.
#' 
#' @param tag character of form "1.0.2" that uniquely identifies this set of 
#'   modeling runs.
#' @param strategy character, or vector of length sites, describing this set of 
#'   modeling runs in concise English.
#' @param site site names
#' @param doobs 2-column data.frame with names "type" and "src" describing where DO data should come from
#' @param disch 2-column data.frame with names "type" and "src" describing where discharge data should come from
#' @param wtr 2-column data.frame with names "type" and "src" describing where water temperature data should come from
#' @param filename character or NULL. If NULL, the function returns a data.frame, 
#'   otherwise it writes that data.frame to the file specified by filename.
#' @return file name of the config file
#' @import dplyr
#' @export
#' @examples
#' head(stage_metab_config(tag="0.0.1", strategy="test write_metab_config", filename=NULL))
#' stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", site="nwis_04087142", filename=NULL)
stage_metab_config <- function(
  tag, strategy, 
  model="metab_simple",
  site=choose_sites(c("doobs","disch","wtr")), 
  doobs=choose_data_source("doobs", site),
  disch=choose_data_source("disch", site),
  wtr=choose_data_source("wtr", site),
  filename="./condor_config.tsv") {
  
  # Create the config table
  config <- data.frame(tag=tag, strategy=strategy, date=Sys.time(), 
                       model=model, site=site, doobs=doobs, disch=disch, wtr=wtr)
  
  # Write the table to file if requested
  if(!is.null(filename)) {
    write.table(config, file=filename, sep=pkg.env$ts_delim, row.names=FALSE)
  } else {
    return(config)
  }
}


