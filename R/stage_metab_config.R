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
#' @param site site names
#' @param suntime 2-column data.frame with names "type" and "src" describing where
#'   apparent solar time data should come from
#' @param doobs 2-column data.frame with names "type" and "src" describing where
#'   DO data should come from
#' @param dosat 2-column data.frame with names "type" and "src" describing where
#'   DO saturation data should come from
#' @param depth 2-column data.frame with names "type" and "src" describing where
#'   stream depth data should come from
#' @param wtr 2-column data.frame with names "type" and "src" describing where
#'   water temperature data should come from
#' @param light 2-column data.frame with names "type" and "src" describing where
#'   light (PAR) data should come from
#' @param filename character or NULL. If NULL, the function returns a
#'   data.frame, otherwise it writes that data.frame to the file specified by
#'   filename.
#' @return file name of the config file
#' @import dplyr
#' @export
#' @examples
#' \dontrun{
#' head(stage_metab_config(tag="0.0.1", strategy="test write_metab_config", filename=NULL))
#' stage_metab_config(tag="0.0.1", strategy="try stage_metab_config", 
#'   site="nwis_04087142", filename=NULL)
#'   }
stage_metab_config <- function(
  tag, strategy, date=Sys.time(), 
  model="metab_mle",
  site=choose_sites(c("doobs","disch","wtr")), 
  suntime=choose_data_source("suntime", site),
  doobs=choose_data_source("doobs", site),
  dosat=choose_data_source("dosat", site),
  depth=choose_data_source("depth", site),
  wtr=choose_data_source("wtr", site),
  light=choose_data_source("light", site),
  filename="./condor_config.tsv") {
  
  # Create the config table
  config <- data.frame(tag=tag, strategy=strategy, date=date, 
                       model=model, site=site, 
                       suntime=suntime, doobs=doobs, 
                       dosat=dosat, depth=depth,
                       wtr=wtr, light=light,
                       stringsAsFactors=FALSE)
  
  # Write the table to file if requested
  if(!is.null(filename)) {
    write.table(config, file=filename, sep=pkg.env$ts_delim, row.names=FALSE)
  } else {
    return(config)
  }
}


