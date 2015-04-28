#' Functions for managing the Continental Stream Metabolism data on ScienceBase
#' 
#' mda.streams stands for Model-Data Assimilation for Streams. We're not
#' entirely sure whether this is the appropriate name, but it has its roots in
#' good intentions.
#' 
#' For lower-level functions generic to all ScienceBase tasks, see 
#' \pkg{sbtools}. For higher-level functions that make it easy to use the Powell
#' Center project data in particular, see \pkg{powstreams}.
#' 
#' 
#' @section Logging in:
#'   
#'   Use the \code{\link{authenticate_sb}} command from \pkg{sbtools} to
#'   establish session credentials for working with ScienceBase.
#'   
#'   
#' @section Navigating ScienceBase:
#'   
#'   \itemize{
#'   
#'   \item \code{\link{get_sites}} - lists all site IDs (e.g. "nwis_04067500") 
#'   that are currently on ScienceBase
#'   
#'   \item \code{\link{get_ts_variables}} - Given a site ID, returns a list of 
#'   timeseries variables stored for that site
#'   
#'   }
#'   
#'   
#' @section Reading data from ScienceBase:
#'   
#'   \itemize{
#'   
#'   \item \code{\link{download_ts}} - given a site ID and variable name, finds 
#'   and downloads the timeseries file from SB
#'   
#'   \item \code{\link{get_watershed_WFS}} - gets watershed geospatial data
#'   
#'   \item \code{\link{get_watershed_WMS}} - gets watershed geospatial data
#'   
#'   }
#'   
#'   
#' @section Preparing data to write to ScienceBase:
#'   
#'   
#'   \itemize{
#'   
#'   \item \code{\link{init_nwis_sites}} - generate a list of site IDs that meet
#'   data availability criteria
#'   
#'   \item \code{\link{get_nwis_df}} - downloads data from NWIS to the local R
#'   session
#'   
#'   }
#'   
#'   
#' @section Writing to ScienceBase:
#'   
#'   \itemize{
#'   
#'   \item \code{\link{create_site}} - given a site ID, create an SB node for
#'   the site
#'   
#'   \item \code{\link{post_nwis_ts}} - download the specified NWIS data and
#'   post it to SB
#'   
#'   \item \code{\link{post_ts}} - given a data.frame with data, do a little
#'   format checking and post those data to SB. Where would such data come from?
#'   
#'   \item \code{\link{post_watershed}} - given a vector of watershed filenames,
#'   posts those files to SB. Where do the files come from?
#'   
#'   }
#'   
#'   
#' @section Internal functions:
#'   
#'   \itemize{
#'   
#'   \item \code{build_data.R} - creates the data needed within the package
#'   
#'   \item \code{\link{split_site}} - currently only used in get_nwis_df. Takes
#'   site ID (e.g. "nwis_2345621") and returns the part after the underscore.
#'   
#'   \item \code{mda_helpers.R} - utilities used within the package. Includes
#'   functions make_ts_variable, get_ts_prefix, get_ts_extension, get_ts_delim,
#'   and get_title.
#'   
#'   }
#'   
#' @docType package
#' @name mda.streams
NULL