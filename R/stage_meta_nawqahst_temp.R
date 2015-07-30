#' Import data that Ted has acquired from the Accumulated and Reach Catchment 
#' Land Variables dataset
#' 
#' This is a temporary step toward the long-term goal of having every data 
#' import step fully scripted. In the meantime, see 
#' stream_metab_usa/starter_files/data_scripts for what Ted emailed to Alison in
#' July 2015.
#' 
#' @param types the metadata table to be acquired
#' @param folder the folder in which to store the formatted table destined for
#'   our SB directory
#' @import dplyr
#' @importFrom unitted v u
#' @export
stage_meta_nawqahst_temp <- function(types=c('climate','hydrol','soil'), folder=tempdir()) {
  
  # authenticate SB - access is needed to download the data
  if(is.null(current_session())) stop("session is NULL. call sbtools::authenticate_sb() before querying metab_runs")
  
  # create the folder if it doesn't exist
  if(!dir.exists(folder)) dir.create(folder, showWarnings=TRUE)
  
  # get our list of NHDPlus ComIDs
  . <- site_name <- nhdplus_id <- '.dplyr.var'
  site_ids <- get_meta('basic') %>%
    #.[.$nhdplus_id_confidence %in% c('MOST_SURE','MAYBE2','MAYBE1'),] %>%
    select(site_name, nhdplus_id) %>%
    mutate(nhdplus_id = as.numeric(nhdplus_id))
  
  # read, munge, and write Ted's tables
  dests <- sapply(types, function(type) {
    
    # read Ted's table
    ted_file <- paste0('../stream_metab_usa/starter_files/data_scripts/pow',toupper(substr(type, 1, 1)), substring(type, 2),'.csv')
    ted_df <- read.table(ted_file, header=TRUE, sep=",", stringsAsFactors=FALSE)

    # rename site_name column
    ted_df <- rename(ted_df, site_name=site_no)

    # compare the site IDs against the ones on SB
    sites_wo_data <- setdiff(site_ids$site_name, ted_df$site_name)
    if(length(sites_wo_data)>0) message("sites on SB without data in ted_df: ", paste0("'",sites_wo_data,"'",collapse=","))
    data_wo_sites <- setdiff(ted_df$site_name, site_ids$site_name)
    if(length(data_wo_sites)>0) message("sites in ted_df without data on SB: ", paste0("'",data_wo_sites,"'",collapse=","))
    # compare the com IDs against the ones on SB
    sb_ids <- sort(paste(site_ids$site_name,site_ids$nhdplus_id)[site_ids$site_name %in% ted_df$site_name])
    td_ids <- sort(paste(ted_df$site_name,ifelse(ted_df$COMID==0,NA,ted_df$COMID))[ted_df$site_name %in% site_ids$site_name])
    mismatches <- which(sb_ids != td_ids)
    if(length(mismatches)>0) message('site-comid disagreements: ', paste0("(SB=",sb_ids[mismatches],",Ted=",td_ids[mismatches],")", collapse="; "))
    
    # remove comid because it's redundant
    ted_df <- ted_df[!(names(ted_df) %in% c('COMID','COMID_CONFIDENCE'))]
    
    # add units
    #need units data from ted before this is possible to do right
    ted_df <- u(ted_df)
    
    # write & return path
    dest <- paste0(folder,"/meta_",type,".tsv")
    write_unitted(ted_df, file=dest)
    dest
  })
  
  # return
  dests
}