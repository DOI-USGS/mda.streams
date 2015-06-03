#' @title generate nwis site_ids for given p_codes
#' @description finds all NWIS sites that meet given data requirements
#'   
#' @param p_codes a character vector of NWIS p_codes
#' @param state_codes character vector of state codes, or "all" for all data.
#' @param folder directory path, or NULL, indicating where to save the file or 
#'   (NULL) to return it as a character vector
#' @param verbose logical. Should status messages be given?
#' @import dplyr
#' @importFrom dataRetrieval readNWISdata
#' @return a character vector of NWIS sites, appended with 'nwis_'
#'   
#' @examples
#' \dontrun{
#' sites <- stage_nwis_sitelist(p_codes=c('00095','00060','00010','00300'), 
#'   state_codes=c("wi","mi"))
#' sites <- stage_nwis_sitelist(p_codes=get_var_codes("doobs","p_code"), 
#'   state_codes=c("all"), verbose=TRUE)
#' sites_file <- stage_nwis_sitelist(p_codes=get_var_codes("doobs","p_code"), 
#'   state_codes=c("wi"), folder=tempdir()); readLines(sites_file)
#' }
#' @export
stage_nwis_sitelist <- function(p_codes, state_codes, folder = NULL, verbose = TRUE) {
  
  parm_cd <- site_no <- pcodes <- begin_date <- end_date <- '.dplyr.var'
  
  p_codes <- unique(p_codes[order(p_codes)])
  if(isTRUE(verbose)) message("searching for sites with all of these p_codes: ", paste0(p_codes, collapse=", "))
  if("all" %in% tolower(state_codes)) {
    state_codes <- names(states)
  }

  # for loop of state_codes's
  sites <- c()
  for(state_code in state_codes){
    if(isTRUE(verbose)) message("getting ", state_code, " information")
    
    possibleError <- tryCatch(
      sitesAll <- dataRetrieval::readNWISdata(
        parameterCd=p_codes, stateCd=state_code, 
        outputDataTypeCd="iv", seriesCatalogOutput="true", service = "site"),
      error=function(e) e
    )
    if(!inherits(possibleError, "error")){
      
      # find sites that have all of the requested pcodes
      summaryTable <- filter(sitesAll, parm_cd %in% p_codes) %>%
                        group_by(site_no) %>%
                        arrange(parm_cd) %>%
                        summarize(pcodes = paste0(unique(parm_cd), collapse=",")) %>%
                        filter(pcodes==paste0(p_codes,collapse=","))    
      
      # find sites that are in summaryTable and also have data with believable dates
      summaryTwo <- filter(sitesAll, site_no %in% summaryTable$site_no) %>%
                        filter(parm_cd %in% p_codes) %>%
                        select(site_no, parm_cd, begin_date, end_date) %>%
                        group_by(site_no) %>%
                        filter(max(as.Date(begin_date)) < min(as.Date(end_date)))
        
      sites <- c(sites, unique(summaryTwo$site_no))
    } else {
      if(isTRUE(verbose)) message("NWIS doesn't have data on ", state_code)
    }
  }
  sites <- make_site_name(sites, database="nwis")
  
  if(!is.null(folder)) {
    file_handle <- file.path(folder, 'nwis_sitelist.txt')
    writeLines(sites, con=file_handle)
    return(file_handle)
  } else {
    return(sites)
  }

}