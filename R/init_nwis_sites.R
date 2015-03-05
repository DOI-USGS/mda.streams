#'@title generate nwis site_ids for given p_codes
#'@description finds all NWIS sites that meet given data requirements
#'
#'@param p_codes a character vector of NWIS p_codes
#'@param state_codes a character vector of state codes 
#'(e.g., \code{c("WA","WI")}). Default is all, which will use all state 
#'codes available to \code{\link{get_state()}}. Note this includes Puerto Rico and more.
#'@return a character vector of NWIS sites, appended with 'nwis_'
#'
#'@examples
#'@import dplyr
#'@import dataRetrieval
#'\dontrun{
#'init_nwis_sites(p_codes = c('00095', '00060', '00010', '00300'), state_codes="wi")
#'}
#'@importFrom dataRetrieval whatNWISdata
#'@export
init_nwis_sites <- function(p_codes, state_codes){
  
  # for loop of stateCd's
  
  sitesAll <- dataRetrieval::readNWISdata(parameterCd=p_codes, stateCd=state_codes, 
                                          outputDataTypeCd="iv", 
                                          seriesCatalogOutput="true",
                                          service = "site")
  p_codes <- p_codes[order(p_codes)]
  
  summaryTable <- filter(sitesAll, parm_cd %in% p_codes) %>%
                    group_by(site_no) %>%
                    arrange(parm_cd) %>%
                    summarize(pcodes = paste0(unique(parm_cd), collapse=",")) %>%
                    filter(pcodes==paste0(p_codes,collapse=","))
  
  summaryTwo <- filter(sitesAll, site_no %in% summaryTable$site_no) %>%
                    filter(parm_cd %in% p_codes) %>%
                    select(site_no, parm_cd, begin_date, end_date) %>%
                    group_by(site_no) %>%
                    mutate(begin_min=min(as.Date(begin_date))) %>%
                    mutate(begin_max=max(as.Date(begin_date))) %>%
                    mutate(end_min=min(as.Date(end_date))) %>%
                    mutate(end_max=max(as.Date(end_date)))   
    

}