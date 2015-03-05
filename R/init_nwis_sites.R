#'@title generate nwis site_ids for given p_codes
#'@description finds all NWIS sites that meet given data requirements
#'
#'@param p_codes a character vector of NWIS p_codes
#'@param stateCd character vector of state codes
#'@return a character vector of NWIS sites, appended with 'nwis_'
#'
#'@examples
#'@import dplyr
#'@import dataRetrieval
#'\dontrun{
#'init_nwis_sites(p_codes = c('00095', '00060', '00010', '00300'), stateCd="wi")
#'}
#'@import dataRetrieval
#'@export
init_nwis_sites <- function(p_codes, stateCd){
  
  # for loop of stateCd's
  
  sitesAll <- dataRetrieval::readNWISdata(parameterCd=p_codes, stateCd=stateCd, 
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