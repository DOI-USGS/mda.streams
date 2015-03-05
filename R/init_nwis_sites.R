#'@title generate nwis site_ids for given p_codes
#'@description finds all NWIS sites that meet given data requirements
#'
#'@param p_codes a character vector of NWIS p_codes
#'@param stateCd character vector of state codes, or "all" for all data.
#'@import dplyr
#'@importFrom dataRetrieval readNWISdata
#'@return a character vector of NWIS sites, appended with 'nwis_'
#'
#'@examples
#'\dontrun{
#'sites <- init_nwis_sites(p_codes = c('00095', '00060', '00010', '00300'), 
#'                stateCd=c("wi","mi"))
#'}
#'@export
init_nwis_sites <- function(p_codes, stateCd){
  
  parm_cd <- ""
  site_no <- ""
  pcodes <- ""
  begin_date <- ""
  end_date <- ""
  
  if(any("all" == stateCd)){
    stateCd <- names(states)
  }
  # for loop of stateCd's
  sites <- c()
  for(i in stateCd){
    cat("Getting: ", i, "information\n")
    
    possibleError <- tryCatch(
      sitesAll <- dataRetrieval::readNWISdata(parameterCd=p_codes, stateCd=i, 
                                              outputDataTypeCd="iv", 
                                              seriesCatalogOutput="true",
                                              service = "site"),
      error=function(e) e
    )
    if(!inherits(possibleError, "error")){
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
                        filter(max(as.Date(begin_date)) < min(as.Date(end_date)))
        
      sites <- c(sites, unique(summaryTwo$site_no))
    } else {
      cat("NWIS doesn't have data on:", i, "\n")
    }
  }
  
  sites <- paste("nwis",sites,sep="-")
  
  return(sites)

}