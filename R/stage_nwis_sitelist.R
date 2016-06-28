#' @title generate nwis site_ids for given p_codes
#' @description finds all NWIS sites that meet given data requirements
#'   
#' @param vars a character vector of mda.streams var codes, as in 
#'   \code{unique(get_var_src_codes(out='var'))}
#' @param min.obs minimum number of observations of the vars that a site must 
#'   have to be accepted
#' @param site.types character vector of acceptable NWIS site types as in 
#'   http://maps.waterdata.usgs.gov/mapper/help/sitetype.html
#' @param HUCs character or numeric vector of HUC codes, e.g., 1:3 or '04'
#' @param folder directory path, or NULL, indicating where to save the file or 
#'   (NULL) to return it as a character vector
#' @param verbose logical. Should status messages be given?
#' @param p_codes optional - allows you to specify p_codes that aren't in 
#'   \code{get_var_src_codes(!is.na(p_code), out=c("var","src","p_code"))} for 
#'   exploratory purposes
#' @import dplyr
#' @importFrom dataRetrieval readNWISdata
#' @return a character vector of NWIS sites, appended with 'nwis_'
#'   
#' @examples
#' \dontrun{
#' stage_nwis_sitelist(vars=c('doobs','wtr','disch','stage'), 
#'   state_codes=c("wi","Michigan"))
#' sites <- stage_nwis_sitelist(
#'   p_codes=get_var_src_codes(var=="doobs",!is.na(p_code),out="p_code"), 
#'   state_codes=c("all"), verbose=TRUE)
#' sites_file <- stage_nwis_sitelist(
#'   p_codes=get_var_src_codes(var=="doobs",!is.na(p_code),out="p_code"), 
#'   state_codes=c("wi"), folder=tempdir()); readLines(sites_file)
#' }
#' @export
stage_nwis_sitelist <- function(vars, min.obs=100, site.types='ST', HUCs=1:21, folder = NULL, verbose = TRUE, p_codes) {
  
  # look up p_codes from vars, reconcile vars and p_codes
  if(missing(p_codes)) {
    var <- p_code <- '.dplyr_var'
    p_codes <- get_var_src_codes(var%in%vars,!is.na(p_code),out="p_code")
  } else if(!missing(vars)) {
    stop("please provide vars or p_codes but not both")
  }
  p_codes <- sort(unique(p_codes))
  if(isTRUE(verbose)) message("searching for sites with all of these p_codes: ", paste0(p_codes, collapse=", "))

  # format HUCs
  if(is.numeric(HUCs)) HUCs <- sprintf('%02.0f', HUCs)
  
  # for loop through HUCs
  parm_cd <- site_no <- count_nu <- access_cd <- site_tp_cd <- begin_date <- end_date <- '.dplyr.var'
  sites <- c()
  for(HUC in HUCs){
    if(isTRUE(verbose)) message("getting HUC ", as.numeric(HUC), " information")
    
    possibleError <- tryCatch(
      sitesAll <- dataRetrieval::readNWISdata(
        parameterCd=p_codes,
        huc=HUC,
        outputDataTypeCd="iv", # this was commented out for a while...?
        seriesCatalogOutput="true",
        service = "site"),
      error=function(e) e
    )
    if(!inherits(possibleError, "error")){
      
      # find sites that have all of the requested pcodes
      summaryTable <- sitesAll %>%
        # filtering at var-site level: only retain pcode records for pcodes we 
        # care about, that have enough observations, that are public, that are
        # for a site of the right type, and that are for a site with a
        # reasonable NWIS ID
        filter(parm_cd %in% p_codes, 
               count_nu > min.obs, 
               access_cd == '0',
               site_tp_cd %in% site.types,
               nchar(site_no) >= 8) %>%
        group_by(site_no) %>%
        # filtering at site level - pcodes need to all be present and to overlap
        # in time
        filter(all(p_codes %in% parm_cd),
               max(as.Date(begin_date)) < min(as.Date(end_date)))

      sites <- c(sites, unique(summaryTable$site_no))
    } else {
      if(isTRUE(verbose)) message("NWIS doesn't have data on HUC ", HUC)
    }
  }
  if(length(sites) == 0) return(NULL)
  sites <- make_site_name(sites, database="nwis")
  sites <- sort(sites)
  
  if(!is.null(folder)) {
    file_handle <- file.path(folder, 'nwis_sitelist.txt')
    writeLines(sites, con=file_handle)
    return(file_handle)
  } else {
    return(sites)
  }

}