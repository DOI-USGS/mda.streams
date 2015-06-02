#' @title get project codes
#' @description a lookup for parameter codes and longnames based on the 
#'   shortname convention used in mda.streams
#' @param shortname a valid shortname for project (e.g., 'disch')
#' @param out character or character vector of name types to include in output. 
#'   If there is only one, the result will be a vector/value. One or more of 
#'   these options may be selected: 
#'   \code{c("shortname","p_code","sm_name","longname")} (where \code{shortname}
#'   is also the rowname)
#' @param drop logical. In data.frame subsetting, should a single column be
#'   dropped to a vector or a single row be dropped to a list?
#' @return vector or data.frame of the requested variables and outnames. 
#'   \code{shortname} is the name used in mda.streams. \code{p_code} is the 
#'   corresponding code in the NWIS or NLDAS databases. \code{sm_name} is the 
#'   name used in streamMetabolizer. \code{longtime} is the human-readable 
#'   description.
#' @examples
#' get_var_codes()
#' get_var_codes('wtr')
#' get_var_codes('disch', 'p_code')
#' get_var_codes('baro', c('longname','p_code'))
#' @export
get_var_codes <- function(shortname, out=c("p_code","sm_name","longname"), drop=TRUE){
  
  codes <- matrix(
    data=
      c('doobs', '00300',   'DO.obs',     'dissolved oxygen concentration',
        'dosat',  NA,       'DO.sat',     'hypothetical dissolved oxygen concentration at saturation',
        'wtr',   '00010',   'temp.water', 'water temperature',
        'disch', '00061',    NA,          'instantaneous discharge',
        'stage', '00065',    NA,          'gage height',
        'depth',  NA,       'depth',      'stream depth',
        'par',   '99988',   'light',      'photosynthetically active radiation',
        'baro',  'pressfc',  NA,          'barometric pressure',
        'sw',    'dswrfsfc', NA,          'downwelling shortwave radiation'), 
    ncol=4, byrow=TRUE,
    dimnames=list(NULL, c("shortname","p_code","sm_name","longname"))) %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    do({rownames(.) <- .$shortname; .})
  
  if (missing(shortname))
    return(codes[ , out, drop])
  
  if (!shortname %in% rownames(codes))
    stop (shortname, ' not found in ', paste(names(codes), collapse = ', '))
  
  return(codes[shortname, out, drop])
  
}