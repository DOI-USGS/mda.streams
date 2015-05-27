#'@title get project codes
#'@param shortname a valid shortname for project (e.g., 'disch')
#'@param with.longname boolean for whether to include longnames in output
#'@examples
#'get_var_codes('wtr')
#'get_var_codes('disch', TRUE)
#'@export
get_var_codes <- function(shortname, with.longname = FALSE){
  
  codes <- list('doobs' = 
                  c(p_code = '00300', longname = 'dissolved oxygen concentration'),
                'wtr' = 
                  c(p_code = '00010', longname = 'water temperature'),
                'disch' = 
                  c(p_code = '00061', longname = 'instantaneous discharge'),
                'stage' = 
                  c(p_code = '00065', longname = 'gage height'),
                'par' = 
                  c(p_code = '99988', longname = 'photosynthetically active radiation'),
                'baro' =
                  c(p_code = 'pressfc', longname = 'barometric pressure'),
                'sw' = 
                  c(p_code = 'dswrfsfc', longname = 'downwelling shortwave radiation'))

  if (missing(shortname))
    return(codes)
  
  if (!shortname %in% names(codes))
    stop (shortname, ' not found in ', paste(names(codes), collapse = ', '))
  
  if (with.longname){
    return(codes[[shortname]])
  } else {
    return(codes[[shortname]][['p_code']])
  }

}