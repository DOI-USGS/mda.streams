#' Search within a data.frame for text matches to the constraints
#' 
#' Wrapper for grepl
#' 
#' @param constraints named vector of the constraints, e.g., 
#'   c(site_name='nwis_0806'), where the names are column names in data.frame. 
#'   constraints with NA values will be excluded
#' @param match_case logical. should the case be matched exactly?
#' @param fixed logical. As in \code{\link{grepl}}, TRUE to require an exact
#'   match and FALSE to use regular expressions
#' @return vector of T/F values where T indicates that the row of the dataframe 
#'   is matched.
#' @keywords internal
search_dataframe <- function(dataframe, constraints, match_case, fixed) {
  constraints <- constraints[!is.na(constraints)]
  matches <- sapply(seq_along(constraints), function(cnum) {
    constraint <- constraints[cnum]
    if(match_case) {
      grepl(constraint, dataframe[[names(constraint)]], fixed=fixed)  
    } else {
      grepl(tolower(constraint), tolower(dataframe[[names(constraint)]]), fixed=fixed)      
    }
  }) %>% apply(MARGIN=1, FUN=all)
  
}
