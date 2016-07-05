#' Search the var_src_codes
#'
#' @param var text to search for in the var column
#' @param src text to search for in the src column
#' @param var_src text to search for in the var_src column
#' @param units text to search for in the units column
#' @param var_descrip text to search for in the var_descrip column
#' @param src_descrip text to search for in the src_descrip column
#' @param data_type text to search for in the data_type column
#' @param p_code text to search for in the p_code column
#' @param metab_var text to search for in the metab_var column
#' @param priority text to search for in the priority column
#' @param var_src_codes table of var_src_codes to search
#' @inheritParams search_dataframe
#' @import dplyr
#' @import tibble
#' @examples 
#' search_var_src_codes(units='mgO2 L^-1')
#' @export
search_var_src_codes <- function(
  var=NA, src=NA, var_src=NA, units=NA, var_descrip=NA, src_descrip=NA, data_type=NA, p_code=NA, metab_var=NA, priority=NA,
  var_src_codes=get_var_src_codes(), match_case=FALSE, fixed=TRUE) {
  
  . <- '.dplyr.var'
  constraints <- 
    c('var','src','var_src','units','var_descrip','src_descrip','data_type','p_code','metab_var','priority') %>% setNames(.,.) %>%
    sapply(function(x) eval(as.symbol(x)))
  
  matches <- search_dataframe(var_src_codes, constraints, match_case=match_case, fixed=fixed)
  
  var_src_codes[matches, ]
}