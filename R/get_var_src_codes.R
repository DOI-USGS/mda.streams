
#' Retrieve the var and src codes for mda.streams
#' 
#' Facilitates quick filtering and selecting
#' @param filter_by a list of arguments to pass to \code{dplyr::filter_}
#' @param out a list of arguments to pass to \code{dplyr::select_}
#' @import dplyr
#' @importFrom lazyeval lazy_dots
#' @examples 
#' get_var_src_codes()
#' get_var_src_codes(var=="baro", out='var')
#' get_var_src_codes(var=="baro", out='var', drop=FALSE)
#' get_var_src_codes(var=="baro", out=c('var','var_descrip'))
#' get_var_src_codes(var=="baro", out=c('var','var_descrip'), drop=FALSE)
#' get_var_src_codes(out=c('var','var_src','p_code'))
#' get_var_src_codes(src_type=="data", !is.na(p_code), 
#'   out=list('var','var_src','p_code', 'src_type'))
#' @export
get_var_src_codes <- function(..., out, drop=TRUE) {
  codes <- var_src_codes
  dots=lazyeval::lazy_dots(...)
  if(length(dots) > 0)
    codes <- filter_(codes, .dots=dots)
  if(!missing(out))
    codes <- do.call(select_, c(list(codes), out))
  codes <- codes %>% as.data.frame()
  if(drop) codes[,] else codes
}
