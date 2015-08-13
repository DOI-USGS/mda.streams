
#' Retrieve the var and src codes for mda.streams
#' 
#' Facilitates quick filtering and selecting
#' @param ... a list of unnamed arguments to pass to \code{dplyr::filter_}
#' @param out a list of arguments to pass to \code{dplyr::select_}
#' @param drop logical. if the filtering & selection lead to a data.frame whose 
#'   dimensions would ordinarily be dropped using z[x,y] location, should the 
#'   dimensions be dropped?
#' @import dplyr
#' @importFrom lazyeval lazy_dots
#' @examples 
#' get_var_src_codes()
#' unique(get_var_src_codes(var=="baro", out='var'))
#' get_var_src_codes(var=="baro", out='var', drop=FALSE)
#' get_var_src_codes(var=="baro", out=c('var','var_descrip'))
#' get_var_src_codes(var=="baro", out=c('var','var_descrip'), drop=FALSE)
#' get_var_src_codes(out=c('var','var_src','p_code'))
#' get_var_src_codes(src_type=="data", !is.na(p_code), 
#'   out=list('var','var_src','p_code', 'src_type'))
#' @export
get_var_src_codes <- function(..., out, drop=TRUE) {
  # get the codes
  if(!exists('tsmeta_varsrccodes', envir=pkg.env)) {
    code_item <- locate_ts_meta('varsrccodes')
    code_file <- file.path(tempdir(), 'varsrccodes.tsv')
    sbtools::item_file_download(code_item, names='tsmeta_varsrccodes.tsv', destinations=code_file)
    var_src_codes <- read.table(file=code_file, header=TRUE, sep='\t', colClasses='character', stringsAsFactors=FALSE, fill=TRUE, quote="\"")
    assign(x='tsmeta_varsrccodes', value=var_src_codes, envir=pkg.env)
  }
  codes <- pkg.env$tsmeta_varsrccodes
  
  # process filtering and column selection criteria
  dots=lazyeval::lazy_dots(...)
  if(length(dots) > 0)
    codes <- filter_(codes, .dots=dots)
  if(!missing(out))
    codes <- do.call(select_, c(list(codes), out))
  codes <- codes %>% as.data.frame()
  if(drop) codes[,] else codes
}
