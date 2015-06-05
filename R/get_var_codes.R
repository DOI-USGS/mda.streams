#' Get variable codes used in this project
#' 
#' A lookup for parameter codes and long_names based on the var 
#'   convention used in mda.streams
#'
#' @import dplyr
#' 
#' @param var a valid variable name for project (e.g., 'disch')
#' @param out character or character vector of name types to include in output. 
#'   If there is only one, the result will be a vector/value. One or more of the
#'   given options may be selected
#' @param type optional filter criterion: setting type to a subset of the 
#'   default options will restrict the output to variables that have one of 
#'   those selected variable types.
#' @param drop logical. In data.frame subsetting, should a single column be 
#'   dropped to a vector or a single row be dropped to a list?
#' @param use_names logical. Should names be attached to the data.frame rows or
#'   vector elements? The default tries to be smart about what would be
#'   convenient.
#' @return vector or data.frame of the requested variables and outnames. 
#'   \code{var} is the name used in mda.streams. \code{p_code} is the 
#'   corresponding code in the NWIS or NLDAS databases. \code{metab_name} is the
#'   name used in streamMetabolizer. \code{long_name} is the human-readable 
#'   description.
#' @examples
#' get_var_codes()
#' get_var_codes('wtr')
#' get_var_codes('disch', 'p_code')
#' get_var_codes('baro', c('long_name','p_code'))
#' get_var_codes(out='var')
#' get_var_codes(out='var_src')
#' get_var_codes(out='units')
#' dplyr::filter(get_var_codes(), !is.na(metab_var))
#' @export
get_var_codes <- function(var, out=c(names(var_codes), "var_src"), type=c("ts","watershed"), drop=(length(out) == 1), use_names) {
  
  # take a guess for use_names
  if(missing(use_names)) {
    use_names <- 
      length(out)==1 & 
      !(out[1] %in% c("var","var_src")) &
      (if(missing(var)) TRUE else length(var)>1)
  } 
  
  # filter by var and type
  out_rows <- which(var_codes$type %in% type)
  if (!missing(var)) {
    if (any(missing_vars <- !(var %in% var_codes$var[out_rows]))) {
      stop (paste0(var[missing_vars], collapse=' & '), ' not found in c(', paste(var_codes$var[out_rows], collapse = ', '), ')')
    }
    var_matches <- match(var, var_codes$var)
    out_rows <- var_matches[var_matches %in% out_rows]
  }
  codes <- var_codes[out_rows, ]
  
  # expand the dataset: split the src field on '|' and make a new row for each 
  # var_src combination. by doing this here, after filtering by var and type, we
  # can provide the results with rows/elements sorted according to the
  # user-specified var argument (and still have multiple rows per var as needed)
  . <- '.dplyr.var'
  var_src_codes <- var_codes %>%
    dplyr::select(var, src) %>%
    dplyr::group_by(var) %>%
    do(with(., data.frame(var=var, src=strsplit(src, '|', fixed=TRUE)[[1]], stringsAsFactors=FALSE))) %>%
    mutate(var_src=make_var_src(var, src))
  codes <- inner_join(select(codes, -src), var_src_codes, by="var")
  if(use_names) codes_names <- codes$var_src
  
  # filter by out
  codes <- codes[, out, drop]
  
  # add names if requested, otherwise explicitly clear them after the filtering
  if(is.data.frame(codes)) {
    rownames(codes) <- if(use_names) codes_names else NULL
  } else {
    names(codes) <- if(use_names) codes_names else NULL
  }
  
  # return
  codes
}