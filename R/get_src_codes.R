#' Get source codes used in this project
#' 
#' A lookup for parameter codes and long_names based on the var 
#'   convention used in mda.streams
#'
#' @param src a valid source name for project (e.g., 'nwis')
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
#' @examples
#' get_src_codes()
#' get_src_codes('nwis')
#' get_src_codes(out=c('r_function'), type='calc')
#' @export
get_src_codes <- function(src, out=names(src_codes), type=c("data","calc","sim"), drop=(length(out) == 1), use_names) {
  
  # take a guess for use_names
  if(missing(use_names)) {
    use_names <- 
      length(out)==1 & 
      !(out[1] %in% "src") &
      (if(missing(src)) TRUE else length(src)>1)
  } 
  
  # filter by var and type
  out_rows <- which(src_codes$type %in% type)
  if (!missing(src)) {
    if (any(missing_srcs <- !(src %in% src_codes$src[out_rows]))) {
      stop (paste0(src[missing_srcs], collapse=' & '), ' not found in c(', paste(src_codes$src[out_rows], collapse = ', '), ')')
    }
    src_matches <- match(src, src_codes$src)
    out_rows <- src_matches[src_matches %in% out_rows]
  }
  codes <- src_codes[out_rows, ]
  
  # prepare names
  if(use_names) codes_names <- codes$src
  
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