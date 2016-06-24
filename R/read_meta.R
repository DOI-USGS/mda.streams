#' Read metadata file for mda.streams into data.frame
#' 
#' @param file a valid ts file path
#' @seealso \code{\link{download_meta}}
#' @return a metadata data.frame
#' @author Alison P. Appling
#' @examples
#' \dontrun{
#' file <- download_meta(type='basic')
#' meta_basic <- read_meta(file)
#' }
#' @export
read_meta = function(file){
  if (length(file) != 1)
    stop('read_ts only supported for a single file')
  
  df <- read_unitted(file, sep=pkg.env$meta_delim, header=TRUE, stringsAsFactors=FALSE, colClasses="character")
  
  # convert to numeric almost anything that can be numeric, excluding only those
  # columns we know we want to keep as character
  type <- parse_meta_path(file)$type
  non_numeric <- switch(
    type,
    'basic'=c('site_num', 'nhdplus_id'),
    'dvqcoefs'=c(),
    'metabinput'=c('config_row','data_errors','data_warnings'),
    'nawqahst'=c('nhdplus_id'),
    c() # default for unknown metadata table is to convert anything we can
  )
  for(col in names(df)[!(names(df) %in% non_numeric)]) {
    numercol <- tryCatch(
      {as.numeric(df[[col]])},
      error=function(e) NULL,
      warning=function(w) NULL)
    if(!is.null(numercol)) {
      df[[col]] <- u(numercol, get_units(df[[col]]))
    }
  }
  
  return(df)
}

