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
  
  # convert numerics to numeric
  type <- parse_meta_path(file)$type
  numcols <- switch(
    type,
    'basic'=c("lat","lon","alt"),
    'metabinput'=c("num_dates","num_rows","num_complete","modal_timestep","num_modal_timesteps"),
    'dvqcoefs'=c("c","f"),
    c() # default for unknown metadata table is not to convert anything
  )
  for(col in numcols) {
    df[,col] <- u(as.numeric(df[,col]), get_units(df[,col]))
  }
  
  return(df)
}

