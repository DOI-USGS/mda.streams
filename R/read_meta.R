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
  
  df <- read.table(file, sep=pkg.env$meta_delim, header=TRUE, stringsAsFactors=FALSE, colClasses="character")
  
  # convert numerics to numeric
  for(col in c("lat","lon")) {
    df[,col] <- u(as.numeric(df[,col]), NA)
  }
  
  return(df)
}

