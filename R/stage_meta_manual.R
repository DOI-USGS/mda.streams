#' Create/modify file of manually-determined site information
#' 
#' Pull in user review data from Google Spreadsheet at
#' 
#' @param folder the folder in which to save the metadata file
#' @param verbose logical. print status messages?
#' @import dplyr
#' @importFrom unitted u v get_units
#' @export
stage_meta_manual <- function(folder = tempdir(), verbose = FALSE) {
  
  # get a list of sites known to us/sciencebase
  meta_basic <- get_meta('basic')
  
  # import the Google Spreadsheet data (see function below)
  user_review <- load_google_sheet(sheets_key='1yAzDrdaztX0vh1ifpYiMurOQDPDrfqysUGdpC1HzHEA')
  
  # remove any trailing spaces
  user_review <- as.data.frame(lapply(user_review, function(col) {
    gsub("[[:space:]]*$", "", col)
  }), stringsAsFactors=FALSE)
  
  # make diel_signal uniform
  user_review$diel_signal[tolower(user_review$diel_signal) %in% c('y','yes')] <- 'yes'
  user_review$diel_signal[tolower(user_review$diel_signal) %in% c('n','no')] <- 'no'
  user_review$diel_signal[tolower(user_review$diel_signal) %in% c('maybe')] <- 'maybe'
  user_review$diel_signal[tolower(user_review$diel_signal) %in% c('sometimes')] <- 'sometimes'
  
  # make assessment uniform. but no need - already has exactly 3 unique values
  # unique(user_review$assessment)

  # check for unknown site_names
  unknown_sites <- setdiff(user_review$site_name, meta_basic$site_name)
  if(length(unknown_sites)>0) warning("omitting user reviews from these unknown sites: ", paste0(unknown_sites, collapse=", "))
  missing_sites <- setdiff(meta_basic$site_name, user_review$site_name)
  if(length(missing_sites)>0) warning("these sites have no user reviews (probably on purpose): ", paste0(missing_sites, collapse=", "))
  
  # add units
  user_review <- u(user_review, c(NA,NA,NA,"%",NA,NA,NA,NA))
  
  # remove redundant columns
  long_name <- '.dplyr_var'
  user_review <- select(user_review, -long_name)
  
  # either return the data.frame or save data to local file and return the fname
  if(is.null(folder)) {
    return(user_review)
  } else {
    fpath <- make_meta_path(type='manual', folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write_unitted(user_review, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
    close(gz_con)
    return(invisible(fpath))
  }
  
}

#' Read in data from a Google Spreadsheet
#' 
#' Data-reading function from comments in
#' http://blog.revolutionanalytics.com/2014/06/reading-data-from-the-new-version-of-google-spreadsheets.html
#' @param sheets_key the Google document key, a string of letters and numbers found in the browser URL after /d/
#' @param na.string the string to convert to NA if it's all that's in a cell
#' @param header logical. Expect a header row?
#' @keywords internal
load_google_sheet <- function(sheets_key, na.string="", header=TRUE){
  stopifnot(requireNamespace('RCurl'))
  url <- paste0('https://docs.google.com/spreadsheets/d/', sheets_key, '/export?format=tsv&id=', sheets_key)
  myCsv <- RCurl::getURL(url, .opts=list(ssl.verifypeer=FALSE))
  ret <- read.table(textConnection(myCsv), sep="\t", header=header, stringsAsFactors=FALSE, fill=TRUE, quote="\"")
  as.data.frame(lapply(ret, function(x){ x[x == na.string] <- NA; x}), stringsAsFactors=FALSE)
}
