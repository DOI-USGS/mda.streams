#' Translate a timeseries name from mda.streams to ScienceBase
#' @param variable a timeseries name[s] in mda.streams lingo (e.g., \code{wtr})
#' @return timseries name[s] in ScienceBase lingo (e.g., "\code{ts_wtr})
make_ts_name <- function(variable){
  # error checking
  if(any(ts_names <- substr(variable, 1, 3) == pkg.env$ts_prefix))
    stop("variable is in ScienceBase format already: ", paste0("[", which(ts_names), "] ", variable[ts_names], collapse=", "))
  if(any(non_var <- !(variable %in% get_ts_variables()))) 
    stop("variable is not listed in get_ts_variables(): ", paste0("[", which(non_var), "] ", variable[non_var], collapse=", "))
  
  # renaming
  paste(pkg.env$ts_prefix, variable, sep="")
}

#' Translate a timeseries name from ScienceBase to mda.streams
#' @param ts_name timeseries name[s] in ScienceBase lingo (e.g., "\code{ts_wtr})
#' @param use_names logical. Should the return vector be named according to the input values?
#' @return timeseries name[s]in mda.streams lingo (e.g., \code{wtr})
parse_ts_name <- function(ts_name, use_names=TRUE) {
  # error checking
  if(any(non_ts <- substr(ts_name, 1, 3) != pkg.env$ts_prefix)) 
    stop("unexpected ts variable prefix in: ", paste0("[", which(non_ts), "] ", ts_name[non_ts], collapse=", "))
  
  # split
  splits <- strsplit(ts_name, '_')
  if(any(not_two <- sapply(splits, function(split) length(split) != 2)))
    stop("ts_name doesn't have two parts split on '_': ", paste0("[", which(not_two), "] ", ts_name[not_two], collapse=", "))
  
  # renaming
  varnames <- vapply(splits, '[', vector('character', length = 1), 2, USE.NAMES=use_names)
  
  # more error checking
  if(any(non_var <- !(varnames %in% get_ts_variables())))
    stop("variable isn't listed in get_ts_variables(): ", paste0("[", which(non_var), "] ", varnames[non_var], collapse=", "))
    
  # return
  varnames
}

#' Translate a site name from database and site number to a 
#' ScienceBase+mda.streams site ID
#' @param sitenum integer or character, coercible to character, representing the
#'   site code as used by the database.
#' @param database a character or character vector of databases from which the 
#'   site ID is derived, probably \code{"nwis"}.
#' @return site ID in ScienceBase and mda.streams lingo
make_site_name <- function(sitenum, database=c("nwis")) {
  # error checking
  expected_databases <- paste0("^", paste0(eval(formals(make_site_name)$database), collapse="|"))
  if(any(non_db <- grepl(expected_databases, as.character(sitenum))))
    stop("variable is in ScienceBase format already: ", paste0("[", which(non_db), "] ", sitenum[non_db], collapse=", "))
  database <- match.arg(database)
  # could check sitenum against known site nums, but that would be time
  # consuming, so relying on the user's good judgement here
  
  # get the site names
  paste(database, as.character(sitenum), sep='_')
}

#' Split site name into siteID (used for NWIS site numbers)
#' @param site a valid powstreams site (e.g., "nwis_0338102")
#' @param out character, length 1 or 2 selected from 
#'   \code{c("database","sitenum")} indicating whether you want to get back the 
#'   database, the site number, or both.
#' @param use_names logical. Should the return vector be named according to the
#'   input values?
#' @return the database, sitenum, or both. If both, the return value is a 
#'   data.frame; otherwise it's a vector.
#' @import dplyr
parse_site_name <- function(site, out="sitenum", use_names=length(out)>1){
  # split first
  splitcols <- c("database","sitenum")
  splits <- strsplit(site, '_')
  
  # error checking
  if(any(not_two <- sapply(splits, function(split) length(split) != 2)))
    stop("site doesn't have two parts split on '_': ", paste0("[", which(not_two), "] ", site[not_two], collapse=", "))
  expected_databases <- eval(formals(make_site_name)$database)
  database <- sapply(splits, '[', match("database", splitcols))
  if(any(bad_db <- !(database %in% expected_databases)))
    stop("unexpected database: ", paste0("[", which(bad_db), "] ", site[bad_db], collapse=", "))
  
  # renaming   
  parsed <- sapply(splits, '[', match(out, splitcols))
  if(!is.null(dim(parsed))) {
    parsed <- parsed %>% t %>% as.data.frame(stringsAsFactors=FALSE) %>% setNames(splitcols)
  }
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- site; .}) 
      } else {
        parsed %>% setNames(site)
      }
  }
  parsed
}

#' Split file path into contents
#' @param file_path a valid file path for ts file
#' @param out a character for desired output ('variable','site', or 'file_name')
#' @return a character
parse_ts_path <- function(file_path, out = "variable"){
  
  
  base_file <- basename(file_path)
  pieces <- strsplit(base_file, '[-.]')
  site <- pieces[[1]][1]
  variable = parse_ts_name(pieces[[1]][2])
  out_values <- c(file_name = base_file,
                    site = site,
                    variable = variable)
  
  return(as.character(sapply(out, function(x)out_values[[x]])))
  
}