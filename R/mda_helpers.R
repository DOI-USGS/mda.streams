#' Translate a variable and data source into a var_src
#' 
#' @param var a variable shortname from
#'   \code{unique(get_var_src_codes(out='var'))}, e.g., "doobs"
#' @param src a data source from the sources available for the given var; e.g., 
#'   see \code{get_var_src_codes(var=='doobs', out='src')}
#' @export
make_var_src <- function(var, src) {
  
  # error checking - make sure the specified src is an option for the specified 
  # var. don't use get_var_src_codes because that function calls this one.
  if(any(missing_src <- mapply(function(var1,src1) {
    !(src1 %in% var_src_codes[var_src_codes$var==var1, 'src'])
  }, var, src))) {
    stop("var-src mismatch[es] for ", paste0(var[missing_src], "-" ,src[missing_src], collapse=", "))
  }
  
  # name creation
  paste0(var, "_", src)
  
}

#' Translate a var_src into a variable and a source
#' 
#' @param var_src an underscore-separated combination of a variable and its data
#'   source
#' @param out the parsed fields to return.
#' @param use_names logical. Should names/rownames be attached to the
#'   vector/data.frame?
#' @return if length(out)==1 a vector, else a data.frame
#' @export
parse_var_src <- function(var_src, out=c("var","src"), use_names=FALSE) {
  splitcols <- c("var","src")
  splits <- strsplit(var_src, "_")
  parsed <- sapply(splits, function(split) split[match(out, splitcols)] )
  if(!is.null(dim(parsed))) {
    parsed <- parsed %>% t %>% as.data.frame(stringsAsFactors=FALSE) %>% setNames(splitcols[match(out, splitcols)])
  }
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- var_src; .}) 
      } else {
        parsed %>% setNames(var_src)
      }
  }
  parsed
}

#' Translate a timeseries name from mda.streams to ScienceBase
#' @param var_var_src may be either a variable, e.g. "doobs", or a var_src, e.g.
#'   "doobs_nwis".
#' @param src a data source; optional if var_var_src is specified with both
#'   variable and source
#' @return timseries name[s] in ScienceBase lingo (e.g., "\code{ts_wtr})
#' @export
make_ts_name <- function(var_var_src, src) {
  # combine var & src
  var_src <- if(!missing(src)) paste0(var_var_src, "_", src) else var_var_src
  
  # error checking
  if(any(ts_names <- substr(var_src, 1, 3) == pkg.env$ts_prefix))
    stop("variable is in ScienceBase format already: ", paste0("[", which(ts_names), "] ", var_src[ts_names], collapse=", "))
  if(any(not_two <- sapply(strsplit(var_src, "_"), function(split) length(split)!=2)))
    stop("improper var_src format: ", paste0("[", which(not_two), "] ", var_src[not_two], collapse=", "))
  if(any(non_var <- !(var_src %in% get_var_src_codes(out="var_src")))) 
    stop("var_src is not listed in get_var_src_codes(out='var_src': ", paste0("[", which(non_var), "] ", non_var[non_var], collapse=", "))
  
  # renaming
  paste0(pkg.env$ts_prefix, var_src)
}

#' Translate a timeseries name from ScienceBase to mda.streams
#' 
#' @import dplyr
#' @param ts_name timeseries name[s] in ScienceBase lingo (e.g., 
#'   \code{ts_wtr_nwis})
#' @param out character, length 1 or 2 selected from \code{c("var_src", 
#'   "var", "src")} indicating whether you want to get back the 
#'   joined variable & source, the variable, the src, or some 
#'   combination.
#' @param use_names logical. Should the return vector or data.frame be named
#'   according to the input values?
#' @return timeseries name[s]in mda.streams lingo (e.g., \code{wtr})
#' @examples 
#' mda.streams:::parse_ts_name("ts_doobs_nwis")
#' mda.streams:::parse_ts_name(c("ts_doobs_nwis", "ts_stage_nwis"))
#' mda.streams:::parse_ts_name(ts_name="ts_doobs_nwis", out=c("var_src","src"))
#' mda.streams:::parse_ts_name(c("ts_doobs_nwis", "ts_stage_nwis"), c("src","var"))
#' @export
parse_ts_name <- function(ts_name, out="var_src", use_names=length(ts_name)>1) {
  # error checking
  splitcols <- c("var_src", "var", "src")
  if(any(non_ts <- substr(ts_name, 1, 3) != pkg.env$ts_prefix)) 
    stop("unexpected ts variable prefix in: ", paste0("[", which(non_ts), "] ", ts_name[non_ts], collapse=", "))
  if(any(non_out <- !(out %in% splitcols)))
    stop("unexpected output requested: ", paste0(out[non_out]))
  var_src <- substring(ts_name, 4)
  if(any(non_var <- !(var_src %in% get_var_src_codes(out='var_src'))))
    stop("var_src isn't listed in get_var_src_codes(out='var_src'): ", paste0("[", which(non_var), "] ", var_src[non_var], collapse=", "))
  splits <- strsplit(ts_name, '_')
  if(any(not_three <- sapply(splits, function(split) length(split) != 3)))
    stop("ts_name doesn't have three parts split on '_': ", paste0("[", which(not_three), "] ", ts_name[not_three], collapse=", "))
  
  # renaming
  parsed <- do.call(rbind, splits)
  parsed[,1] <- var_src
  parsed <- parsed[,match(out, splitcols)]
  if(length(match(out, splitcols)) > 1) {
    if(is.null(dim(parsed))) parsed <- parsed %>% t() 
    parsed <- parsed %>% as.data.frame(stringsAsFactors=FALSE) %>% setNames(splitcols[match(out, splitcols)])
  }
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- ts_name; .}) 
      } else {
        parsed %>% setNames(ts_name)
      }
  }
  
  # return
  parsed
}

#' Translate a site name from database and site number to a 
#' ScienceBase+mda.streams site ID
#' @param sitenum integer or character, coercible to character, representing the
#'   site code as used by the database.
#' @param database a character or character vector of databases from which the 
#'   site ID is derived, probably \code{"nwis"} (from the USGS NWIS database) or
#'   \code{"styx"} (made-up data).
#' @return site ID in ScienceBase and mda.streams lingo
#' @export
make_site_name <- function(sitenum, database=c("nwis", "styx")) {
  # error checking
  if(missing(database)) database <- "nwis"
  expected_databases <- paste0("^", paste0(eval(formals(make_site_name)$database), collapse="|"))
  if(any(non_db <- grepl(expected_databases, as.character(sitenum))))
    stop("variable is in ScienceBase format already: ", paste0("[", which(non_db), "] ", sitenum[non_db], collapse=", "))
  expected_databases <- eval(formals(make_site_name)$database)
  if(length(odd_db <- which(!(database %in% expected_databases))) > 0) 
    stop("unexpected database: ", paste0("(",odd_db,") ", database[odd_db], collapse=" "))
  # could check sitenum against known site nums, but that would be time
  # consuming, so relying on the user's good judgement here
  
  # get the site names
  paste(database, as.character(sitenum), sep='_')
}

#' Split site name into siteID (used for NWIS site numbers)
#' @param site_name a valid ScienceBase site (e.g., "nwis_0338102")
#' @param out character, length 1 or 2 selected from 
#'   \code{c("database","sitenum")} indicating whether you want to get back the 
#'   database, the site number, or both.
#' @param use_names logical. Should the return vector be named according to the
#'   input values?
#' @return the database, sitenum, or both. If both, the return value is a 
#'   data.frame; otherwise it's a vector.
#' @import dplyr
#' @export
parse_site_name <- function(site_name, out="sitenum", use_names=length(out)>1) {
  # split first
  splitcols <- c("database","sitenum")
  splits <- strsplit(site_name, '_')
  
  # error checking
  if(any(not_two <- sapply(splits, function(split) length(split) != 2)))
    stop("site doesn't have two parts split on '_': ", paste0("[", which(not_two), "] ", site_name[not_two], collapse=", "))
  expected_databases <- eval(formals(make_site_name)$database)
  database <- sapply(splits, '[', match("database", splitcols))
  if(any(bad_db <- !(database %in% expected_databases)))
    stop("unexpected database: ", paste0("[", which(bad_db), "] ", site_name[bad_db], collapse=", "))
  
  # renaming   
  parsed <- sapply(splits, '[', match(out, splitcols))
  if(!is.null(dim(parsed))) {
    parsed <- parsed %>% t %>% as.data.frame(stringsAsFactors=FALSE) %>% setNames(splitcols[match(out, splitcols)])
  }
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- site_name; .}) 
      } else {
        parsed %>% setNames(site_name)
      }
  }
  parsed
}

#' Create a standardized file name for specific file contents
#' 
#' @param site_name the full site name, e.g., 'nwis_06893820'
#' @param ts_name the full ts name, e.g., 'ts_doobs_nwis'
#' @param folder the folder to write the file in, or missing
#' @return a full file path
#' @export
make_ts_path <- function(site_name, ts_name, folder) {
  # basic error checking - let parse_site_name and parse_ts_name return any errors
  parse_site_name(site_name)
  parse_ts_name(ts_name)
  
  file_name <- sprintf('%s-%s.%s.%s', site_name, ts_name, pkg.env$ts_extension, (gz_extension="gz"))
  if(missing(folder)) {
    file.path(file_name) # pretty sure this does absolutely nothing
  } else {
    file.path(folder, file_name)
  }
}


#' Split file path into contents
#' @import dplyr
#' @param file_path a valid file path for ts file
#' @param out a character for desired output ('dir_name','file_name',
#'   'site_name', 'ts_name', or any of the out names from parse_ts_name or
#'   parse_site_name)
#' @param use_names logical. Should the return vector be named according to the 
#'   input values?
#' @return a character
#' @export
parse_ts_path <- function(file_path, out=c("site_name","ts_name"), use_names=length(out)>1) {
  
  out = match.arg(out, several.ok=TRUE)
  
  dir_name <- sapply(file_path, dirname, USE.NAMES=FALSE)
  file_name <- sapply(file_path, basename, USE.NAMES=FALSE)
  splits <- strsplit(file_name, '[-.]')
  parsed <- data.frame(
    dir_name = dir_name,
    file_name = file_name, 
    site_name = sapply(splits, "[", 1),
    ts_name = sapply(splits, "[", 2),
    stringsAsFactors=FALSE)
  parsed <- parsed %>%
    bind_cols(parse_ts_name(parsed$ts_name, out=c("var_src", "var", "src"), use_names=FALSE)) %>%
    bind_cols(parse_site_name(parsed$site_name, out=c("database","sitenum"), use_names=FALSE)) %>%
    as.data.frame()
  
  parsed <- parsed[,out]
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- file_name; .}) 
      } else {
        parsed %>% setNames(file_name)
      }
  }
  parsed
}

#' Create a standardized file name for specific file contents
#' 
#' @param type
#' @param folder the folder to write the file in, or missing
#' @return a full file path
#' @export
make_meta_path <- function(type="basic", folder) {
  # input checking
  type <- match.arg(type)
  
  # create path
  file_name <- sprintf('meta_%s.%s.%s', type, pkg.env$meta_extension, (gz_extension="gz"))
  if(missing(folder)) {
    file.path(file_name) # pretty sure this does absolutely nothing (besides not break)
  } else {
    file.path(folder, file_name)
  }
}

#' Split a metadata file path into contents
#' 
#' @param file_path the path[s] to split
#' @param out the columns to return
#' @return a data.frame, one row per path
#' @export
parse_meta_path <- function(file_path, out=c("dir_name","file_name","type","meta_type"), use_names=length(out)>1) {
  out = match.arg(out, several.ok=TRUE)
  
  dir_name <- sapply(file_path, dirname, USE.NAMES=FALSE)
  file_name <- sapply(file_path, basename, USE.NAMES=FALSE)
  splits <- strsplit(file_name, '[_.]')
  parsed <- data.frame(
    dir_name = dir_name,
    file_name = file_name, 
    type = sapply(splits, "[", 2),
    meta_type = paste0("meta_", sapply(splits, "[", 2)),
    stringsAsFactors=FALSE)
  
  parsed <- parsed[,out]
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- file_name; .}) 
      } else {
        parsed %>% setNames(file_name)
      }
  }
  parsed
}