#' Translate a variable and data source into a var_src
#' 
#' @param var a variable shortname from
#'   \code{unique(get_var_src_codes(out='var'))}, e.g., "doobs"
#' @param src a data source from the sources available for the given var; e.g., 
#'   see \code{get_var_src_codes(var=='doobs', out='src')}
#' @export
make_var_src <- function(var, src) {
  paste0(var, "_", src)
}

#' Check whether a var_src is listed in get_var_src_codes()
#' 
#' @param var_var_src may be either a variable, e.g. "doobs", or a var_src, e.g.
#'   "doobs_nwis".
#' @param src a data source; optional if var_var_src is specified with both 
#'   variable and source
#' @param on_fail the function to apply to an information message (character) if
#'   the var_src is not valid
#' @return a single TRUE if successful, else on_fail(msg) followed by a vector
#'   of T/F values with F for invalid var_srces
#' @importFrom stats setNames
#' @export
verify_var_src <- function(var_var_src, src, on_fail=warning) {
  # combine var & src
  var_src <- if(!missing(src)) paste0(var_var_src, "_", src) else var_var_src
  var_src_p <- parse_var_src(var_src)
  
  # prepare to collect info
  valid <- rep(TRUE, nrow(var_src_p))

  # check that the var_src combo is known
  if(any(missing_var_src <- !(var_src %in% get_var_src_codes()$var_src))) {
    msg <- paste0("unrecognized var_src",if(length(which(missing_var_src))>1) "es" else "",": ", paste0(var_src[missing_var_src], collapse=", "))
    on_fail(msg)
    valid <- valid & !missing_var_src
  }
  
  # check that the var is known
  if(any(missing_var <- !(var_src_p$var %in% get_var_src_codes()$var))) {
    msg <- paste0("unrecognized var",if(length(which(missing_var))>1) "s" else "",": ", paste0(var_src_p$var[missing_var], collapse=", "))
    on_fail(msg)
    valid <- valid & !missing_var
  }

  # check that the var is known
  if(any(missing_src <- !(var_src_p$src %in% get_var_src_codes()$src))) {
    msg <- paste0("unrecognized src",if(length(which(missing_src))>1) "s" else "",": ", paste0(var_src_p$src[missing_src], collapse=", "))
    on_fail(msg)
    valid <- valid & !missing_src
  }
  
  if(all(valid)) {
    TRUE 
  } else {
    setNames(valid, var_src)
  } 
}

#' Translate a var_src into a variable and a source
#' 
#' @param var_src an underscore-separated combination of a variable and its data
#'   source
#' @param out the parsed fields to return.
#' @param use_names logical. Should names/rownames be attached to the
#'   vector/data.frame?
#' @return if length(out)==1 a vector, else a data.frame
#' @importFrom stats setNames
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
#' @importFrom stats setNames
#' @export
parse_ts_name <- function(ts_name, out="var_src", use_names=length(ts_name)>1) {
  # error checking
  splitcols <- c("var_src", "var", "src")
  if(any(non_ts <- substr(ts_name, 1, 3) != pkg.env$ts_prefix)) 
    stop("unexpected ts variable prefix in: ", paste0("[", which(non_ts), "] ", ts_name[non_ts], collapse=", "))
  if(any(non_out <- !(out %in% splitcols)))
    stop("unexpected output requested: ", paste0(out[non_out]))
  var_src <- substring(ts_name, 4)
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
#' 
#' @param sitenum integer or character, coercible to character, representing the
#'   site code as used by the database.
#' @param database a character or character vector of databases from which the 
#'   site ID is derived, probably \code{"nwis"} (from the USGS NWIS database) or
#'   \code{"styx"} (made-up data) or \code{"indy"} (a real site independent of
#'   NWIS or other large networks).
#' @return site ID in ScienceBase and mda.streams lingo
#' @export
make_site_name <- function(sitenum, database=c("nwis", "styx", "indy")) {
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
#' @importFrom stats setNames
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
#' @importFrom stats setNames
#' @export
parse_ts_path <- function(file_path, 
                          out=c("dir_name","file_name","site_name","ts_name","var_src","var","src","database","sitenum"), 
                          use_names=length(file_path)>1) {
  
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
#' @param type the type of metadata the file will include
#' @param folder the folder to write the file in, or missing
#' @return a full file path
#' @export
make_meta_path <- function(type, folder) {
  # don't check the input. we can have any old meta type we want.
  # type <- match.arg(type)
  
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
#' @param use_names logical. Should names be attached to the data.frame rows or
#'   list elements?
#' @return a data.frame, one row per path
#' @export
#' @importFrom stats setNames
parse_meta_path <- function(file_path, out=c("dir_name","file_name","type","meta_type"), use_names=length(file_path)>1) {
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

#' Combine the date, tag, and strategy of a model_run into a title
#' 
#' @param date text date, e.g. 070802
#' @param tag text tag, e.g., 0.0.3
#' @param strategy text strategy, e.g., nighttime_k
#' @export
make_metab_run_title <- function(date, tag, strategy) {
  paste(date, tag, strategy, sep=" ")
}

#' Parse the title of a metab_model run
#' 
#' @param title the model run title
#' @param out the columns to return
#' @param use_names logical. Should names be attached to the data.frame rows or
#'   list elements?
#' @return a data.frame, one row per path
#' @import dplyr
#' @importFrom stats setNames
#' @export
parse_metab_run_title <- function(title, out=c('date','tag','strategy'), use_names=length(title)>1) {
  
  out = match.arg(out, several.ok=TRUE)
  
  split_spaces <- gregexpr(' ', title, fixed=TRUE)
  split_1 <- split_2 <- '.dplyr.var'
  parsed <- 
    data.frame(
      title = title,
      split_1 = sapply(split_spaces, '[', 1),
      split_2 = sapply(split_spaces, '[', 2),
      stringsAsFactors=FALSE) %>%
    mutate(
      date = substr(title, 1, split_1-1),
      tag = substr(title, split_1+1, split_2-1),
      strategy = substr(title, split_2+1, nchar(title)))
  
  parsed <- parsed[,out]
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- title; .}) 
      } else {
        parsed %>% setNames(title)
      }
  }
  parsed
}

#' Produce a name unique to a single metab_model
#' 
#' @param title metab_run title
#' @param row the config row number == metab_all.RData element number
#' @param site the site name
#' @export
make_metab_model_name <- function(title, row, site) {
  tryCatch(as.numeric(row), warning=function(w) warning("can't convert row to numeric"))
  sprintf('%s-%s-%s', site, as.character(row), title)
}

#' Parse a metab_model name
#' 
#' @param model_name the unique name of the metab_model as produced by 
#'   make_metab_model_name
#' @param out the columns to return
#' @param use_names logical. Should names be attached to the data.frame rows or 
#'   list elements?
#' @import dplyr
#' @export
#' @importFrom stats setNames
parse_metab_model_name <- function(model_name, out=c('title','row','site','date','tag','strategy'), use_names=length(model_name)>1) {
  
  out = match.arg(out, several.ok=TRUE)
  
  split_scores <- gregexpr('-', model_name, fixed=TRUE)
  split_1 <- split_2 <- '.dplyr.var'
  parsed <- 
    data.frame(
      model_name = model_name,
      split_1 = sapply(split_scores, '[', 1),
      split_2 = sapply(split_scores, '[', 2),
      stringsAsFactors=FALSE) %>%
    mutate(
      site = substr(model_name, 1, split_1-1),
      row = as.numeric(substr(model_name, split_1+1, split_2-1)),
      title = substr(model_name, split_2+1, nchar(model_name))) %>%
    bind_cols(parse_metab_run_title(.$title, out=c('date','tag','strategy'), use_names=FALSE)) %>%
    as.data.frame()
  
  parsed <- parsed[,out]
  if(use_names) {
    parsed <- 
      if(is.data.frame(parsed)) {
        parsed %>% do({rownames(.) <- model_name; .}) 
      } else {
        parsed %>% setNames(model_name)
      }
  }
  parsed
}

#' Combine the model_name and folder into a file path for a metab_model
#' 
#' @param model_name the name of the model as from make_metab_model_name()
#' @param folder the folder of the model file
#' @param version character indicating whether you want the original metab_model
#'   or a modernized one that works with the current streamMetabolizer version
#' @export
make_metab_model_path <- function(model_name, folder, version=c('original','modern')) {
  # don't check; permit new items
  version <- match.arg(version)
  
  # create path
  file_name <- sprintf('%s%s%s.%s', if(version=='modern') 'm' else '', pkg.env$metab_model_prefix, model_name, pkg.env$metab_model_extension)
  if(missing(folder)) {
    file.path(file_name) # pretty sure this does absolutely nothing (besides not break)
  } else {
    file.path(folder, file_name)
  }
  
}

#' Parse a file path into metab_model information
#' 
#' @param file_path the file path
#' @param out character. which columns to return
#' @param use_names attach row/vector names?
#' @import dplyr
#' @importFrom stats setNames
#' @export
parse_metab_model_path <- function(file_path, out=c("dir_name","file_name","model_name","title","row","site",'date','tag','strategy','version'), use_names=length(file_path)>1) {
  out = match.arg(out, several.ok=TRUE)
  
  dir_name <- sapply(file_path, dirname, USE.NAMES=FALSE)
  file_name <- sapply(file_path, basename, USE.NAMES=FALSE)
  
  if(substring(file_path, 1, 3) == "mmm") {
    version='modern'
    file_path <- substring(file_path, 1)
  } else {
    version='original'
  }
  parsed <- data.frame(
    dir_name = dir_name,
    file_name = file_name, 
    model_name = substr(file_name, nchar(pkg.env$metab_model_prefix)+1, nchar(file_name)-1-nchar(pkg.env$metab_model_extension)),
    version = version,
    stringsAsFactors=FALSE)
  parsed <- parsed %>%
    bind_cols(parse_metab_model_name(parsed$model_name, out=c("title","row","site",'date','tag','strategy','version'), use_names=FALSE)) %>%
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