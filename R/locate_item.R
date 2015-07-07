#' Find a folder or other item on ScienceBase
#' 
#' locate_item is case insensitive. This function is only guaranteed to return 
#' the first match; others may exist and may or may not be returned.
#' 
#' @param key the item key, probably identical to the folder name. Examples are 
#'   c("nwis_02322688", "project", "presentations", "proposals", "publications",
#'   "sites")
#' @param type the item type, probably either root or site_root. Examples are 
#'   c("site_root","root")
#' @param format character indicating whether the folder should be returned as 
#'   an ID or as a full URL
#' @param by character indicating how to search for the item: using tags ("tag",
#'   the default and recommended option), by scanning the parent directory for 
#'   the desired title ("dir"), or both in combination ("either")?
#' @param parent the ScienceBase ID of the parent whose children to search if 
#'   \code{by \%in\% c("dir","either")}
#' @param title the title to seek among the children if \code{by \%in\% 
#'   c("dir","either")}
#' @param limit number of items to return, as in 
#'   \code{\link[sbtools]{query_item_identifier}} or 
#'   \code{\link[sbtools]{item_list_children}}
#' @param browser logical. Should the URL be opened in a browser?
#' @import dplyr
#' @import sbtools
#' @examples 
#' \dontrun{
#' mda.streams:::locate_item(key="sites", type="root")
#' mda.streams:::locate_item(key=c("sites","proposals","publications"), 
#'   type="root", format="folder_url")
#' mda.streams:::locate_item(key="nwis_02322688", type="site_root")
#' }
locate_item <- function(key, type, format=c("id","item_url","folder_url"), 
                        by=c("tag","dir","either"), parent, title, limit=5000, 
                        browser=(format %in% c("item_url","folder_url"))) {
  
  # process arguments
  format <- match.arg(format) # only one format per call to locate_item
  by <- match.arg(by)
  query_args <- 
    switch(
      by,
      tag=data.frame(key=key, type=type, stringsAsFactors=FALSE),
      dir=data.frame(parent=parent, title=title, stringsAsFactors=FALSE),
      either=data.frame(key=key, type=type, parent=parent, title=title, stringsAsFactors=FALSE)
    ) %>%
    as.data.frame(stringsAsFactors=FALSE)

  if(get_scheme() == 'mda_streams_dev' && is.null(current_session())) 
    stop("log in to use mda_streams_dev. see authenticate_sb()")

  # run the query or queries
  if(by %in% c("tag","either")) {
    items <- bind_rows(lapply(1:nrow(query_args), function(argnum) {
      # query by tag
      item <- query_item_identifier(scheme = get_scheme(), type = query_args$type[argnum], key = query_args$key[argnum], limit=limit)
      # create a df of NAs if item wasn't found
      if(ncol(item) == 0) item <- data.frame(title=NA, id=NA)
      # attach argnum and return
      data.frame(query_args[argnum,], argnum=argnum, setNames(item, c("item.title","id")))
    })) %>% as.data.frame(stringsAsFactors=FALSE)
  } else if(by == "dir") {
    # just set up df so that the next block will do the search
    items <- data.frame(query_args, argnum=1:nrow(query_args), item.title=NA, id=NA)
  }
  
  # query by title as requested+needed
  if(by %in% c("dir","either")) {
    needy_argnums <- unique(items[is.na(items$id), "argnum"])
    for(i in needy_argnums) {
      query <- items[items$argnum == i, ]
      query_out <- query_item_in_folder(text=query$title[1], folder=query$parent[1], limit=limit)
      query_out <- query_out[tolower(query_out$title) == tolower(query$title[1]), ]
      if(nrow(query_out) > 0) {
        query_out$parentId <- sapply(query_out$id, function(id) {item_get_fields(id, "parentId")})
        query_match <- query_out[query_out$parentId == query$parent[1], ]
        if(nrow(query_match) > 0) {
          items[items$argnum == i, "id"] <- query_match$id
          items[items$argnum == i, "title"] <- query_match$title
        }
      }
    }
  }
  
  # reformat the results to our liking
  sapply(1:nrow(items), function(item) {
    format_item(items[item,], format, browser)
  })
}

#' Format an item as a ScienceBase ID or URL, as requested
#' 
#' Internal helper to locate_item. Doesn't check the value of format - that's up
#' to the calling functions to do, for efficiency.
#' 
#' @param item a ScienceBase item data.frame, e.g., as returned from 
#'   query_item_identifier
#' @inheritParams locate_item
#' @import httr
#' @keywords internal
format_item <- function(item, format, browser) {
  out <- if(nrow(item) == 0) {
    NA
  } else {
    switch(
      format,
      id=item$id,
      item_url=paste0("https://www.sciencebase.gov/catalog/item/", item$id),
      folder_url=paste0("https://www.sciencebase.gov/catalog/folder/", item$id))
  }
  if(browser) {
    if(format=="id") {
      if(is.na(out)) {
        url <- NA
      } else {
        ids <- sbtools::item_get(out)$identifiers[[1]]
        if(!is.null(ids))
          browser_format <- if(ids$type %in% c("root", "site_root")) "folder_url" else "item_url"
        else 
          browser_format <- "item_url"
        url <- format_item(item, format=browser_format, browser=FALSE)
      }
    } else {
      url <- out
    }
    if(!is.na(url)) BROWSE(url)
  }
  out
}

#' Find a high-level folder on ScienceBase
#' 
#' @param folder the folder to locate
#' @inheritParams locate_item
#' @import sbtools
#' @export
#' @examples 
#' \dontrun{
#' locate_folder("publications", format="url")
#' testthat::expect_error(locate_folder("cvs", format="url"))
#' }
locate_folder <- function(folder=c("project","presentations","proposals","publications","sites","sites_meta","ideas"), 
                          format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  folder <- tolower(folder)
  folder <- match.arg(folder)
  if(!(folder %in% c('sites','sites_meta')) && is.null(current_session())) 
    stop("session is NULL, so only the sites folder is visible. see authenticate_sb()")
  if(folder == 'project' && by %in% c("dir", "either"))
    stop("'by' must be 'tag' when searching for the project folder")
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="folder_url")
  locate_item(key=folder, type="root", parent=locate_folder("project", by="tag"), title=folder, by=by, format=format, limit=limit, browser=browser)
}

#' Find a site folder on ScienceBase
#' 
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_site("nwis_02322688", format="url")
#' locate_site(c("nwis_02322688", "nwis_03259813", "nwis_04024000"))
#' locate_site("nwis_notasite", format="url")
#' testthat::expect_error(locate_site("notasite", format="url"))
#' }
locate_meta <- function(type, format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  meta_type <- paste0("meta_",type)
  meta_folder <- locate_folder("sites_meta", by="tag")
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  locate_item(key="sites_meta", type=meta_type, parent=meta_folder, title=meta_type, by=by, format=format, limit=limit, browser=browser)
}

#' Find a site folder on ScienceBase
#' 
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_site("nwis_02322688", format="url")
#' locate_site(c("nwis_02322688", "nwis_03259813", "nwis_04024000"))
#' locate_site("nwis_notasite", format="url")
#' testthat::expect_error(locate_site("notasite", format="url"))
#' }
locate_site <- function(site_name, format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  site_name <- do.call(make_site_name, parse_site_name(site_name, out=c("sitenum","database"), use_names=FALSE)) # check the site name
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="folder_url")
  locate_item(key=site_name, type="site_root", parent=locate_folder("sites", by="tag"), title=site_name, by=by, format=format, limit=limit, browser=browser)
}

#' Find a timeseries item on ScienceBase
#' 
#' @param var_src the variable name, e.g., "doobs_nwis", for which you want the 
#'   timeseries
#' @param site_name the site ID, e.g. "nwis_02322688", whose folder you want to
#'   look in
#' @inheritParams locate_item
#' @export
#' @examples 
#' \dontrun{
#' locate_ts(c("doobs","wtr","disch"), "nwis_02322688")
#' locate_ts("doobs", "nwis_02322688", format="url")
#' locate_ts("doobs", "nwis_notasite", format="url")
#' testthat::expect_error(locate_ts("notavar", "nwis_notasite"))
#' }
locate_ts <- function(var_src="doobs_nwis", site_name="nwis_02322688", format=c("id","url"), by=c("tag","dir","either"), limit=5000, browser=(format=="url")) {
  by <- match.arg(by)
  var_src <- make_ts_name(var_src) # check that the variable name is valid and add prefix
  site_name <- do.call(make_site_name, parse_site_name(site_name, out=c("sitenum","database"), use_names=FALSE)) # check the site name
  browser <- isTRUE(browser)
  format <- switch(match.arg(format), id="id", url="item_url")
  if(by %in% c("dir","either")) {
    site_parent <- locate_site(site_name, by="either")
    if(any(na_site <- is.na(site_parent))) stop("couldn't find the site folder[s] ", paste0(site_name[na_site], collapse=", "))
  } else {
    site_parent <- NA
  }
  locate_item(key=site_name, type=var_src, parent=site_parent, title=var_src, by=by, format=format, limit=limit, browser=browser)
}


#' Repair a ts item that is missing its identifier tags
#' 
#' Sometimes ts items/files get posted but the identifiers don't get updated, 
#' making it harder to search for the ts item. This function gives ScienceBase 
#' another chance to add tags.
#' 
#' @param var_src one or more var_src strings, e.g., "doobs_nwis"
#' @param site_name one or more site names, e.g., "nwis_040871488"
#' @param limit the maximum number of items to return in the SB query to find 
#'   all listed var_src:site_name combinations
#'   
#' @import sbtools
#' @export
#' 
#' @examples 
#' \dontrun{
#' repair_ts("wtr_nwis", "nwis_01374019")
#' }
repair_ts <- function(var_src, site_name, limit=5000) {
  
  # check the session; we'll need write access
  if(is.null(current_session()))
    stop("log in to repair data. see authenticate_sb()")
  
  # package the args together for arg replication & easier looping
  query_args <- data.frame(
    var_src=var_src, site_name=site_name, 
    var_src_site=paste0(var_src, "-", site_name),
    ts_id_tag=locate_ts(var_src=var_src, site_name=site_name, by='tag', limit=limit),
    ts_id_dir=locate_ts(var_src=var_src, site_name=site_name, by='either', limit=limit),
    stringsAsFactors=FALSE
  )
  
  # if we can't find the item, throw an error
  if(any(bad_rows <- is.na(query_args$ts_id_dir))) {
    warning("couldn't find the ts for\n", 
         paste(query_args[bad_rows,'var_src_site'], collapse=" or\n"),
         ", even searching by dir")
    query_args <- query_args[!bad_rows,]
  }
  
  sapply(setNames(seq_len(nrow(query_args)), query_args$ts_id_dir), function(arg) {
    # unpackage the df row
    var_src <- query_args[arg, "var_src"]
    site_name <- query_args[arg, "site_name"]
    ts_id_tag <- query_args[arg, "ts_id_tag"]
    ts_id_dir <- query_args[arg, "ts_id_dir"]
    
    # if we found the ts by tags, we're already good to return
    if(!is.na(ts_id_tag)) return(NA)
    
    # redo the action that somehow failed before
    idlist <- list(type=make_ts_name(var_src), scheme=get_scheme(), key=site_name)
    tryCatch(
      item_update_identifier(id=ts_id_dir, scheme=idlist$scheme, type=idlist$type, key=idlist$key),
      warning=function(w) { message("warning in item_update_identifier: ", w) }
    )
    
    # waiting and checking is required
    for(wait in 1:100) {
      Sys.sleep(0.2)
      is_updated <- isTRUE(all.equal(item_get(ts_id_dir)$identifiers[[1]], idlist))
      if(is_updated) break
      if(wait==100) {
        warning("identifiers couldn't be restored; try again later with ",
                "repair_ts('", var_src, "', '", site_name, ")")
        return(FALSE)
      }
    }
    return(TRUE)
  })
}

#' Repair a meta item that is missing its identifier tags
#' 
#' Sometimes ts items/files get posted but the identifiers don't get updated, 
#' making it harder to search for the item. This function gives ScienceBase 
#' another chance to add tags.
#' 
#' @param type one or more meta types, e.g., "basic"
#' @param limit the maximum number of items to return in the SB query to find 
#'   all listed var_src:site_name combinations
#'   
#' @import sbtools
#' @export
#' 
#' @examples 
#' \dontrun{
#' repair_meta("basic")
#' }
repair_meta <- function(type, limit=5000) {
  
  # check the session; we'll need write access
  if(is.null(current_session()))
    stop("log in to repair data. see authenticate_sb()")
  
  # package the args together for arg replication & easier looping
  query_args <- data.frame(
    type=type, 
    meta_type=paste0("meta_", type),
    meta_id_tag=locate_meta(type=type, by='tag', limit=limit),
    meta_id_dir=locate_meta(type=type, by='either', limit=limit),
    stringsAsFactors=FALSE
  )
  
  # if we can't find the item, throw an error
  if(any(bad_rows <- is.na(query_args$meta_id_dir))) {
    warning("couldn't find the metadata file for\n", 
            paste(query_args[bad_rows,'meta_type'], collapse=" or\n"),
            ", even searching by dir")
    query_args <- query_args[!bad_rows,]
  }
  
  sapply(setNames(seq_len(nrow(query_args)), query_args$meta_id_dir), function(arg) {
    # unpackage the df row
    type <- query_args[arg, "type"]
    meta_type <- query_args[arg, "meta_type"]
    meta_id_tag <- query_args[arg, "meta_id_tag"]
    meta_id_dir <- query_args[arg, "meta_id_dir"]
    
    # if we found the metafile by tags, we're already good to return
    if(!is.na(meta_id_tag)) return(NA)
    
    # redo the action that somehow failed before
    idlist <- list(type=meta_type, scheme=get_scheme(), key="sites_meta")
    tryCatch(
      item_update_identifier(id=meta_id_dir, scheme=idlist$scheme, type=idlist$type, key=idlist$key),
      warning=function(w) { message("warning in item_update_identifier: ", w) }
    )
    
    # waiting and checking is required
    for(wait in 1:100) {
      Sys.sleep(0.2)
      is_updated <- isTRUE(all.equal(item_get(meta_id_dir)$identifiers[[1]], idlist))
      if(is_updated) break
      if(wait==100) {
        warning("identifiers couldn't be restored; try again later with ",
                "repair_meta('", type, "')")
        return(FALSE)
      }
    }
    return(TRUE)
  })
}
