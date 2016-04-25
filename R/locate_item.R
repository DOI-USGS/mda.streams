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
#' @importFrom stats setNames
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
    )

  if(get_scheme() == 'mda_streams_dev' && !is_logged_in())
    stop("log in to use mda_streams_dev. see login_sb()")

  # run the query or queries
  if(by %in% c("tag","either")) {
    item_df <- bind_rows(lapply(1:nrow(query_args), function(argnum) {
      # query by tag
      query_arg_list <- c(
        list(scheme=get_scheme(), limit=limit),
        if(!is.na(query_args$type[argnum])) list(type=query_args$type[argnum]), 
        if(!is.na(query_args$key[argnum])) list(key=query_args$key[argnum]))
      item_list <- do.call(query_item_identifier, query_arg_list)
      # create a df of NAs if item wasn't found
      arg_item_df <- if(length(item_list) == 0) {
        data.frame(title=NA_character_, id=NA_character_,stringsAsFactors=FALSE)
      } else {
        as.data.frame(bind_rows(lapply(item_list, function(item) as_data_frame(item[c('title','id')]))))
      }
      # attach argnum and return
      data.frame(query_args[argnum,], argnum=argnum, rename(arg_item_df, item.title=title), stringsAsFactors=FALSE)
    }))
  } else if(by == "dir") {
    # just set up df so that the next block will do the search
    item_df <- data.frame(query_args, argnum=1:nrow(query_args), item.title=NA_character_, id=NA_character_, stringsAsFactors=FALSE)
  }
  
  # query by title as requested+needed
  if(by %in% c("dir","either")) {
    needy_argnums <- unique(item_df[is.na(item_df$id), "argnum"])
    for(i in needy_argnums) {
      query <- item_df[item_df$argnum == i, ]
      # query_item_in_folder finds item_df where the title matches any text 
      # (title, abstract, key, etc.) anywhere in the specified folder or its 
      # children
      items <- query_item_in_folder(text=query$title[1], folder=query$parent[1], limit=limit)
      # filter to just those whose *title* matches the text query
      if(length(items) > 0) {
        items <- items[tolower(sapply(items, function(qitem) qitem$title)) == tolower(query$title[1])]
      }
      # filter again to just those whose parentID is exactly the folder we specified
      if(length(items) > 0) {
        parentIds <- sapply(items, function(qitem) {item_get_fields(qitem$id, "parentId")}) # slow, but necessary until sbtools#193 gets resolved
        items <- items[parentIds == query$parent[1]]
        if(length(items) > 0) {
          if(length(items) > 1) stop('found more than one match in query for key=', query$key, ', type=', query$type, ', parent=', query$parent, ', title=', query$title)
          item_df[item_df$argnum == i, "id"] <- items[[1]]$id
          item_df[item_df$argnum == i, "item.title"] <- items[[1]]$title
        }
      }
    }
  }
  
  # reformat the results to our liking
  format_item(item_df, format, browser)
}

#' Format an item as a ScienceBase ID or URL, as requested
#' 
#' Internal helper to locate_item. Doesn't check the value of format - that's up
#' to the calling functions to do, for efficiency.
#' 
#' @param item a data.frame with a column for id (and optionally columns for
#'   title, etc.) describing one or more ScienceBase items
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
        ids <- sbtools::item_get(out[1])$identifiers[[1]]
        if(!is.null(ids))
          browser_format <- if(ids$type %in% c("root", "site_root")) "folder_url" else "item_url"
        else 
          browser_format <- "item_url"
        url <- format_item(item, format=browser_format, browser=FALSE)
      }
    } else {
      url <- out
    }
    # only open first 10 URLs to avoid creating a bazillion tabs (assuming this would be an accident)
    if(length(url) > 10) 
      warning('found >10 URLs and browser=TRUE; only browsing to the first 10')
    sapply(url[1:min(10, length(url))], function(ur) if(!is.na(ur)) BROWSE(ur))
  }
  out
}
