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
    ) %>%
    as.data.frame(stringsAsFactors=FALSE)

  if(get_scheme() == 'mda_streams_dev' && !is_logged_in())
    stop("log in to use mda_streams_dev. see login_sb()")

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
