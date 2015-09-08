#' Combine several tables into a single table
#' 
#' @param ... tables (data.frames or tbl_dfs) to combine. These may be unitted 
#'   if fun is prepared for unitted tables. Tables are joined left to 
#'   right. e.g., if dots are A, B, C then the result is 
#'   fun(fun(A,B),C)
#' @param fun the combining function to apply to pairs of tables in the iterative 
#'   join.
#' @param by character. The columns[s] on which to join the tables. Good choices
#'   are 'DateTime' for timeseries data or 'site_name' for site metadata.
#' @param allow_constants logical. if TRUE, tables with \code{NA} in their \code{by} column
#' @return a joined function
#' @importFrom stats setNames
#' @keywords internal
combine_tables <- function(..., by, fun=combine_dplyr('full_join', by=by), allow_constants=FALSE) {
  dots <- list(...)
  if(length(dots) == 0) return(NULL)
  data <- dots[[1]]
  if(is.na(data[1,by]) && nrow(data)==1) {
    stop("first table in list should always be a full table, not a const")
  }
  if(!isTRUE(allow_constants)) {
    const_tbl <- sapply(dots[-1], function(dot) {
      is.na(dot[1,by]) && nrow(dot)==1
    })
    if(any(const_tbl)) {
      stop("table ", paste0(const_tbl+1, collapse=", "), " is a const, but allow_constants==FALSE")
    }
  }
  for(dot in dots[-1]) {
    data <- 
      if(isTRUE(allow_constants) && isTRUE(is.na(dot[1,by]))) {
        data.frame(data, rep(dot[,2],nrow(data))) %>%
          setNames(c(names(data), names(dot[2]))) %>%
          u()
      } else {
        fun(data, dot)
      }
  }
  data
}

#' A function to combine unitted data.frames/tbl_dfs
#' 
#' @param method character. dplyr join function to use.
#' @param by columns to join on, to be passed to the dplyr join function and
#'   arrange_
#' @return a joined, sorted, unitted tbl_df
#' @import dplyr
#' @keywords internal
combine_dplyr <- function(method, by, ...) {
  # use requested dplyr join method
  dplyr_join <- get(method, envir=environment(dplyr::full_join))
  function(x, y) {
    df <- dplyr_join(x, y, by=by, ...)
    df %>% v() %>% arrange_(by) %>% u(get_units(df))
  }
}