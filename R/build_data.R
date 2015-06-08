#' Variable code data
#'
#' Variable code data
#'
#' @name var_codes
#' @docType data
#' @export
NULL

#' Source code data
#'
#' Source code data
#'
#' @name src_codes
#' @docType data
#' @export
NULL

#' For use in package development only: build R/sysdata.rda
#' 
#' Moves/reformats the tables of variable and source metadata from inst/extdata 
#' (where they can be manually edited in .tsv format) to R/sysdata.rda (the 
#' storage location for data used internally by mda.streams)
#' 
#' @keywords internal
build_sysdata <- function() {

  # Read in the raw, hand-editable tsv files
  var_codes <- read.table(file="inst/extdata/var_codes.tsv", header=TRUE, colClasses="character", sep="\t", stringsAsFactors=FALSE)
  src_codes <- read.table(file="inst/extdata/src_codes.tsv", header=TRUE, colClasses="character", sep="\t", stringsAsFactors=FALSE)
  
  # Consistency checking: make sure all var_codes$src entries are in src_codes
  varcode_srces <- strsplit(var_codes$src, '|', fixed=TRUE) %>% unlist() %>% unique() %>% na.omit()
  if(any(unknown_src <- !(varcode_srces %in% src_codes$src))) {
    stop("unknown src in var_codes: ", paste0(varcode_srces[unknown_src], collapse=", "))
  }
  
  # Save
  save(var_codes, src_codes, file="R/sysdata.rda")
}
