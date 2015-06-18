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

# NWIS data notes
# 
# Other promising dissolved oxygen p codes: 99977 (1m below surface), 99981 (1m
# above bottom), 99985 (mid-depth of water column)
# 
# Other temperature: 00011 (temperature, degF) 00020 and 00021 (air 
# temperature)
# 
# Barometric pressure: 00025, 62607
# 
# Light: 00030 (incident solar radiation intensity), 00200 (incident light, 
# 400-700 nm), 61028 (turbidity), 62608/62609 solar radiation, 99986 (solar
# radiation), 99989 (par, mmol/m2)
# 
# Flow: 00055 (stream velocity), 00060 (mean daily discharge), 00064 (mean 
# stream depth), 00072 (stream stage), 30207 (gage height), 30209 (discharge, 
# cms), 74072 (flow, L/s), 74082 (flow, acre ft), 81380 (discharge velocity),

