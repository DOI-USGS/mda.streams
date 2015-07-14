#' Variable and source code data
#'
#' Variable and source code data
#'
#' @name var_src_codes
#' @docType data
#' @export
NULL

#' For use in package development only: build R/sysdata.rda
#' 
#' Moves/reformats the tables of variable and source metadata from inst/extdata 
#' (where they can be manually edited in .tsv format) to R/sysdata.rda (the 
#' storage location for data used internally by mda.streams)
#'
#' @import dplyr 
#' @keywords internal
build_sysdata <- function() {

  # Read in the raw, hand-editable tsv file
  var_src_codes <- 
    read.table(file="inst/extdata/var_src_codes.tsv", header=TRUE, colClasses="character", sep="\t", stringsAsFactors=FALSE) %>% 
    mutate(var_src=paste0(var, "_", src),
           priority=as.numeric(priority))
  
  # Consistency checking
  var_descrip <- metab_var <- priority <- src_descrip <- '.dplyr.var'
  # Within a var, ought to have just one unique units, var_descrip, metab_var
  var_counts <- (var_src_codes %>% group_by(var) %>% summarize(just_one=nrow(unique(data.frame(var, units, var_descrip, metab_var)))==1))
  if(length(which(!var_counts$just_one)) > 0) {
    stop("non-duplicate var details within var[s] ", var_counts[!var_counts$just_one, "var"])
  }
  # Within a var, ought to have many unique src, priority
  src_counts <- (var_src_codes %>% group_by(var) %>% summarize(test_nsrc=length(unique(src)) == length(src)))
  if(length(which(!src_counts$test_nsrc)) > 0) {
    stop("non-unique srces within var[s] ", src_counts[!src_counts$test_nsrc, "var"])
  }
  priority_counts <- (var_src_codes %>% group_by(var) %>% summarize(test_npriority=length(unique(priority)) == length(priority)))
  if(length(which(!priority_counts$test_npriority)) > 0) {
    stop("non-unique priorities within var[s] ", priority_counts[!priority_counts$test_npriority, "var"])
  }
  # Within a src, ought to have just one unique url, src_descrip
  src_counts <- (var_src_codes %>% group_by(src) %>% summarize(just_one=nrow(unique(data.frame(src, url, src_descrip)))==1))
  if(length(which(!src_counts$just_one)) > 0) {
    stop("non-duplicate src details within src[s] ", src_counts[!src_counts$just_one, "src"])
  }
  # p-codes should exist for data (and only data) types
  odd_pcode <- with(var_src_codes, !ifelse(!is.na(p_code), src_type=="data", TRUE))
  if(any(odd_pcode)) {
    stop("p_code for non-data src: ", paste0(var_src_codes[odd_pcode,"var"], "_", var_src_codes[odd_pcode,"src"], collapse=", "))
  }
  
  # Save
  save(var_src_codes, file="R/sysdata.rda")
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

