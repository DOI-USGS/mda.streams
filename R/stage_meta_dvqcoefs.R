#' Create/modify file of depth v discharge scaling relationships
#' 
#' Pull in scaling coefficients from Jud Harvey
#' 
#' @param folder the folder in which to save the metadata file
#' @param verbose logical. print status messages?
#' @import dplyr
#' @importFrom unitted u v get_units
#' @export
stage_meta_dvqcoefs <- function(folder = tempdir(), verbose = FALSE) {
  
  # get a list of sites known to us/sciencebase
  meta_basic <- get_meta('basic')
  meta_basic$site_num <- as.numeric(meta_basic$site_num)
  
  # import Jud's list
  jud_coefs <- read.table(system.file("extdata/RatingCurves_JudHarvey.tsv", package="mda.streams"), header=T, sep="\t", colClasses="numeric")
  
  # determine which site_nums we can't use from Jud
  unknown_sites <- setdiff(jud_coefs$site_num, meta_basic$site_num)
  if(length(unknown_sites)>0) warning("omitting coefficients from these unknown sites: ", paste0(unknown_sites, collapse=", "))
  
  # subset to just those sites we CAN use
  #jud_coefs <- jud_coefs[na.omit(match(meta_basic$site_num, jud_coefs$site_num)), ]
  
  # turn NA flags (-9999.00) into NAs
  for(col in 1:ncol(jud_coefs)) {
    jud_coefs[jud_coefs[,col]==-9999,col] <- NA
  }
  
  # while we're here, see how Jud's NHD codes match with ours
  meta_basic$nhdplus_id <- as.numeric(meta_basic$nhdplus_id)
  meta_basic$jud_nhdplus_id <- jud_coefs[match(meta_basic$site_num, jud_coefs$site_num), 'nhdplus_id']
  mismatched_nhd_ids <- which(!(meta_basic$nhdplus_id == meta_basic$jud_nhdplus_id))
  if(length(mismatched_nhd_ids) > 0) {
    print("found disagreement in nhd_ids for these sites: ")
    print(meta_basic[mismatched_nhd_ids, c('site_name', 'nhdplus_id', 'jud_nhdplus_id')])
  }
  
  # clean up jud's table, keeping just the columns we want, adding units, etc.
  site_num <- jud_c <- jud_f <- '.dplyr.var'
  jud_coefs <- jud_coefs %>%
    inner_join(v(meta_basic[,c('site_name','site_num')]), by='site_num') %>%
    select(site_name, c=jud_c, f=jud_f) %>%
    as.data.frame() %>%
    u(c(NA, NA, NA))

  # either return the data.frame, or save data to local file and return the
  # filename.
  if(is.null(folder)) {
    return(jud_coefs)
  } else {
    fpath <- make_meta_path(type='dvqcoefs', folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write_unitted(jud_coefs, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
    close(gz_con)
    return(invisible(fpath))
  }
  
}
