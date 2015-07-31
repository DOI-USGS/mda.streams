#' Acquire data from the Accumulated and Reach Catchment Land Variables dataset
#' 
#' Mike Wieczorek's database, Accumulated and Reach Catchment Land Variables 
#' (abbreviated ARCLV here), contains data computed for every NHDPlus reach, 
#' identifiable by ComID. This function pulls data from one ARCLV table, subsets
#' to just the reaches relevant to the Powell Center study, and prepares that 
#' table to be posted as a metadata table in the Powell Center SB directory.
#' 
#' @param types the metadata types to be acquired. This type will be identical 
#'   (even in case) to the name of the zip file from which the data will be 
#'   acquired. E.g., AC_NLCD11 is the type for 
#'   https://www.sciencebase.gov/catalog/item/534c35d0e4b0af6611b1d0d0, 
#'   Accumulated areas of NLCD 2011 Land-Use Classes
#' @param folder the folder in which to store both the full table from Mike 
#'   Wieczorek's site and the formatted, subsetted table destined for our SB 
#'   directory
#' @importFrom foreign read.dbf write.dbf
#' @import dplyr
#' @importFrom unitted v u
#' @export
stage_meta_nawqahst <- function(types=c('AC_RUNOFF', 'Ac_popd10', 'AC_NLCD11'), folder='temp') {
  
  # authenticate SB - access is needed to download the data
  if(is.null(current_session())) stop("need ScienceBase access; call login_sb() first")
  
  # create the folder if it doesn't exist
  if(!dir.exists(folder)) dir.create(folder, showWarnings=TRUE)
  
  # get our list of NHDPlus ComIDs
  . <- site_name <- nhdplus_id <- '.dplyr.var'
  site_ids <- get_meta('basic') %>%
    .[.$nhdplus_id_confidence %in% c('MOST_SURE','MAYBE2','MAYBE1'),] %>%
    select(site_name, nhdplus_id) %>%
    mutate(nhdplus_id = as.numeric(nhdplus_id))
  
  # get our table of NAWQA-HST variable codes
  codefile <- system.file('extdata/NAWQA_HST_codes.tsv', package='mda.streams')
  nawqahst_vars <- read.table(codefile, sep="\t", header=TRUE, colClasses='character')
  
  # extract the relevant rows from each of the tables specified by types
  Variable.File_Name <- '.dplyr.var'
  metadfs <- lapply(types, function(type) {
    
    # look up the location of this variable
    item_id <- nawqahst_vars %>% filter(Variable.File_Name == type) %>% .$Variable.ScienceBase.ID
    if(length(item_id) != 1) stop("couldn't find type ", type, " in extdata/NAWQA_HST_codes.tsv")
    filename <- paste0(folder,"/",type)

    # get the large data file from Mike Wieczorek's ScienceBase page
    if(!file.exists(paste0(filename,'.zip'))) {
      message('downloading ', type, ' to ', filename, '.zip')
      download_item_files(item_id, type, files=paste0(type,'.zip'), folder=folder, on_remote_missing='stop', on_local_exists='skip')
    }
    if(!file.exists(paste0(filename,'.dbf'))) {
      unzip(paste0(filename,".zip"), exdir=folder)
    }
    #system('wc temp/Ac_popd10.dbf') # lines, words, character counts. given 3 columns, nrow=wc[2]/3
    bigdata <- read.dbf(paste0(filename,".dbf"))
    
    # join the data.frames, filtering to just the nhdplus_ids in site_ids.
    # right_join and data.table %>% right_join are both ~10x slower than this:
    meta_mtype <- bigdata[match(site_ids$nhdplus_id, bigdata$COMID),]
    rownames(meta_mtype) <- NULL
    
    # write the subsetted table to .dbf to speed future staging, then clear the
    # big table from memory
    write.dbf(meta_mtype, file=paste0(filename,".dbf"))
    rm(bigdata)

    # remove comid because it's redundant
    meta_mtype <- meta_mtype[names(meta_mtype) != 'COMID']

    # add units
    typeunits <- 
      nawqahst_vars %>% filter(Variable.File_Name == type) %>% .$ColnameUnits %>%
      strsplit(., split=";") %>% .[[1]] %>%
      strsplit(., split="=") %>% sapply(., function(x) setNames(x[2], x[1]))
    
    if(!all.equal(names(typeunits), names(meta_mtype))) stop("mismatched units specs and data cols")
    meta_mtype <- u(meta_mtype, typeunits)
    
    # rename columns to make unique
    names(meta_mtype) <- paste0(type,".",names(meta_mtype))
    
    # return
    meta_mtype
  })
  
  # merge and add units. haven't identified a good way to extract units from the NAWQA-HST SB site yet
  metadf <- do.call(cbind, c(list(site_ids), metadfs)) %>% u()
  
  # write to table
  dest <- paste0(folder,"/meta_nawqahst.tsv")
  write_unitted(metadf, file=dest)
  dest
}