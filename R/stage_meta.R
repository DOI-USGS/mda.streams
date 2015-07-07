#' Create/modify sites file
#' 
#' @param sites a vector of sites to acquire/update metadata for
#' @param folder the folder in which to save the metadata file
#' @param verbose logical. print status messages?
#' @export
stage_meta <- function(sites=list_sites(), folder = tempdir(), verbose = FALSE) {
  
  # Extablish the basic site information
  sites_meta <- 
    data_frame(
      site_name=sites, 
      site_database=parse_site_name(sites, out='database'),
      site_num=parse_site_name(sites, out='sitenum'))
  
  # get metadata from each site database in turn
  meta_nwis <- sites_meta %>% filter(site_database=="nwis") %>% stage_meta_nwis(verbose=verbose)
  meta_styx <- sites_meta %>% filter(site_database=="styx") %>% stage_meta_styx(verbose=verbose)
  
  # merge the datasets
  sites_meta <- sites_meta %>% 
    left_join(bind_rows(meta_nwis, meta_styx), by="site_name")
  
  # either return the data.frame, or save data to local file and return the
  # filename.
  if(is.null(folder)) {
    return(sites_meta)
  } else {
    fpath <- make_meta_path(folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write.table(sites_meta, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
    close(gz_con)
    return(invisible(fpath))
  }
  
}

#' Get data for NWIS sites
#' 
#' Helper to stage_meta. Download/clean up columns of useful data for NWIS
#' sites
#' 
#' @param sites_meta a data.frame of site_names and parse site_names, as in the
#'   opening lines of stage_meta
#' @param verbose logical. give status messages?
#' @importFrom dataRetrieval readNWISsite
#' @keywords internal
stage_meta_nwis <- function(sites_meta, verbose=FALSE) {
  
  if(verbose) message("acquiring NWIS metadata")
  
  # get NWIS site metadata from NWIS in chunks
  group_size <- 10
  sites_meta <- sites_meta %>%
    filter(site_database == "nwis") %>%
    mutate(group=rep(1:ceiling(length(site_name)/group_size), each=group_size)[1:length(site_name)]) %>%
    group_by(group) %>%
    do(readNWISsite(.$site_num))
  
  # do our best to get decimal coordinates
  parse_nwis_coords <- function(coords) {
    deg <- floor(coords/10000)
    min <- floor((coords-deg*10000)/100)
    sec <- coords-deg*10000-min*100
    return(deg + (min/60) + (sec/3600))
  }
  sites_meta <- sites_meta %>% 
    select(site_no, lat_va, long_va, dec_lat_va, dec_long_va, coord_meth_cd, coord_acy_cd, coord_datum_cd, dec_coord_datum_cd) %>%
    mutate(
      lat=ifelse(!is.na(dec_lat_va) & !is.na(dec_long_va), dec_lat_va, parse_nwis_coords(lat_va)),
      lon=ifelse(!is.na(dec_lat_va) & !is.na(dec_long_va), dec_long_va, parse_nwis_coords(long_va)),
      coord_datum=ifelse(!is.na(dec_lat_va) & !is.na(dec_long_va), dec_coord_datum_cd, coord_datum_cd)) %>%
    select(site_no, lat, lon, coord_datum) %>%
    # eliminate duplicates
    group_by(site_no) %>%
    summarize(site_name=make_site_name(unique(site_no), database="nwis"), 
              lat=mean(lat, na.rm=TRUE), lon=mean(lon, na.rm=TRUE), coord_datum=unique(coord_datum)) %>%
    select(-site_no)
    
  sites_meta
}

#' Get data for Styx (simulated data) sites
#' 
#' Helper to stage_meta. Create dummy columns of metadata for Styx sites.
#' 
#' @param sites_meta a data.frame of site_names and parsed site_names, as in the
#'   opening lines of stage_meta
#' @param verbose logical. give status messages?
#' @importFrom dataRetrieval readNWISsite
#' @keywords internal
stage_meta_styx <- function(sites_meta, verbose=FALSE) {
  sites_meta %>%
    select(site_name) %>%
    mutate(lat=NA, lon=NA, coord_datum=NA)
}