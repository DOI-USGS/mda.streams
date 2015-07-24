#' Create/modify sites file
#' 
#' @param sites a vector of sites to acquire/update metadata for
#' @param folder the folder in which to save the metadata file
#' @param verbose logical. print status messages?
#' @import dplyr
#' @importFrom unitted u v get_units
#' @export
#' @examples 
#' \dontrun{
#' stage_meta(sites=list_sites()[655:665], verbose=TRUE, folder=NULL)
#' }
stage_meta <- function(sites=list_sites(), folder = tempdir(), verbose = FALSE) {
  
  # Establish the basic site information
  sites_meta <- 
    data_frame(
      site_name=sites, 
      site_database=parse_site_name(sites, out='database'),
      site_num=parse_site_name(sites, out='sitenum')) %>%
    as.data.frame() %>% # unitted_data.frames work better than unitted_tbl_dfs for now
    u()
  
  # get metadata from each site database in turn. can't use filter() until
  # filter.unitted is implemented.
  meta_nwis <- sites_meta[sites_meta$site_database=="nwis",] %>% stage_meta_nwis(verbose=verbose)
  meta_styx <- sites_meta[sites_meta$site_database=="styx",] %>% stage_meta_styx(verbose=verbose)

  # merge the datasets
  if(!all.equal(get_units(meta_nwis), get_units(meta_styx))) stop("units mismatch in binding meta_xxxxes")
  meta_merged <- bind_rows(v(meta_nwis), v(meta_styx)) %>% as.data.frame() %>% 
    u(get_units(meta_nwis))
  sites_meta <- left_join(sites_meta, meta_merged, by="site_name", copy=TRUE)
 
  # add a quick-access column of sciencebase site item IDs
  sites_meta$sciencebase_id <- locate_site(sites_meta$site_name, format="id", browser=FALSE)
  
  # either return the data.frame, or save data to local file and return the
  # filename.
  if(is.null(folder)) {
    return(sites_meta)
  } else {
    fpath <- make_meta_path(folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write_unitted(sites_meta, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
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
#' @importFrom unitted u v
#' @import dplyr
#' @keywords internal
#' @examples
#' \dontrun{
#' sites <- list_sites()[655:665]
#' sites_meta <- data_frame(
#'   site_name=sites, 
#'   site_database=parse_site_name(sites, out='database'),
#'   site_num=parse_site_name(sites, out='sitenum')) %>%
#'   as.data.frame() %>% # unitted_data.frames work better than unitted_tbl_dfs for now
#'   u()
#' str(mda.streams:::stage_meta_nwis(sites_meta, verbose=TRUE))
#' }
stage_meta_nwis <- function(sites_meta, verbose=FALSE) {
  
  if(verbose) message("acquiring NWIS metadata")
  
  # get NWIS site metadata from NWIS in chunks
  site_database <- site_name <- group <- . <- '.dplyr.var'
  group_size <- 10
  sites_meta <- sites_meta %>%
    v() %>% # unitted can't handle all that's to come here
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
  station_nm <- dec_lat_va <- dec_long_va <- lat_va <- long_va <- dec_coord_datum_cd <- 
    coord_datum_cd <- alt_va <- alt_datum_cd <- site_name <- site_no <- 
    long_name <- lat <- lon <- coord_datum <- alt <- alt_datum <- site_name <- '.dplyr.var'
  sites_meta <- sites_meta %>% 
    mutate(
      long_name=station_nm,
      lat=ifelse(!is.na(dec_lat_va) & !is.na(dec_long_va), dec_lat_va, parse_nwis_coords(lat_va)),
      lon=ifelse(!is.na(dec_lat_va) & !is.na(dec_long_va), dec_long_va, -parse_nwis_coords(long_va)),
      coord_datum=ifelse(!is.na(dec_lat_va) & !is.na(dec_long_va), dec_coord_datum_cd, coord_datum_cd),
      alt=alt_va,
      alt_datum=alt_datum_cd) %>%
    # eliminate duplicates
    group_by(site_no) %>%
    summarize(long_name=long_name[1], lat=mean(lat, na.rm=TRUE), lon=mean(lon, na.rm=TRUE), coord_datum=unique(coord_datum),
              alt=mean(alt_va), alt_datum=unique(alt_datum_cd)) %>%
    # turn site_no into site_name
    mutate(site_name=make_site_name(unique(site_no), database="nwis"))
  
  # load NHDPlus ComIDs from file and attach to sites_meta
  comids <- read.table("inst/extdata/NHDPlus_ComIDs.tsv", header=TRUE, sep="\t", colClasses="character")
  comids[comids$comid_best==0,'comid_best'] <- NA
  matched_comids <- comids[match(sites_meta$site_no, comids$nwis_id),c("comid_best","comid_confidence")]
  sites_meta$nhdplus_id <- matched_comids$comid_best
  sites_meta$nhdplus_id_confidence <- matched_comids$comid_confidence

  # format
  nhdplus_id <- nhdplus_id_confidence <- '.dplyr.var'
  sites_meta  %>%
    # put columns in proper order
    select(site_name, long_name, lat, lon, coord_datum, alt, alt_datum, nhdplus_id, nhdplus_id_confidence) %>%
    # add units
    as.data.frame() %>%
    u(c(site_name=NA, long_name=NA, lat="degN", lon="degE", coord_datum=NA, alt="ft", alt_datum=NA, nhdplus_id=NA, nhdplus_id_confidence=NA))
}

#' Get data for Styx (simulated data) sites
#' 
#' Helper to stage_meta. Create dummy columns of metadata for Styx sites.
#' 
#' @param sites_meta a data.frame of site_names and parsed site_names, as in the
#'   opening lines of stage_meta
#' @param verbose logical. give status messages?
#' @importFrom dataRetrieval readNWISsite
#' @importFrom unitted u v
#' @keywords internal
#' @examples
#' \dontrun{
#' sites <- list_sites()[655:665]
#' sites_meta <- data_frame(
#'   site_name=sites, 
#'   site_database=parse_site_name(sites, out='database'),
#'   site_num=parse_site_name(sites, out='sitenum')) %>%
#'   as.data.frame() %>% # unitted_data.frames work better than unitted_tbl_dfs for now
#'   u()
#' str(mda.streams:::stage_meta_styx(sites_meta, verbose=TRUE))
#' }
stage_meta_styx <- function(sites_meta, verbose=FALSE) {
  site_name <- '.dplyr.var'
  sites_meta %>%
    select(site_name) %>%
    mutate(long_name=site_name, lat=NA, lon=NA, coord_datum=NA, alt=NA, alt_datum=NA, nhdplus_id=NA, nhdplus_id_confidence=NA) %>%
    as.data.frame() %>%
    u(c(site_name=NA, long_name=NA, lat="degN", lon="degE", coord_datum=NA, alt="ft", alt_datum=NA, nhdplus_id=NA, nhdplus_id_confidence=NA))
}