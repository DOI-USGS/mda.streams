#' Create/modify file of interferences within the metabolic footprints
#'
#' Create/modify file of interferences within quantiles of the metabolic
#' footprint
#'
#' @param struct_file path to the file containing data on distances to
#'   structural interferences - NPDES, canals, and dams. Description of that
#'   file: "A -9999 means that there weren't any of that feature in the basin.
#'   i.e. no dam or no NPDES etc.  Other numbers express the straight-line
#'   geodesic distance between the probe and the feature in meters"
#' @param folder the folder in which to save the metadata file
#' @param verbose logical. print status messages?
#' @import dplyr
#' @importFrom unitted u v get_units
#' @importFrom utils read.table
#' @export
stage_meta_struct <- function(struct_file = "../stream_metab_usa/1_spatial/in/CONF_struct_170407.csv", 
                                  folder = tempdir(), verbose = FALSE) {
  
  # get a list of sites known to us/sciencebase
  meta_basic <- get_meta('basic')
  
  # load the structures files
  struct <- read.csv(struct_file, stringsAsFactors=FALSE, header=TRUE)
  struct[struct == -9999] <- Inf # based on metadata: -9999 means there isn't one in the watershed
  
  # compute quantiles for daily metabolic footprint data. use any mfootdailys available in
  # ts_folder rather than downloading them; download anything we don't have
  mf_quants <- bind_rows(lapply(meta_basic$site_name, function(site) {
    mfootdaily <- tryCatch(
      unitted::v(get_ts(var_src='mfootdaily_calc3vK', site_name=site)$mfootdaily),
      error=function(e) NA)
    as.data.frame(c(list(site_name=site), as.list(quantile(mfootdaily, c(0.5, 0.8, 0.95), na.rm=TRUE))), check.names=FALSE, stringsAsFactors=FALSE)
  }))
  
  # compute flags based on quantiles. the footprint distances for 95% turnover
  # are -log(0.05)v/k = 3v/K and the footprint distances for 80% turnover are
  # -log(0.2)v/k = 1.6v/k. so we can use the same quantiles but multiply by
  # 1.6/3 = -log(0.2)/3 to get quantiles for 80% turnover
  mf_qs <- left_join(mf_quants, struct, by=c('site_name'='site_no')) %>%
    rename(q50='50%', q80='80%', q95='95%') %>%
    mutate(q50=q50*-log(0.2)/3, q80=q80*-log(0.2)/3, q95=q95*-log(0.2)/3) %>% # convert from 95% to 80% turnover footprints
    group_by(site_name) %>%
    do(mutate(
      .,
      canal_flag = ifelse(CANAL_DIST > q95, 95, # 95th quantile of 80%-turnover footprints
                          ifelse(CANAL_DIST > q80, 80, # 80th quantile of 80%-turnover footprints
                                 ifelse(CANAL_DIST > q50, 50, 0))), # 50th quantile of 80%-turnover footprints
      dam_flag = ifelse(DAM_DIST > q95, 95, # 95th quantile of 80%-turnover footprints
                          ifelse(DAM_DIST > q80, 80, # 80th quantile of 80%-turnover footprints
                                 ifelse(DAM_DIST > q50, 50, 0))), # 50th quantile of 80%-turnover footprints
      npdes_flag = ifelse(NPDES_DIST > q95, 95, # 95th quantile of 80%-turnover footprints
                          ifelse(NPDES_DIST > q80, 80, # 80th quantile of 80%-turnover footprints
                                 ifelse(NPDES_DIST > q50, 50, 0))) # 50th quantile of 80%-turnover footprints
    )) %>%
    ungroup()
  

  # finalize the report
  meta_struct <- select(mf_qs, site_name, ends_with('flag')) %>%
    as.data.frame(stringsAsFactors=FALSE) %>% 
    unitted::u() # these flags don't really have good units. quantiles (% of days) but also turnover (a different sort of %)
  
  # either return the data.frame, or save data to local file and return the
  # filename.
  if(is.null(folder)) {
    return(meta_struct)
  } else {
    fpath <- make_meta_path(type='struct', folder=folder)
    gz_con <- gzfile(fpath, "w")
    meta_file <- write_unitted(meta_struct, file=gz_con, sep="\t", row.names=FALSE, quote=TRUE)
    close(gz_con)
    return(invisible(fpath))
  }
}


summarize_meta_struct <- function(meta_struct) {
  mf_counts <- meta_struct
  if('dam_flag' %in% names(mf_counts)) {
    mf_counts <- rename(mf_counts, struct.dam_flag=dam_flag, struct.canal_flag=canal_flag, struct.npdes_flag=npdes_flag)
  }
  mf_counts <- mf_counts %>%
    group_by(struct.dam_flag, struct.canal_flag, struct.npdes_flag) %>% count()
  flags <- c(0,50,80,95)
  
  cum_counts <- bind_rows(lapply(flags, function(dam) {
    bind_rows(lapply(flags, function(npdes) {
      bind_rows(lapply(flags, function(canal) {
        data_frame(
          dam_atleast = dam,
          npdes_atleast = npdes,
          canal_atleast = canal,
          count = filter(mf_counts, 
                         struct.dam_flag >= dam,
                         struct.npdes_flag >= npdes,
                         struct.canal_flag >= canal) %>% 
            pull(n) %>% sum
        )
      }))
    }))
  }))
  
  list(
    simple =
      # if we're going to display this, i'd do it this way
      filter(cum_counts, dam_atleast == npdes_atleast, npdes_atleast == canal_atleast),
    complete = {
      # a more exhaustive set of counts. 
      message("for complete counts, rows=dams, cols=npdes, pipes=canals")
      cum_counts %>%
        tidyr::spread(key=canal_atleast, value=count) %>%
        mutate(counts=paste(`0`,`50`,`80`,`95`,sep='|')) %>%
        select(-`95`,-`80`,-`50`,-`0`) %>%
        tidyr::spread(key=npdes_atleast, value=counts)
    })
}
