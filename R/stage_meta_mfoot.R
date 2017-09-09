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
#' @param ts_folder the folder in which to look for and/or cache the timeseries
#'   data on daily metabolic footprints
#' @param folder the folder in which to save the metadata file
#' @param verbose logical. print status messages?
#' @import dplyr
#' @importFrom unitted u v get_units
#' @importFrom utils read.table
#' @export
stage_meta_dvqcoefs <- function(struct_file = "../stream_metab_usa/1_spatial/in/CONF_struct_170407.csv", 
                                ts_folder="../stream_metab_usa/2_metab_outputs/cache",
                                  folder = tempdir(), verbose = FALSE) {
  
  # get a list of sites known to us/sciencebase
  meta_basic <- get_meta('basic')
  
  # load the structures files
  struct <- read.csv(struct_file, stringsAsFactors=FALSE, header=TRUE)
  struct[struct == -9999] <- Inf # based on metadata: -9999 means there isn't one in the watershed
  
  # compute quantiles for daily metabolic footprint data. use any mfootdailys available in
  # ts_folder rather than downloading them; download anything we don't have
  mf_files <- dir(ts_folder, pattern='mfootdaily', full.names=TRUE)
  file.copy(mf_files, tempdir())
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
    group_by(site_name) %>%
    do(mutate(
      .,
      canal_flag_t95 = if(any(is.na(c(q50,q80,q95,CANAL_DIST)))) NA else
        cut(CANAL_DIST, breaks=c(0,q50,q80,q95,Inf), labels=c('0-50','50-80','80-95','95+')),
      canal_flag_t80 = if(any(is.na(c(q50,q80,q95,CANAL_DIST)))) NA else
        cut(CANAL_DIST, breaks=c(0,q50,q80,q95,Inf)*-log(0.2)/3, labels=c('0-50','50-80','80-95','95+')),
      canal_flag_t50 = if(any(is.na(c(q50,q80,q95,CANAL_DIST)))) NA else
        cut(CANAL_DIST, breaks=c(0,q50,q80,q95,Inf)*-log(0.5)/3, labels=c('0-50','50-80','80-95','95+')),
      
      dam_flag_t95 = if(any(is.na(c(q50,q80,q95,DAM_DIST)))) NA else
        cut(DAM_DIST, breaks=c(0,q50,q80,q95,Inf), labels=c('0-50','50-80','80-95','95+')),
      dam_flag_t80 = if(any(is.na(c(q50,q80,q95,DAM_DIST)))) NA else
        cut(DAM_DIST, breaks=c(0,q50,q80,q95,Inf)*-log(0.2)/3, labels=c('0-50','50-80','80-95','95+')),
      dam_flag_t50 = if(any(is.na(c(q50,q80,q95,DAM_DIST)))) NA else
        cut(DAM_DIST, breaks=c(0,q50,q80,q95,Inf)*-log(0.5)/3, labels=c('0-50','50-80','80-95','95+')),
      
      npdes_flag_t95 = if(any(is.na(c(q50,q80,q95,NPDES_DIST)))) NA else
        cut(NPDES_DIST, breaks=c(0,q50,q80,q95,Inf), labels=c('0-50','50-80','80-95','95+')),
      npdes_flag_t80 = if(any(is.na(c(q50,q80,q95,NPDES_DIST)))) NA else
        cut(NPDES_DIST, breaks=c(0,q50,q80,q95,Inf)*-log(0.2)/3, labels=c('0-50','50-80','80-95','95+')),
      npdes_flag_t50 = if(any(is.na(c(q50,q80,q95,NPDES_DIST)))) NA else
          cut(NPDES_DIST, breaks=c(0,q50,q80,q95,Inf)*-log(0.5)/3, labels=c('0-50','50-80','80-95','95+')))) %>%
    ungroup()
  
  mf_flags <- mf_qs %>%
    mutate(
      canal_flag = ifelse(canal_flag_t95 == '95+', '95&95', 
                          ifelse(canal_flag_t80 %in% c('80-95','95+'), '80&80',
                                 ifelse(canal_flag_t50 %in% c('50-80','80-95','95+'), '50&50', '0&0'))),
      dam_flag = ifelse(dam_flag_t95 == '95+', '95&95', 
                          ifelse(dam_flag_t80 %in% c('80-95','95+'), '80&80',
                                 ifelse(dam_flag_t50 %in% c('50-80','80-95','95+'), '50&50', '0&0'))),
      npdes_flag = ifelse(npdes_flag_t95 == '95+', '95&95', 
                          ifelse(npdes_flag_t80 %in% c('80-95','95+'), '80&80',
                                 ifelse(npdes_flag_t50 %in% c('50-80','80-95','95+'), '50&50', '0&0'))))
  
  mf_counts_t95 <- 
    mf_qs %>% #rename(dam_flag=dam_flag_t95, canal_flag=canal_flag_t95, npdes_flag=npdes_flag_t95) %>%
    group_by(dam_flag, canal_flag, npdes_flag) %>% count()
  mf_counts_t80 <- mf_qs %>% rename(dam_flag=dam_flag_t80, canal_flag=canal_flag_t80, npdes_flag=npdes_flag_t80) %>%
    group_by(dam_flag, canal_flag, npdes_flag) %>% count()
  mf_counts_t50 <- mf_qs %>% rename(dam_flag=dam_flag_t50, canal_flag=canal_flag_t50, npdes_flag=npdes_flag_t50) %>%
    group_by(dam_flag, canal_flag, npdes_flag) %>% count()
  
  flags <- ordered(levels(mf_counts_t95$canal_flag))  
  
  get_cum_counts <- function(counts_tx) {
    bind_rows(lapply(flags, function(dam) {
      bind_rows(lapply(flags, function(npdes) {
        bind_rows(lapply(flags, function(canal) {
          data_frame(
            dam_atleast = dam,
            npdes_atleast = npdes,
            canal_atleast = canal,
            count = filter(counts_tx, 
                           as.numeric(dam_flag) >= as.numeric(dam),
                           as.numeric(npdes_flag) >= as.numeric(npdes),
                           as.numeric(canal_flag) >= as.numeric(canal)) %>% 
              pull(n) %>% sum
          )
        }))
      }))
    })) %>%
      mutate(
        dam_atleast = c('0-50'='>0','50-80'='>50','80-95'='>80','95+'='>95')[dam_atleast],
        npdes_atleast = c('0-50'='>0','50-80'='>50','80-95'='>80','95+'='>95')[npdes_atleast],
        canal_atleast = c('0-50'='>0','50-80'='>50','80-95'='>80','95+'='>95')[canal_atleast]
      )
  }
  cum_counts_t95 <- get_cum_counts(mf_counts_t95)
  cum_counts_t80 <- get_cum_counts(mf_counts_t80)
  cum_counts_t50 <- get_cum_counts(mf_counts_t50)
  
  filter(cum_counts_t80, canal_atleast %in% c('>0','>95')) %>%
    tidyr::spread(key=canal_atleast, value=count) %>%
    mutate(counts=paste0(`>95`,'-',`>0`)) %>%
    select(-`>95`,-`>0`) %>%
    tidyr::spread(key=npdes_atleast, value=counts)
  
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
