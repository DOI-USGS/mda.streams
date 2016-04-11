#' @title stage nwis data into a file
#' @description get data from nwis and return created file handle
#'   
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of variable as in
#'   \code{unique(get_var_src_codes(out="var"))}
#' @param times a length 2 vector of text dates in YYYY-MM-DD format
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param version character string indicating whether you want to stage the 
#'   \code{ts} as a .tsv or .rds
#' @param verbose provide verbose output (currently not implemented)
#' @return a character vector of file handles
#' @importFrom dataRetrieval readNWISuv
#' @importFrom unitted u
#' @importFrom stats setNames
#' @importFrom lubridate with_tz
#' @importFrom tidyr gather
#' @import dplyr
#'   
#' @examples
#' 
#' \dontrun{
#' 
#' files <- stage_nwis_ts(sites = c("nwis_06893820","nwis_01484680"), 
#'   var = "doobs", times = c('2014-01-01','2014-01-02'), verbose=TRUE)
#' head(read_ts(files[1]))
#' 
#' # par is unavailable for all sites, so returns NULL
#' stage_nwis_ts(sites = list_sites(), var = "par",
#'   times = c('2014-01-01', '2014-01-03'), verbose=TRUE) 
#' }
#' @export
stage_nwis_ts <- function(sites, var, times, folder = tempdir(), version=c('rds','tsv'), verbose = FALSE){

  version <- match.arg(version)
  
  # # diagnose an apparent issue with NWIS - many sites return values with a 1-hour offset from what has been requested in UTC
  # all_sites <- list_sites()
  # all_files <- stage_nwis_ts(sites = all_sites, var = "doobs", times = c('2014-06-01','2014-06-02'), verbose=TRUE)
  # tz_00 <- sapply(setNames(all_files, mda.streams:::parse_ts_path(all_files, "site_name")), function(file) format(unitted::v(read_ts(file)[1,"DateTime"]), "%H") == "00")
  # tz_01 <- sapply(setNames(all_files, mda.streams:::parse_ts_path(all_files, "site_name")), function(file) format(unitted::v(read_ts(file)[1,"DateTime"]), "%H") == "01")
  # tz_other <- sapply(setNames(all_files, mda.streams:::parse_ts_path(all_files, "site_name")), function(file) !(format(unitted::v(read_ts(file)[1,"DateTime"]), "%H") %in% c("00","01")))
  # write.table(mda.streams:::parse_ts_path(all_files[tz_00], "sitenum"), "C:/Users/aappling/desktop/right_time.txt", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
  # write.table(mda.streams:::parse_ts_path(all_files[tz_01], "sitenum"), "C:/Users/aappling/desktop/wrong_time.txt", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
  # write.table(mda.streams:::parse_ts_path(all_files[tz_other], "sitenum"), "C:/Users/aappling/desktop/other_time.txt", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
  
  # process inputs
  if(length(var) > 1) stop("one var at a time, please")
  verify_var_src(var, 'nwis', on_fail=warning)
  vars <- var # need a renamed version for get_var_src_codes filter on var
  p_code <- '.dplyr_var'
  p_code <- get_var_src_codes(src=="nwis",var%in%vars,!is.na(p_code),out="p_code")
  p_code_vec <- strsplit(p_code, split=",")[[1]]
  var_units <- get_var_src_codes(var==vars, src=='nwis', out='units')
  site_nums <- parse_site_name(sites)
  
  # request times with 1-hour buffer to deal with NWIS bug. specify times as UTC
  # (see http://waterservices.usgs.gov/rest/IV-Service.html#Specifying)
  dates <- times # keep in char format for qw data
  truetimes <- as.POSIXct(paste0(times, " 00:00:00"), tz="UTC")
  asktimes <- format(truetimes + as.difftime(c(-1, 0), units="hours"), "%Y-%m-%dT%H:%MZ")

  # download the data to nwis_data, with special handling for QW data
  convert_qw <- TRUE
  if(var %in% c("sed","sedpfine","so4","ca","ph","alk","no3","ntot","ptot","po4")) {
    requireNamespace("smwrQW")
  }
  # convert QW to numeric in an if() block. this will let us (1) test with the
  # existing code now, and (2) switch easily to QW format later
  switch(
    var,
    sed={
      . <- sample_dt <- sample_tm <- SuspSed <- sed <- dateTime <- '.dplyr.var'
      nwis_data <- smwrQW::importNWISqw(site_nums, p_code, begin.date=dates[1]) %>% #, end.date=dates[2] # slows everything horribly??
        transform(DateTime=as.POSIXct(paste(format(sample_dt, "%Y-%m-%d"), sample_tm), "%Y-%m-%d %H:%M", tz = "UTC"), 
                  sed=SuspSed) %>%
        .[,c('site_no','DateTime','sed')]
      if(convert_qw) {
        nwis_data <- nwis_data %>% transform(sed=as.numeric(sed))
      }
    },
    sedpfine={},
    so4={},
    ca={},
    ph={},
    alk={},
    no3={},
    ntot={},
    ptot={},
    po4={},
    {
      # default case for switch() applies to all metabolism input variables and 
      # anything else not listed above. For these we use dataRetrieval.
      if(isTRUE(verbose)) message("requesting data from NWIS for p_code ", p_code)
      nwis_data <- tryCatch(
        readNWISuv(siteNumbers=site_nums, parameterCd=p_code, startDate = asktimes[1], endDate = asktimes[2]),
        error=function(e) {
          if(isTRUE(verbose)) {
            message("NWIS says:  ", strsplit(as.character(e), "\n")[[1]][1])
          }
          data.frame()
        })
      if(ncol(nwis_data) == 0) {
        if(isTRUE(verbose)) {
          message("data are unavailable for ", paste0(p_code, "-", sites,collapse=" &/| "), ".")
        }
      } else {
        ignore.cols <- grepl('_cd', names(nwis_data)) # _cd includes flag, tz, agency. 
        everything <- '.dplyr.var'
        nwis_data <- nwis_data[, !ignore.cols] %>%
          rename(DateTime = dateTime) %>%
          select(site_no, DateTime, everything())
      }
    }
  )
  
  # loop through to filter and write the data
  file_paths <- c()
  site_no <- datetime <- tz_cd <- DateTime <- matches <- ends_with <- ".dplyr.var"
  if (ncol(nwis_data) != 0){
    if(isTRUE(verbose)) message("writing the downloaded data to file")
    un_sites <- unique(sites)
    for (i in 1:length(un_sites)){
      
      site <- parse_site_name(un_sites[i])
      site_data <- filter(nwis_data, site_no == site)
      
      if(nrow(site_data) > 0) {
        key <- value <- num_non_NAs <- '.dplyr.var'
        which.var <- select(site_data, -DateTime, -site_no) %>% 
          tidyr::gather() %>% group_by(key) %>% summarize(num_non_NAs = sum(!is.na(value))) %>% 
          filter(num_non_NAs==max(num_non_NAs)) %>% {.$key[1]}
        
        site_data <- select_(site_data, 'DateTime', which.var) %>%
          filter(DateTime >= truetimes[1] & DateTime < truetimes[2]) %>% # filter back to the times we actually want (only needed b/c of NWIS bug)
          u(c(NA,var_units)) %>% 
          setNames(c('DateTime', var))
      }

      if(nrow(site_data) > 0) {
        fpath <- write_ts(site_data, site=un_sites[i], var=var, src="nwis", folder, version=version)
        file_paths <- c(file_paths, fpath)
      } else {
        if(isTRUE(verbose)) message("no data found for site ", un_sites[i])
        # leave file_paths untouched if there's no new file
      }
    }
  }

  return(file_paths)
}

