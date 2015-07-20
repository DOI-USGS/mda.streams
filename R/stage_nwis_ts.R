#' @title stage nwis data into a file
#' @description get data from nwis and return created file handle
#'   
#' @param sites a character vector of valid NWIS site IDs
#' @param var short name of variable as in
#'   \code{unique(get_var_src_codes(out="var"))}
#' @param times a length 2 vector of text dates in YYYY-MM-DD format
#' @param folder a folder to place the file outputs in (defaults to temp 
#'   directory)
#' @param verbose provide verbose output (currently not implemented)
#' @return a character vector of file handles
#' @importFrom dataRetrieval constructNWISURL importRDB1
#' @importFrom unitted u
#'   
#' @examples
#' 
#' \dontrun{
#' files <- stage_nwis_ts(sites = c("nwis_06893820","nwis_01484680"), 
#'   var = "doobs", times = c('2014-01-01','2014-01-02'), verbose=TRUE)
#' head(read_ts(files[1]))
#' 
#' # par is unavailable for all sites, so returns NULL
#' stage_nwis_ts(sites = get_sites(), var = "par",
#'   times = c('2014-01-01', '2014-01-03'), verbose=TRUE) 
#' }
#' @export
stage_nwis_ts <- function(sites, var, times, folder = tempdir(), verbose = FALSE){

  # # diagnose an apparent issue with NWIS - many sites return values with a 1-hour offset from what has been requested in UTC
  # all_sites <- get_sites()
  # all_files <- stage_nwis_ts(sites = all_sites, var = "doobs", times = c('2014-06-01','2014-06-02'), verbose=TRUE)
  # tz_00 <- sapply(setNames(all_files, mda.streams:::parse_ts_path(all_files, "site_name")), function(file) format(unitted::v(read_ts(file)[1,"DateTime"]), "%H") == "00")
  # tz_01 <- sapply(setNames(all_files, mda.streams:::parse_ts_path(all_files, "site_name")), function(file) format(unitted::v(read_ts(file)[1,"DateTime"]), "%H") == "01")
  # tz_other <- sapply(setNames(all_files, mda.streams:::parse_ts_path(all_files, "site_name")), function(file) !(format(unitted::v(read_ts(file)[1,"DateTime"]), "%H") %in% c("00","01")))
  # write.table(mda.streams:::parse_ts_path(all_files[tz_00], "sitenum"), "C:/Users/aappling/desktop/right_time.txt", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
  # write.table(mda.streams:::parse_ts_path(all_files[tz_01], "sitenum"), "C:/Users/aappling/desktop/wrong_time.txt", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
  # write.table(mda.streams:::parse_ts_path(all_files[tz_other], "sitenum"), "C:/Users/aappling/desktop/other_time.txt", sep="\n", col.names=FALSE, row.names=FALSE, quote=FALSE)
  
  # process inputs
  if(length(var) > 1) stop("one var at a time, please")
  vars <- var # need a renamed version for get_var_src_codes filter on var
  p_code <- '.dplyr_var'
  p_code <- get_var_src_codes(src=="nwis",var%in%vars,!is.na(p_code),out="p_code")
  var_units <- get_var_src_codes(var==vars, src=='nwis', out='units')
  
  # request times with 1-hour buffer to deal with NWIS bug. specify times as UTC
  # (see http://waterservices.usgs.gov/rest/IV-Service.html#Specifying)
  truetimes <- as.POSIXct(paste0(times, " 00:00:00"), tz="UTC")
  asktimes <- format(truetimes + as.difftime(c(-1, 0), units="hours"), "%Y-%m-%dT%H:%MZ")

  # download the data. rather than using xml (as in readNWISuv), use tsv because
  # it's much faster. the drawback is that if the file is incomplete, we won't
  # be told. that's life.
  if(isTRUE(verbose)) message("requesting data from NWIS for p_code ", p_code)
  url <- constructNWISURL(siteNumber = parse_site_name(sites), parameterCd = p_code, startDate = asktimes[1], endDate = asktimes[2], service="uv", format="tsv")
  nwis_data <- tryCatch(
    importRDB1(url, asDateTime = TRUE),
    error=function(e) {
      if(isTRUE(verbose)) {
        message("data are unavailable for ", paste0(p_code, "-", sites,collapse=" &/| "), ". NWIS says:  ", strsplit(as.character(e), "\n")[[1]][1])
      }
      data.frame()
    })
  
  # loop through to filter and write the data
  file_paths <- c()
  site_no <- datetime <- tz_cd <- DateTime <- matches <- ends_with <- ".dplyr.var"
  if (ncol(nwis_data)!=0){
    if(isTRUE(verbose)) message("writing the downloaded data to file")
    un_sites <- unique(sites)
    for (i in 1:length(un_sites)){
      
      site <- parse_site_name(un_sites[i])
      datacol <- names(nwis_data)[5]
      site_data <- filter(nwis_data, site_no == site) %>%
        mutate(DateTime = as.POSIXct(datetime, tz = ifelse(tz_cd=="UTC", "GMT", tz_cd))) %>%
        select_('DateTime', datacol) %>%
        setNames(c("DateTime",var)) %>%
        filter(DateTime >= truetimes[1] & DateTime < truetimes[2]) %>% # filter back to the times we actually want (only needed b/c of NWIS bug)
        u(c(NA,var_units))

      if(nrow(site_data) > 0) {
        fpath <- write_ts(site_data, site=un_sites[i], var=var, src="nwis", folder)
        file_paths <- c(file_paths, fpath)
      } else {
        if(isTRUE(verbose)) message("no data found for site ", un_sites[i])
        # leave file_paths untouched if there's no new file
      }
    }
  }

  return(file_paths)
}

