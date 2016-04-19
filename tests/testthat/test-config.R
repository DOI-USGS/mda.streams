context("config")

test_that("config files can be staged", {
  
  config_file <- stage_metab_config(
    tag="0.0.1", strategy="try stage_metab_config", site="nwis_04087142", filename=tempfile())
  expect_is(read.table(config_file, header=TRUE, stringsAsFactors=FALSE), "data.frame")
  
  config_object <- stage_metab_config(
    tag="0.0.1", strategy="test", site=c("nwis_01433500", "nwis_01454700", "nwis_01463500"), filename=NULL,
    omit_incomplete=FALSE)
  expect_equal(3, nrow(config_object))
  
})


test_that("config files can be verified", {
  
  egconfig <- stage_metab_config(
    tag="0.0.1", strategy="try stage_metab_config", site="nwis_04087142", filename=NULL)
  expect_true(verify_config(egconfig))
  expect_false(suppressWarnings(verify_config(egconfig[,c(1,3:5,9:30,9)])))
  expect_warning(verify_config(egconfig[,c(1,3:5,9:30,9)]))
  expect_error(testthat::expect_warning(verify_config(egconfig[,c(1,3:5,9:30,9)], on_fail=stop)))
  
})


test_that("config files can be converted to data & models", {
  
  dontrun <- function() {
    # get a config (would be better to create right now)
    config <-
      structure(list(tag = "0.0.2", strategy = "local_makefile_run", 
                     date = "2015-07-14 15:49:40", model = "metab_mle", model_args = "list()", 
                     site = "nwis_01645762", sitetime.type = "ts", sitetime.site = "nwis_01645762", 
                     sitetime.src = "calcLon", sitetime.logic = "priority local", 
                     doobs.type = "ts", doobs.site = "nwis_01645762", doobs.src = "nwis", 
                     doobs.logic = "priority local", dosat.type = "ts", dosat.site = "nwis_01645762", 
                     dosat.src = "calcGGbts", dosat.logic = "priority local", 
                     depth.type = "ts", depth.site = "nwis_01645762", depth.src = "calcDisch", 
                     depth.logic = "priority local", wtr.type = "ts", wtr.site = "nwis_01645762", 
                     wtr.src = "nwis", wtr.logic = "priority local", par.type = "ts", 
                     par.site = "nwis_01645762", par.src = "calcLat", par.logic = "priority local"), 
                .Names = c("tag", 
                           "strategy", "date", "model", "model_args", "site", "sitetime.type", 
                           "sitetime.site", "sitetime.src", "sitetime.logic", "doobs.type", 
                           "doobs.site", "doobs.src", "doobs.logic", "dosat.type", "dosat.site", 
                           "dosat.src", "dosat.logic", "depth.type", "depth.site", "depth.src", 
                           "depth.logic", "wtr.type", "wtr.site", "wtr.src", "wtr.logic", 
                           "par.type", "par.site", "par.src", "par.logic"), row.names = 40L, class = "data.frame")
    
    # config_to_data
    library(streamMetabolizer)
    data <- config_to_data(config, 1, metab_night, list())
    data_ply <- data[which(as.Date(v(data$local.time)+as.difftime(12,units="hours")) %in% as.Date(paste0("2014-04-",c("10","11","12")))),]
    data_ply <- u(data_ply, get_units(data_ply) %>% replace(., which(.=="mg L^-1"), "mgO2 L^-1"))
    
    # data works for model
    mm <- metab_night(v(data_ply))
    
    # config_to_metab
    data <- config_to_metab(config, 1, metab_night)
  }
  
})