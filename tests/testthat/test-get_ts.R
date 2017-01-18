context('get_ts')

test_that('get_ts works', {
  
  # easy ones: no errors or warnings
  doobs <- suppressWarnings(get_ts('doobs_nwis', 'nwis_06601200')) # verify_ts for doobs failed on test for units 
  sw <- suppressWarnings(get_ts('sw_nldas', 'nwis_06601200')) # verify_ts for sw failed on test for timesteps
  gpp <- suppressWarnings(get_ts('gpp_estBest', 'nwis_03027500', version='tsv')) # verify_ts for gpp failed on test for units 
  
  # multiple sites gives error on purpose
  expect_error(suppressWarnings(get_ts('sitetime_calcLon', c('nwis_06601200','nwis_08062500'))), 
               "only one site_name is allowed")
  
  # multiple compatible tses works fine, no warning or error
  tses <- suppressWarnings(get_ts(c('sitetime_calcLon','doobs_nwis'), 'nwis_06601200', version='rds')) # verify_ts for doobs failed on test for units
  
  # multiple different-length, different-resolution tses give useful warnings
  expect_warning(get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200'))
  expect_warning(get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200', method='full_join'))
  hourlytohalf1 <- tryCatch(get_ts(c('doobs_nwis','sw_nldas'), 'nwis_06601200'), warning=function(w) w$message)
  hourlytohalf2 <- tryCatch(get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200', match_var='doobs_nwis'), warning=function(w) w$message)
  # expect_equal(hourlytohalf1, hourlytohalf2) # doesn't work until the verify_ts warnings go away and we can compare warnings data.frames
  
  # multiple different-length, different-resolution tses give expecteed dims
  matchsw <- suppressWarnings(get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200', quietly=TRUE))
  matchdoobs1 <- suppressWarnings(get_ts(c('doobs_nwis','sw_nldas'), 'nwis_06601200', quietly=TRUE))
  matchdoobs2 <- suppressWarnings(get_ts(c('sw_nldas','doobs_nwis'), match_var='doobs_nwis', 'nwis_06601200', quietly=TRUE))
  expect_equal(nrow(matchsw), nrow(sw))
  expect_equal(nrow(matchdoobs1), nrow(doobs))
  expect_equal(nrow(matchdoobs2), nrow(doobs))
  
  # realistic request
  tses1 <- get_ts(c('velocdaily_calcDMean','doobs_nwis','doamp_calcDAmp'), 'nwis_03027500', quietly=TRUE) # one var to condense
  tses2 <- get_ts(c('velocdaily_calcDMean','doobs_nwis','doamp_calcDAmp','baro_nldas'), 'nwis_03027500', version='rds', quietly=TRUE) # >1 var to condense
  expect_equal(nrow(tses1), nrow(tses2))
  
  # working code to profile code & make it faster - most of the work had to be done in summarize_ts  
  # devtools::install_github("rstudio/profvis")
  # library(profvis)
  # profvis(tses <- get_ts(c('sitetime_calcLon','doobs_nwis'), 'nwis_06601200')) #92% in summarize_ts
  # profvis(tssum <- summarize_ts(rep(c("doobs_nwis", "wtr_nwis"), each=4), rep(c("nwis_01021050","nwis_01036390","nwis_01073389","nwis_notasite"), times=2), out='modal_timestep'))
  
})
