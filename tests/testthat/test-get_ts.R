context('get_ts')

test_that('get_ts works', {
  
  # easy ones: no errors or warnings
  doobs <- get_ts('doobs_nwis', 'nwis_06601200')
  gpp <- get_ts('gpp_estBest', 'nwis_06601200')
  sw <- suppressWarnings(get_ts('sw_nldas', 'nwis_06601200')) # verify_ts for sw failed on test for timesteps
  
  # multiple sites gives error on purpose
  expect_error(suppressWarnings(get_ts('sitetime_calcLon', c('nwis_06601200','nwis_08062500'))), 
               "only one site_name is allowed")
  
  # multiple compatible tses works fine, no warning or error
  tses <- get_ts(c('sitetime_calcLon','doobs_nwis'), 'nwis_06601200')
  
  # multiple different-length, different-resolution tses give useful warnings
  expect_warning(get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200'))
  expect_warning(get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200', method='full_join'))
  hourlytohalf1 <- tryCatch(get_ts(c('doobs_nwis','sw_nldas'), 'nwis_06601200'), warning=function(w) w$message)
  hourlytohalf2 <- tryCatch(get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200', match_var='doobs_nwis'), warning=function(w) w$message)
  expect_equal(hourlytohalf1, hourlytohalf2)
  
  # multiple different-length, different-resolution tses give expecteed dims
  matchsw <- get_ts(c('sw_nldas','doobs_nwis'), 'nwis_06601200', quietly=TRUE)
  matchdoobs1 <- get_ts(c('doobs_nwis','sw_nldas'), 'nwis_06601200', quietly=TRUE)
  matchdoobs2 <- get_ts(c('sw_nldas','doobs_nwis'), match_var='doobs_nwis', 'nwis_06601200', quietly=TRUE)
  expect_equal(nrow(matchsw), nrow(sw))
  expect_equal(nrow(matchdoobs1), nrow(doobs))
  expect_equal(nrow(matchdoobs2), nrow(doobs))
  
  # realistic request
  tses1 <- get_ts(c('gpp_estBest','doobs_nwis','doamp_calcDAmp'), 'nwis_06601200', quietly=TRUE) # one var to condense
  tses2 <- get_ts(c('gpp_estBest','doobs_nwis','doamp_calcDAmp','baro_nldas'), 'nwis_06601200', quietly=TRUE) # >1 var to condense
  expect_equal(nrow(tses1), nrow(gpp))
  expect_equal(nrow(tses2), nrow(gpp))
  
  # working code to profile code & make it faster - most of the work had to be done in summarize_ts  
  # devtools::install_github("rstudio/profvis")
  # library(profvis)
  # profvis(tses <- get_ts(c('sitetime_calcLon','doobs_nwis'), 'nwis_06601200')) #92% in summarize_ts
  # profvis(tssum <- summarize_ts(rep(c("doobs_nwis", "wtr_nwis"), each=4), rep(c("nwis_01021050","nwis_01036390","nwis_01073389","nwis_notasite"), times=2), out='modal_timestep'))
  
})
