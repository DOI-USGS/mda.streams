context("get_sites")

test_that("internal function get_sites works for multisites", {
  sites <- get_sites()
  expect_is(sites,'character')
  expect_true(length(sites) > 1)
  
  expect_equal(get_sites(), list_sites())
  expect_error(get_sites(with_var_src=c("wtr_nwis","doobs_nwis")))
})
  

context("list_datasets")

test_that("list_datasets ", {
  datasets <- list_datasets(site_name = 'nwis_01021050')
  expect_is(datasets ,'character')
  expect_true(length(datasets) > 1)
  
  expect_error(list_datasets(site_name = 'nwis_01021050asdf')) 
  expect_error(list_datasets(site_name = 'nwis_01021050',type = 'dog')) 
  expect_error(list_datasets(type = 'watershed')) 
  expect_error(list_datasets(type = 'ts')) 
  expect_is(list_datasets(site_name = 'nwis_01021050', type = 'ts'), 'character')
})
