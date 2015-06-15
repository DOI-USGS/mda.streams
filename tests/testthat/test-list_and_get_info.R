context("get_sites")

test_that("internal function get_sites works for multisites", {
  sites <- get_sites()
  expect_is(sites,'character')
  expect_true(length(sites) > 1)
  
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
