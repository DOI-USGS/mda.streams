context("get_sites")

test_that("internal function get_sites works for multisites", {
  sites <- get_sites()
  expect_is(sites,'character')
  expect_true(length(sites) > 1)
  
  # this one yields a sporadic error, probably when SB goofs:
  #   Error in mapply(FUN = f, ..., SIMPLIFY = FALSE) : 
  #     zero-length inputs cannot be mixed with those of non-zero length
  # so i'm commenting it out.
  #expect_equal(get_sites(), list_sites())

  expect_error(get_sites(with_dataset_name=c("wtr_nwis","doobs_nwis")))
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
