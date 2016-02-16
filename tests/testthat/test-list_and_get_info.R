context("get_sites")

test_that("internal function get_sites works for multisites", {
  sites <- mda.streams:::get_sites()
  expect_is(sites,'character')
  expect_true(length(sites) > 1)
  expect_equal(sort(mda.streams:::get_sites()), list_sites())
  expect_error(mda.streams:::get_sites(with_dataset_name=c("wtr_nwis","doobs_nwis")), "must have length 1")
})
  

context("list_datasets")

test_that("list_datasets ", {
  datasets <- list_datasets(site_name = 'nwis_01021050')
  expect_is(datasets ,'character')
  expect_true(length(datasets) > 1)
  
  expect_error(list_datasets(site_name = 'nwis_01021050asdf')) 
  expect_error(list_datasets(site_name = 'nwis_01021050',data_type = 'dog')) 
  expect_error(list_datasets(type = 'watershed')) 
  expect_error(list_datasets(type = 'ts')) 
  expect_is(list_datasets(site_name = 'nwis_01021050', data_type = 'ts'), 'character')
})

test_that("get_site_coords works", {
  # simple case - all sites recognized
  sc <- get_site_coords(c("nwis_312514091273400", "nwis_08062500"))
  expect_is(sc, "unitted_data.frame")
  expect_equal(names(sc), c("site_name","lat","lon"))
  expect_equal(unname(unitted::get_units(sc)), c("","degN","degE"))
  
  # some/all sites not recognized
  expect_warning(sc <- get_site_coords(c("nwis_312514091273400","himom", "nwis_08062500")), "unrecognized site_name")
  expect_equal(dim(sc), c(3,3))
  expect_warning(sc <- get_site_coords(c("nwis_312514091273400","himom", "nwis_08062500"), on_missing = "NA"), "unrecognized site_name")
  expect_equal(dim(sc), c(3,3))
  expect_warning(sc <- get_site_coords(c("nwis_312514091273400","himom", "nwis_08062500"), on_missing = "omit"), "unrecognized site_name")
  expect_equal(dim(sc), c(2,3))
  expect_warning(sc <- get_site_coords(c("himom", "hidad"), on_missing = "NA"), "unrecognized site_name")
  expect_equal(dim(sc), c(2,3))
  expect_warning(sc <- get_site_coords(c("himom", "hidad"), on_missing = "omit"), "unrecognized site_name")
  expect_equal(dim(sc), c(0,3))
})