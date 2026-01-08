test_that("download flowlines works", {
  data(locs)

  path <- 'C:/Users/rmummah/OneDrive - DOI/Documents/GitHub/species-futures/data/USA/'

  expect_true(file.exists(paste0(path, 'NHDPlusNationalData/waterbody.rds')))

  expect_warning(get_waterbodies(locs[1:3,],
                                 path = path,
                                 id.label = "grid.id"))

})
