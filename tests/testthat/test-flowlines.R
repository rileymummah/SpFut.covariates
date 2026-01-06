test_that("download flowlines works", {
  data(locs)

  expect_true(file.exists('../../../species-futures/data/USA/NHDPlusNationalData/waterbody.rds'))

  expect_warning(get_waterbodies(locs[1:3,],
                                 path = '../../../species-futures/data/USA/',
                                 id.label = "grid.id"))

})
