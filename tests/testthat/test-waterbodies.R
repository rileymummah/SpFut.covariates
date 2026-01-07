test_that("download waterbodies works", {
  data(locs)

  expect_true(file.exists('../../../species-futures/data/USA/NHDPlusNationalData/waterbody.rds'))

  expect_warning(get_waterbodies(locs[1:3,],
                                 path = '../../../species-futures/data/USA/',
                                 id.label = "grid.id",
                                 size = list(verysmall = c(0, 0.01),
                                             small = c(0.01, 0.02),
                                             medium = c(0.02, 0.05),
                                             large = c(0.05, 10),
                                             verylarge = c(10, Inf))))

})

test_that("check thresholds", {
  data(locs)

  expect_warning(get_waterbodies(locs[1:3,],
                                 path = '../../../species-futures/data/USA/',
                                 id.label = "grid.id",
                                 size = list(cat1 = c(0, 0.05),
                                             cat2 = c(0.05, 0.1),
                                             cat3 = c(0.1, Inf))))
})
