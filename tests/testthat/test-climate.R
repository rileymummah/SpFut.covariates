test_that("download climate works", {

  data(locs)

  dat <- get_climate(locs[1:3,], "grid.id")

  expect_equal(class(dat), 'data.frame')
})
