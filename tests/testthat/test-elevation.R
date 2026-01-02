test_that("download elevation works", {

  data(locs)

  expect_true(file.exists('../../data/elevation_tif'))

  dat <- get_elevation(locs[1:3,],
                       path = '../../data/',
                       id.label = 'grid.id',
                       method = "fast")

  expect_equal(class(dat), 'data.frame')

})
