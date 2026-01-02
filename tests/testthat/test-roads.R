test_that("download roads works", {
  data(locs)

  expect_true(file.exists('../../data/roads.gdb'))

  expect_warning(dat <- get_roads(locs[1:3,],
                                  path = '../../data/',
                                  id.label = "grid.id"))

  expect_equal(class(dat), 'data.frame')
})
