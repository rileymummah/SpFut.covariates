test_that("download roads works", {
  data(locs)

  expect_true(file.exists('../../../species-futures/data/USA/roads.gdb'))

  expect_warning(dat <- get_roads(locs[1:3,],
                                  path = '../../../species-futures/data/USA/',
                                  id.label = "grid.id"))

  expect_equal(class(dat), 'data.frame')
})
