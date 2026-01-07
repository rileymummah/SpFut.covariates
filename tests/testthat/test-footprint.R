test_that("download footprint works", {

  data(locs)

  dat <- get_footprint(locs[1:3,],
                       id.label = 'grid.id',
                       method = "fast")

  expect_equal(class(dat), 'data.frame')
})

test_that("footprint: method fails", {

  data(locs)

  expect_error(
    get_footprint(locs[1:3,],
                  id.label = 'grid.id',
                  method = "Fast")
  )
})
