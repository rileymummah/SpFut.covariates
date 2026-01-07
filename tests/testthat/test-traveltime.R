test_that("download traveltime works", {

  data(locs)

  dat <- get_traveltime(locs[1:3,],
                        id.label = 'grid.id',
                        size = 4,
                        up = T,
                        path = tempdir(),
                        method = "fast")

  expect_equal(class(dat), 'data.frame')
})

test_that("traveltime: method fails", {

  data(locs)

  expect_error(
    get_traveltime(locs[1:3,],
                   id.label = 'grid.id',
                   size = 4,
                   up = T,
                   path = tempdir(),
                   method = "Fast")
  )
})

test_that("traveltime: up = FALSE", {

  data(locs)

  dat <- get_traveltime(locs[1:3,],
                        id.label = 'grid.id',
                        size = 4,
                        up = F,
                        path = tempdir(),
                        method = "fast")

  expect_equal(class(dat), 'data.frame')
})

test_that("traveltime: size = 2", {

  data(locs)

  dat <- get_traveltime(locs[1:3,],
                        id.label = 'grid.id',
                        size = 2,
                        up = T,
                        path = tempdir(),
                        method = "fast")

  expect_equal(class(dat), 'data.frame')
})
