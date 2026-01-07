test_that("download population works", {

  data(locs)

  dat <- get_population(locs[1:3,],
                        id.label = "grid.id",
                        year = 2010,
                        res = 0.5,
                        method = "fast")

  expect_equal(class(dat), "data.frame")
})

test_that("population: method fail", {

  data(locs)

  expect_error(
    get_population(locs[1:3,],
                   id.label = "grid.id",
                   year = 2010,
                   res = 0.5,
                   method = "Fast")
  )
})
