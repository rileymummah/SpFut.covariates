test_that("download roads works", {
  data(locs)

  path <- 'C:/Users/rmummah/OneDrive - DOI/Documents/GitHub/species-futures/data/USA/'

  expect_true(file.exists(paste0(path,'roads.gdb')))

  expect_warning(dat <- get_roads(locs[1:3,],
                                  path = path,
                                  id.label = "grid.id"))

  expect_equal(class(dat), 'data.frame')
})
