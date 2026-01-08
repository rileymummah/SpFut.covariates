test_that("download elevation works", {

  data(locs)

  path <- 'C:/Users/rmummah/OneDrive - DOI/Documents/GitHub/species-futures/data/USA/'

  expect_true(file.exists(paste0(path,'elevation_tif')))

  dat <- get_elevation(locs[1:3,],
                       path = path,
                       id.label = 'grid.id',
                       method = "fast")

  expect_equal(class(dat), 'data.frame')

})
