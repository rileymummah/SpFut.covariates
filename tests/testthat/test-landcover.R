test_that("download landcover works", {

  data(locs)

  path <- 'C:/Users/rmummah/OneDrive - DOI/Documents/GitHub/species-futures/data/USA/'

  expect_true(file.exists(paste0(path,'land_cover_2020v2_30m_tif/')))

  dat <- get_landcover(locs[1:3,],
                       path = path,
                       id.label = "grid.id")

  expect_equal(class(dat), 'data.frame')
})
