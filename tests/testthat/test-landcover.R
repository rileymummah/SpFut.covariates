test_that("download landcover works", {

  data(locs)

  expect_true(file.exists('../../../species-futures/data/USA/land_cover_2020v2_30m_tif/'))

  dat <- get_landcover(locs[1:3,],
                       path = "../../../species-futures/data/USA/",
                       id.label = "grid.id")

  expect_equal(class(dat), 'data.frame')
})
