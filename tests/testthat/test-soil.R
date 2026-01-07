test_that("download soil works", {

  data(locs)

  dat <- get_soil(locs[1:3,],
                  id.label = 'grid.id',
                  var = c("bdod", "cfvo", "clay", "nitrogen", "ocd",
                          "phh2o", "sand", "silt", "soc"),
                  depth = 5,
                  stat = "mean",
                  name = "",
                  method = "fast")

    expect_equal(class(dat), 'data.frame')
})
