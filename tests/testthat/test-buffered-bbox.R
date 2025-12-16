test_that("bbox results in length=4", {
  data(locs)

  expect_equal(length(get_buffered_bbox(locs[1:3,])), 4)
})
