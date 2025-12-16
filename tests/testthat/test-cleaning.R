test_that("multiplication works", {
  data(locs)

  tmp <- cleaning(locs[1:3,],
                  id.label = 'grid.id',
                  path = "../species-futures/data/USA/")

  expect_equal(class(tmp),'list')
})
