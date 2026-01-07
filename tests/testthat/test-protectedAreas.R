test_that("download protected areas works", {

  data(locs)

  expect_true(file.exists('../../../species-futures/data/USA/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb'))

  expect_warning(get_protected(locs[1:3,],
                               id.label = 'grid.id',
                               path = '../../../species-futures/data/USA/'))
})
