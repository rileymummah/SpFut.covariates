test_that("download protected areas works", {

  data(locs)

  path <- 'C:/Users/rmummah/OneDrive - DOI/Documents/GitHub/species-futures/data/USA/'

  expect_true(file.exists(paste0(path,'PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb')))

  expect_warning(get_protected(locs[1:3,],
                               id.label = 'grid.id',
                               path = path))
})
