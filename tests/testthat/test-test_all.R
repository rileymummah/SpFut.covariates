
data(locs)

locs <- locs[1:20,]
path <- "../species-futures/data/USA/"
id.label <- "grid.id"

tmp <- cleaning(locs, id.label, path)

tmp1 <- get_buffered_bbox(locs)



# # test population
# dat <- get_population(locs, id.label)
#
# # test human footprint
# dat <- get_footprint(locs, id.label)
#
# # test traveltime
# dat <- get_traveltime(locs, id.label)
#
# # test soil
# dat <- get_soil(locs, id.label)

# test climate
# dat <- get_climate(locs, id.label)

# test landcover
# dat <- get_landcover(locs, path, id.label)
#
# # test elevation
# dat <- get_elevation(locs, path, id.label)
#
# # test protected areas
# dat <- get_protected(locs, path, id.label)
#
# # test flowlines
# dat <- get_flowlines(locs, path, id.label)
#
# # test water bodies
# dat <- get_waterbodies(locs, path, id.label)
#
# # test roads
# dat <- get_roads(locs, path, id.label)
#
