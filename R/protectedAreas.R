get_protected <- function(locs,
                          path,
                          id.label = "id") {

  tmp <- cleaning(locs, id.label, path)
  locs <- tmp$locs
  path <- tmp$path

  if ("PADUS4_0Geodatabase" %in% list.files(path) == F) {
    stop("Protected areas file does not exist. \nStep 1: Download PADUS4_0Geodatabase.zip from https://www.usgs.gov/programs/gap-analysis-project/science/pad-us-data-download \nStep 2: Unzip, move PADUS4_0Geodatabase folder to path, and try again")
  }


  pad <- sf::st_read(paste0(path, "/PADUS4_0Geodatabase/PADUS4_0_Geodatabase.gdb/"),
                     layer='PADUS4_0Designation') %>%

    # Drop Z or M dimension
    st_zm() #%>%

  # Transform CRS to 3857
  #st_transform(3857)

  # these are good
  valid <- pad[which(st_is_valid(pad) == T),]

  # these are bad
  invalid <- pad[which(st_is_valid(pad) == F),]
  invalid1 <- st_make_valid(invalid)

  # these are very bad
  other <- pad[which(is.na(st_is_valid(pad))),]
  # they are concentrated in the west but spread across CONUS
  #
  # tmp <- st_cast(other, "GEOMETRYCOLLECTION") %>% st_collection_extract("POLYGON")
  # tmp <- st_cast(other, "MULTISTRING")
  # tmp <- st_as_sfc(other) %>% st_cast("MULTISTRING")
  #
  #
  # load("data/species/Arizona Toad/all-data.rdata")
  # ggplot() +
  #   geom_sf(data = region$conus) +
  #   geom_sf(data = other)
  #
  #other1 <- st_make_valid(other)
  #other1 <- st_cast(other, "GEOMETRYCOLLECTION")
  # DONT KNOW WHAT TO DO ABOUT THESE
  cat("Warning: There are ", nrow(other), " shapes that are not useable")

  # project locs
  g1 <- locs %>%
    select(id, geometry) %>%
    st_transform(st_crs(pad))
  bb <- get_buffered_bbox(g1)

  #
  pad1 <- bind_rows(valid, invalid1) %>%
    st_crop(bb)

  if (nrow(pad1) == 0) {
    allnms <- paste0("protected.", c("FED", "LOC", "UNK", "STAT", "NGO", "DIST", "PVT", "JNT", "TRIB", "TOT"))

    g2 <- as.data.frame(matrix(ncol = length(allnms),
                               nrow = nrow(g1),
                               data = 0))
    colnames(g2) <- allnms

    g2$id <- g1$id

    g2 <- select(g2, id, all_of(allnms))

  } else {

    # combine each management type into one row
    pad1 <- pad1 %>%
      group_by(Mang_Type) %>%
      summarize(geometry = st_union(SHAPE))

    # get intersection
    tmp <- st_intersection(g1, pad1)
    tmp$overlap.area <- as.numeric(st_area(tmp))

    # add up protected area of each type within each cell
    tmp <- tmp %>%
      group_by(id, Mang_Type) %>%
      summarize(protected = sum(overlap.area),
                .groups = "drop") %>%
      st_drop_geometry()

    g2 <- full_join(g1, as.data.frame(tmp), by = "id") %>%
      st_drop_geometry() %>%
      pivot_wider(names_from = Mang_Type, values_from = protected)
    g2$`NA` <- NULL

    colnames(g2)[2:ncol(g2)] <- paste0("protected.", colnames(g2)[2:ncol(g2)])

    g2[is.na(g2)] <- 0
    g2$protected.TOT <- rowSums(g2[,2:ncol(g2)])

    g2 <- locs %>%
      st_drop_geometry() %>%
      #select(id) %>%
      full_join(g2, by = "id")

    # make sure all columns exist
    allnms <- paste0("protected.", c("FED", "LOC", "UNK", "STAT", "NGO", "DIST", "PVT", "JNT", "TRIB", "TOT"))
    missingnms <- allnms[-which(allnms %in% colnames(g2))]

    if (length(missingnms) > 0) {
      for (m in 1:length(missingnms)) {
        g2[,(ncol(g2)+1)] <- NA
        colnames(g2)[ncol(g2)] <- missingnms[m]
      }
    }


    g2[is.na(g2)] <- 0

    g2 <- select(g2, all_of(id.label), starts_with("protected."))


  }



  return(g2)

}
