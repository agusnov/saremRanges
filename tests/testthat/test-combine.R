test_that("range pieces are combined by species", {
  skip_if_not_installed("sf")
  p1 <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2, byrow = TRUE
  )))
  p2 <- sf::st_polygon(list(matrix(
    c(2, 0, 3, 0, 3, 1, 2, 1, 2, 0),
    ncol = 2, byrow = TRUE
  )))
  x <- sf::st_sf(
    id_sarem = c("1", "1"),
    layer_type = c("rango_residente", "rango_residente"),
    geometry = sf::st_sfc(p1, p2, crs = 4326)
  )
  result <- saremRanges:::.combine_ranges(x)
  expect_equal(nrow(result), 1)
  expect_equal(as.character(sf::st_geometry_type(result)), "MULTIPOLYGON")
})

