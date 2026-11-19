test_that("requested layer types are translated correctly", {
  expect_equal(
    saremRanges:::.requested_layer_types("resident", "range"),
    "rango_residente"
  )
  expect_setequal(
    saremRanges:::.requested_layer_types(
      c("resident", "historical"),
      c("range", "protected_area")
    ),
    c("rango_residente", "rango_historico", "protected_area")
  )
})

test_that("marine families are classified separately", {
  x <- data.frame(
    familia = c("Cricetidae", "Delphinidae", "Otariidae"),
    stringsAsFactors = FALSE
  )
  terrestrial <- saremRanges:::.filter_environment(x, "terrestrial")
  marine <- saremRanges:::.filter_environment(x, "marine")
  expect_equal(terrestrial$familia, "Cricetidae")
  expect_setequal(marine$familia, c("Delphinidae", "Otariidae"))
})

test_that("API rows containing NULL fields are normalized", {
  x <- list(
    list(id = "1", nombre = "Species one", mapa = NULL),
    list(id = "2", nombre = "Species two", mapa = "map.geojson")
  )
  result <- saremRanges:::.rows_bind(x)
  expect_equal(nrow(result), 2)
  expect_true(is.na(result$mapa[1]))
  expect_equal(result$mapa[2], "map.geojson")
})
