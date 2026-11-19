#' Export CMA-SAREM ranges for GIS software
#'
#' @param x An `sf` object returned by [download_sarem_ranges()].
#' @param filename Output filename.
#' @param format One of `"gpkg"`, `"geojson"`, or `"shp"`. When `NULL`, the
#'   format is inferred from the filename.
#' @param overwrite Replace an existing layer.
#'
#' @return The input object, invisibly.
#' @export
write_sarem_ranges <- function(
    x,
    filename,
    format = NULL,
    overwrite = FALSE) {
  if (!inherits(x, "sf")) stop("`x` must be an sf object.", call. = FALSE)
  if (is.null(format)) {
    ext <- tolower(tools::file_ext(filename))
    format <- switch(ext, gpkg = "gpkg", geojson = "geojson", shp = "shp", NA)
  }
  format <- match.arg(format, c("gpkg", "geojson", "shp"))
  driver <- c(gpkg = "GPKG", geojson = "GeoJSON", shp = "ESRI Shapefile")[[format]]

  if (format == "shp") {
    warning(
      "Shapefile truncates field names and has limited data-type support; ",
      "GeoPackage is recommended.",
      call. = FALSE
    )
  }
  sf::st_write(
    x,
    dsn = filename,
    driver = driver,
    delete_dsn = overwrite,
    quiet = TRUE
  )
  invisible(x)
}

