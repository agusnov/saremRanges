.provinces_url <- paste0(
  "https://apis.datos.gob.ar/georef/api/v2.0/",
  "provincias.geojson"
)

.ecoregions_url <- paste0(
  "https://ide.ign.gob.ar/geoservicios/rest/services/ANIDA/",
  "ecologia_ecorr/MapServer/0/query?where=1%3D1&outFields=*",
  "&returnGeometry=true&f=geojson"
)

.geo_cache_file <- function(type) {
  file.path(.cache_dir(), paste0("argentina_", type, ".geojson"))
}

.download_reference_layer <- function(type, refresh = FALSE) {
  type <- match.arg(type, c("provinces", "ecoregions"))
  destination <- .geo_cache_file(type)
  if (!refresh && file.exists(destination) && file.info(destination)$size > 0) {
    return(destination)
  }
  url <- if (type == "provinces") .provinces_url else .ecoregions_url
  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = "saremRanges/0.2.0") |>
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 400) {
    stop("Could not download the ", type, " reference layer.", call. = FALSE)
  }
  writeBin(httr2::resp_body_raw(resp), destination)
  destination
}

.reference_layer <- function(type, refresh = FALSE) {
  x <- suppressWarnings(sf::st_read(
    .download_reference_layer(type, refresh), quiet = TRUE,
    stringsAsFactors = FALSE
  ))
  if (is.na(sf::st_crs(x))) sf::st_crs(x) <- 4326
  if (type == "provinces") {
    name <- intersect(c("nombre", "NOMBRE", "name"), names(x))[1]
  } else {
    name <- intersect(c("ECORREGION", "ecorregion", "nombre"), names(x))[1]
  }
  if (is.na(name)) stop("The reference layer has no name field.", call. = FALSE)
  x$area_name <- as.character(x[[name]])
  x
}

.normalize_area_name <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  tolower(gsub("[^a-zA-Z0-9]+", " ", trimws(x)))
}

.match_areas <- function(requested, type, refresh = FALSE) {
  layer <- .reference_layer(type, refresh)
  available <- layer$area_name
  idx <- match(.normalize_area_name(requested), .normalize_area_name(available))
  if (anyNA(idx)) {
    bad <- requested[is.na(idx)]
    stop(
      "Unknown ", if (type == "provinces") "province" else "ecoregion",
      ": ", paste(bad, collapse = ", "),
      ". Use ", if (type == "provinces") "sarem_provinces()" else "sarem_ecoregions()",
      " to list valid names.", call. = FALSE
    )
  }
  layer[idx, , drop = FALSE]
}

#' List Argentine provinces available for geographic filtering
#'
#' @param refresh Download the official reference layer again.
#' @return Character vector of province names.
#' @export
sarem_provinces <- function(refresh = FALSE) {
  sort(unique(.reference_layer("provinces", refresh)$area_name))
}

#' List Argentine terrestrial ecoregions available for filtering
#'
#' @param refresh Download the official reference layer again.
#' @return Character vector of ecoregion names.
#' @export
sarem_ecoregions <- function(refresh = FALSE) {
  sort(unique(.reference_layer("ecoregions", refresh)$area_name))
}

.one_area <- function(requested, type, crs, refresh = FALSE) {
  x <- .match_areas(requested, type, refresh)
  x <- sf::st_transform(x, crs)
  sf::st_sf(
    area_name = paste(requested, collapse = " | "),
    geometry = sf::st_sfc(sf::st_union(sf::st_geometry(x)), crs = crs)
  )
}

.filter_geography <- function(x, province = NULL, ecoregion = NULL,
                              clip = TRUE, refresh = FALSE) {
  if (is.null(province) && is.null(ecoregion)) return(x)
  if (!inherits(x, "sf")) stop("Geographic filtering requires an sf object.", call. = FALSE)
  out <- x
  requested <- list(province = province, ecoregion = ecoregion)
  types <- c(province = "provinces", ecoregion = "ecoregions")
  for (field in names(requested)) {
    values <- requested[[field]]
    if (is.null(values)) next
    area <- .one_area(values, types[[field]], sf::st_crs(out), refresh)
    hit <- lengths(sf::st_intersects(out, area)) > 0L
    out <- out[hit, , drop = FALSE]
    if (!nrow(out)) break
    if (isTRUE(clip)) {
      old_names <- names(out)
      out <- suppressWarnings(sf::st_intersection(out, sf::st_geometry(area)))
      out <- out[, intersect(old_names, names(out)), drop = FALSE]
    }
  }
  if (!nrow(out)) {
    stop("No downloaded ranges intersect the requested geographic filters.", call. = FALSE)
  }
  out
}
