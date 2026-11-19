.range_layer <- c(
  resident = "rango_residente",
  probable = "rango_probable",
  occasional = "rango_ocasional",
  historical = "rango_historico"
)

.other_layers <- c(
  protected_area = "protected_area",
  unprotected_area = "unprotected_area",
  province = "provincia",
  ecoregion = "ecoregion"
)

.cache_dir <- function() {
  path <- tools::R_user_dir("saremRanges", which = "cache")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

.download_map <- function(url, id, cache = TRUE, refresh = FALSE) {
  extension <- if (grepl("\\.kml($|\\?)", url, ignore.case = TRUE)) ".kml" else ".geojson"
  destination <- file.path(.cache_dir(), paste0("sarem_", id, extension))
  if (cache && !refresh && file.exists(destination) && file.info(destination)$size > 0) {
    return(destination)
  }

  req <- httr2::request(url) |>
    httr2::req_headers(`User-Agent` = "saremRanges/0.1.1") |>
    httr2::req_retry(
      max_tries = 3,
      retry_on_failure = TRUE,
      backoff = function(tries) 1
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status >= 400) stop("HTTP ", status, call. = FALSE)
  writeBin(httr2::resp_body_raw(resp), destination)
  destination
}

.read_map <- function(path) {
  suppressWarnings(sf::st_read(path, quiet = TRUE, stringsAsFactors = FALSE))
}

.requested_layer_types <- function(range_type, layers) {
  range_type <- match.arg(range_type, names(.range_layer), several.ok = TRUE)
  layers <- match.arg(
    layers,
    c("range", names(.other_layers)),
    several.ok = TRUE
  )
  types <- unname(.other_layers[intersect(layers, names(.other_layers))])
  if ("range" %in% layers) {
    types <- c(types, unname(.range_layer[range_type]))
  }
  unique(types)
}

.combine_ranges <- function(x) {
  if (!nrow(x)) return(x)
  is_range <- x$layer_type %in% unname(.range_layer)
  ranges <- x[is_range, , drop = FALSE]
  auxiliary <- x[!is_range, , drop = FALSE]
  if (!nrow(ranges)) return(x)

  groups <- split(seq_len(nrow(ranges)), ranges$id_sarem)
  combined <- lapply(groups, function(idx) {
    one <- ranges[idx[1], , drop = FALSE]
    geom <- sf::st_combine(sf::st_geometry(ranges[idx, , drop = FALSE]))
    sf::st_geometry(one) <- geom
    one
  })
  result <- do.call(rbind, combined)
  rownames(result) <- NULL
  if (nrow(auxiliary)) result <- rbind(result, auxiliary)
  result
}

#' Download CMA-SAREM mammal distribution maps
#'
#' @param order,family,genus,species,category Taxonomic or conservation
#'   filters passed to [sarem_species()].
#' @param environment One of `"terrestrial"`, `"marine"`, or `"all"`.
#' @param range_type One or more of `"resident"`, `"probable"`,
#'   `"occasional"`, or `"historical"`.
#' @param layers One or more of `"range"`, `"protected_area"`,
#'   `"unprotected_area"`, `"province"`, or `"ecoregion"`.
#' @param province,ecoregion Optional character vectors. Values within each
#'   filter are combined with OR; different filters are combined with AND.
#' @param clip Clip returned geometries to the requested geographic areas.
#' @param combine Combine all pieces of each requested distribution range into
#'   one feature per species. Auxiliary layers remain individual features.
#' @param cache Store downloaded map files in the user cache.
#' @param refresh Ignore cached maps and refresh the species taxonomy.
#' @param quiet Suppress progress messages.
#'
#' @return An `sf` object. The download report is stored as the
#'   `"sarem_download_log"` attribute and can be obtained with
#'   [sarem_download_log()].
#' @export
download_sarem_ranges <- function(
    order = NULL,
    family = NULL,
    genus = NULL,
    species = NULL,
    category = NULL,
    environment = c("terrestrial", "marine", "all"),
    range_type = "resident",
    layers = "range",
    province = NULL,
    ecoregion = NULL,
    clip = TRUE,
    combine = TRUE,
    cache = TRUE,
    refresh = FALSE,
    quiet = FALSE) {
  environment <- match.arg(environment)
  requested <- .requested_layer_types(range_type, layers)
  taxa <- sarem_species(
    order = order, family = family, genus = genus, species = species,
    category = category, environment = environment, refresh = refresh,
    quiet = TRUE
  )
  if (!nrow(taxa)) stop("No species matched the requested filters.", call. = FALSE)

  spatial <- list()
  logs <- vector("list", nrow(taxa))
  k <- 0L

  if (!quiet) cli::cli_alert_info("Downloading maps for {nrow(taxa)} species.")

  for (i in seq_len(nrow(taxa))) {
    taxon <- taxa[i, , drop = FALSE]
    log <- data.frame(
      id_sarem = as.character(taxon$id),
      nombre_cientifico = taxon$nombre_cientifico,
      mapa_url = taxon$mapa_url,
      estado = NA_character_,
      n_features_original = 0L,
      n_features_seleccionadas = 0L,
      mensaje = NA_character_,
      stringsAsFactors = FALSE
    )

    if (is.na(taxon$mapa_url) || !nzchar(taxon$mapa_url)) {
      log$estado <- "sin_url_mapa"
      logs[[i]] <- log
      next
    }

    tryCatch({
      path <- .download_map(taxon$mapa_url, taxon$id, cache, refresh)
      map <- .read_map(path)
      log$n_features_original <- nrow(map)
      if (!"layer_type" %in% names(map)) {
        stop("The map has no layer_type field.")
      }
      selected <- map[map$layer_type %in% requested, , drop = FALSE]
      log$n_features_seleccionadas <- nrow(selected)

      if (!nrow(selected)) {
        log$estado <- "sin_capa_solicitada"
      } else {
        selected$id_sarem <- as.character(taxon$id)
        selected$nombre_cientifico <- taxon$nombre_cientifico
        selected$nombre_comun <- taxon$nombre_comun
        selected$orden <- taxon$orden
        selected$familia <- taxon$familia
        selected$categoria <- taxon$categoria
        selected$tendencia_poblacional <- taxon$tendencia_poblacional
        selected$fuente <- "CMA-SAREM"
        selected$fecha_descarga <- as.character(Sys.Date())
        selected$mapa_url <- taxon$mapa_url
        k <- k + 1L
        spatial[[k]] <- selected
        log$estado <- "ok"
      }
    }, error = function(e) {
      log$estado <<- "error"
      log$mensaje <<- conditionMessage(e)
    })
    logs[[i]] <- log
  }

  report <- do.call(rbind, logs)
  if (!length(spatial)) {
    stop(
      "No spatial features could be downloaded. Inspect the requested filters.",
      call. = FALSE
    )
  }

  result <- do.call(rbind, spatial)
  rownames(result) <- NULL
  if (combine) result <- .combine_ranges(result)
  result <- .filter_geography(
    result, province = province, ecoregion = ecoregion,
    clip = clip, refresh = refresh
  )
  attr(result, "sarem_download_log") <- report
  attr(result, "sarem_filters") <- list(
    order = order, family = family, genus = genus, species = species,
    category = category, environment = environment, range_type = range_type,
    province = province, ecoregion = ecoregion, clip = clip
  )

  if (!quiet) {
    ok <- sum(report$estado == "ok", na.rm = TRUE)
    cli::cli_alert_success("{ok} species downloaded successfully.")
    if (ok < nrow(report)) {
      cli::cli_alert_warning("{nrow(report) - ok} species require review.")
    }
  }
  result
}

#' Extract the download report
#'
#' @param x An object returned by [download_sarem_ranges()].
#' @return A data frame with one row per requested species.
#' @export
sarem_download_log <- function(x) {
  log <- attr(x, "sarem_download_log", exact = TRUE)
  if (is.null(log)) {
    stop("The object does not contain a saremRanges download log.", call. = FALSE)
  }
  log
}
