#' Query and filter the CMA-SAREM mammal list
#'
#' @param order Optional character vector of orders.
#' @param family Optional character vector of families.
#' @param genus Optional character vector of genera.
#' @param species Optional character vector of scientific names.
#' @param category Optional character vector of conservation categories.
#' @param environment One of `"terrestrial"`, `"marine"`, or `"all"`.
#' @param refresh Refresh the taxonomic lookup.
#' @param quiet Suppress progress messages.
#'
#' @return A data frame with one row per species.
#' @export
sarem_species <- function(
    order = NULL,
    family = NULL,
    genus = NULL,
    species = NULL,
    category = NULL,
    environment = c("terrestrial", "marine", "all"),
    refresh = FALSE,
    quiet = FALSE) {
  environment <- match.arg(environment)

  query_values <- if (!is.null(family)) {
    lapply(family, function(x) .species_page(family = x, category = category))
  } else if (!is.null(order)) {
    lapply(order, function(x) .species_page(order = x, category = category))
  } else if (!is.null(category) && length(category) > 1L) {
    lapply(category, function(x) .species_page(category = x))
  } else {
    list(.species_page(category = category))
  }

  tab <- unique(do.call(rbind, query_values))
  rownames(tab) <- NULL
  if (!nrow(tab)) return(tab)

  taxonomy <- .taxonomy_lookup(refresh = refresh, quiet = quiet)
  idx <- match(as.character(tab$id), taxonomy$id)
  tab$orden <- taxonomy$orden[idx]
  tab$familia <- taxonomy$familia[idx]

  names(tab)[names(tab) == "categoria_2024"] <- "categoria"
  names(tab)[names(tab) == "mapa"] <- "mapa_url"

  if (!is.null(order)) tab <- tab[tab$orden %in% order, , drop = FALSE]
  if (!is.null(family)) tab <- tab[tab$familia %in% family, , drop = FALSE]
  if (!is.null(category)) tab <- tab[tab$categoria %in% category, , drop = FALSE]

  scientific <- tab$nombre_cientifico
  if (!is.null(genus)) {
    tab <- tab[sub(" .*", "", scientific) %in% genus, , drop = FALSE]
    scientific <- tab$nombre_cientifico
  }
  if (!is.null(species)) {
    tab <- tab[scientific %in% species, , drop = FALSE]
  }
  tab <- .filter_environment(tab, environment)

  preferred <- c(
    "id", "nombre_cientifico", "nombre_comun", "orden", "familia",
    "categoria", "tendencia_poblacional", "mapa_url", "thumbnail"
  )
  tab <- tab[, intersect(preferred, names(tab)), drop = FALSE]
  rownames(tab) <- NULL

  if (!quiet) {
    cli::cli_alert_info("{nrow(tab)} species matched the filters.")
  }
  tab
}

