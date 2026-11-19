.marine_families <- c(
  "Balaenidae", "Balaenopteridae", "Cetotheriidae", "Delphinidae",
  "Kogiidae", "Phocoenidae", "Physeteridae", "Pontoporiidae",
  "Ziphiidae", "Otariidae", "Phocidae"
)

#' List filters available from CMA-SAREM
#'
#' @param order Optional order used to obtain its families.
#' @return A named list containing orders, families, conservation categories,
#'   range types, and map layer types.
#' @export
sarem_filters <- function(order = NULL) {
  api <- .sarem_json(
    "/nativas/filters",
    list(orden = if (is.null(order)) "" else order)
  )
  list(
    orders = unlist(api$orden, use.names = FALSE),
    families = unlist(api$familia, use.names = FALSE),
    categories = unlist(api$categoria, use.names = FALSE),
    range_types = c("resident", "probable", "occasional", "historical"),
    layers = c(
      "range", "protected_area", "unprotected_area",
      "province", "ecoregion"
    )
  )
}

#' List mammal orders available from CMA-SAREM
#'
#' @return Character vector.
#' @rdname sarem_filters
#' @export
sarem_orders <- function() {
  sarem_filters()$orders
}

#' List mammal families
#'
#' @param order Optional taxonomic order.
#' @return Character vector.
#' @rdname sarem_filters
#' @export
sarem_families <- function(order = NULL) {
  if (is.null(order)) {
    unique(.taxonomy_lookup()$familia)
  } else {
    sarem_filters(order)$families
  }
}

.filter_environment <- function(x, environment) {
  environment <- match.arg(environment, c("terrestrial", "marine", "all"))
  if (environment == "all" || !nrow(x)) return(x)
  marine <- x$familia %in% .marine_families
  if (environment == "marine") x[marine, , drop = FALSE] else x[!marine, , drop = FALSE]
}

