.sarem_api_base <- "https://sarem-preprod.via418.com.ar/api"
.sarem_site <- "https://cma.sarem.org.ar"
.sarem_cache <- new.env(parent = emptyenv())

.sarem_request <- function(path, query = list(), attempts = 3) {
  req <- httr2::request(paste0(.sarem_api_base, path))
  if (length(query)) {
    query <- query[!vapply(query, function(x) is.null(x) || !length(x), logical(1))]
    if (length(query)) {
      query <- lapply(query, function(x) paste(x, collapse = ","))
      req <- do.call(httr2::req_url_query, c(list(req), query))
    }
  }

  req |>
    httr2::req_headers(
      Origin = .sarem_site,
      Accept = "application/json",
      `User-Agent` = "saremRanges/0.2.0"
    ) |>
    httr2::req_retry(
      max_tries = attempts,
      retry_on_failure = TRUE,
      backoff = function(tries) 1
    ) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()
}

.sarem_json <- function(path, query = list(), attempts = 3) {
  resp <- .sarem_request(path, query, attempts)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    stop("SAREM returned HTTP ", status, " for ", path, call. = FALSE)
  }
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

.rows_bind <- function(x) {
  if (!length(x)) {
    return(data.frame())
  }
  fields <- unique(unlist(lapply(x, names), use.names = FALSE))
  rows <- lapply(x, function(row) {
    missing <- setdiff(fields, names(row))
    row[missing] <- list(NA)
    row <- row[fields]
    row <- lapply(row, function(value) {
      if (is.null(value) || length(value) == 0L) {
        return(NA)
      }
      if (is.list(value)) {
        return(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"))
      }
      value
    })
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.species_page <- function(order = NULL, family = NULL, category = NULL) {
  ans <- .sarem_json(
    "/nativas",
    list(
      page = 1,
      limit = 500,
      orden = order,
      familia = family,
      categoria = category
    )
  )
  .rows_bind(ans$data)
}

.taxonomy_lookup <- function(refresh = FALSE, quiet = TRUE) {
  if (!refresh && exists("taxonomy", envir = .sarem_cache, inherits = FALSE)) {
    return(get("taxonomy", envir = .sarem_cache, inherits = FALSE))
  }

  filters <- .sarem_json("/nativas/filters", list(orden = ""))
  orders <- unlist(filters$orden, use.names = FALSE)
  result <- list()
  k <- 0L

  for (order in orders) {
    order_filters <- .sarem_json("/nativas/filters", list(orden = order))
    families <- unlist(order_filters$familia, use.names = FALSE)
    for (family in families) {
      tab <- .species_page(family = family)
      if (!nrow(tab)) next
      k <- k + 1L
      result[[k]] <- data.frame(
        id = as.character(tab$id),
        orden = order,
        familia = family,
        stringsAsFactors = FALSE
      )
    }
  }

  lookup <- if (length(result)) unique(do.call(rbind, result)) else data.frame()
  assign("taxonomy", lookup, envir = .sarem_cache)
  if (!quiet) cli::cli_alert_success("Taxonomic lookup updated.")
  lookup
}
