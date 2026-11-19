.clean_trait <- function(value) {
  if (is.null(value) || !length(value)) return(NA_character_)
  if (is.list(value) && !is.data.frame(value)) {
    if (all(vapply(value, function(z) is.atomic(z) && length(z) <= 1L, logical(1)))) {
      value <- unlist(value, recursive = FALSE, use.names = FALSE)
    } else {
      return(as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null")))
    }
  }
  value <- as.character(value)
  value <- trimws(value[!is.na(value) & nzchar(trimws(value)) & value != "NA"])
  if (!length(value)) NA_character_ else paste(value, collapse = "; ")
}

.pluck_trait <- function(x, ...) {
  keys <- list(...)
  value <- x
  for (key in keys) {
    if (!is.list(value) || is.null(value[[key]])) return(NA_character_)
    value <- value[[key]]
  }
  .clean_trait(value)
}

.first_trait <- function(...) {
  values <- list(...)
  for (value in values) {
    cleaned <- .clean_trait(value)
    if (!is.na(cleaned) && nzchar(cleaned)) return(cleaned)
  }
  NA_character_
}

.habitat_traits <- c(
  habitat_arbustales = "arbustales",
  habitat_arroyos_rios = "arroyos_o_rios",
  habitat_canales_artificiales = "canales_artificiales",
  habitat_costero = "costero",
  habitat_cultivos_agricolas = "cultivos_agricolas",
  habitat_embalses_diques = "embalses_o_diques",
  habitat_estepas = "estepas",
  habitat_forestaciones = "forestaciones",
  habitat_lagos_lagunas = "lagos_o_lagunas",
  habitat_oceanico = "oceanico",
  habitat_palustre = "palustre",
  habitat_pastizales = "pastizales",
  habitat_pasturas_ganaderas = "pasturas_ganaderas",
  habitat_rupestre = "rupestre",
  habitat_selvas_bosques = "selvas_o_bosques",
  habitat_urbano_periurbano = "urbano_o_periurbano"
)

.normalize_traits <- function(ficha) {
  tax <- ficha$taxonomia %||% list()
  eco <- ficha$ecologicos %||% list()
  morph <- ficha$morfometria %||% list()
  catg <- ficha$categorizacion %||% list()
  evaluation <- ficha$evaluacion %||% list()
  row <- list(
    id_sarem = .clean_trait(ficha$id),
    nombre_cientifico = .first_trait(tax$especie, tax$nombre_cientifico),
    nombre_cientifico_con_autoridad = .clean_trait(tax$nombre_cientifico),
    nombre_comun = .clean_trait(tax$nombre_comun),
    orden = .clean_trait(tax$orden), familia = .clean_trait(tax$familia),
    peso_general = .first_trait(morph$peso_web, morph$peso),
    peso_machos = .first_trait(morph$peso_machos, morph$peso_macho),
    peso_hembras = .first_trait(morph$peso_hembras, morph$peso_hembra),
    peso_comentarios = .clean_trait(morph$peso_comentarios),
    habito = .clean_trait(eco$habitos),
    habitos_especializados = .clean_trait(eco$habitos_especializados),
    habitos_especializados_otro = .clean_trait(eco$habitos_especializados_otro),
    dieta_general = .clean_trait(eco$dieta),
    dieta_especializada = .clean_trait(eco$dieta_especializada),
    dieta_especializada_otro = .clean_trait(eco$dieta_especializada_otro),
    patron_actividad = .clean_trait(eco$patron_de_actividad),
    gregariedad = .clean_trait(eco$gregariedad),
    tamanio_grupo = .first_trait(eco$tamanio_de_grupo, eco$tamanio_grupo),
    tamanio_grupo_comentario = .clean_trait(eco$tamanio_de_grupo_comentario),
    area_accion = .clean_trait(eco$area_de_accion),
    tolerancia_ambientes_antropizados = .clean_trait(eco$tolerancia_a_hab_antropizados),
    aspectos_reproductivos = .clean_trait(eco$aspectos_reproductivos),
    categoria_sarem_2024 = .pluck_trait(catg, "_2024", "categoria"),
    criterio_sarem_2024 = .pluck_trait(catg, "_2024", "criterio"),
    categoria_sarem_2019 = .pluck_trait(catg, "_2019", "categoria"),
    criterio_sarem_2019 = .pluck_trait(catg, "_2019", "criterio"),
    categoria_uicn = .pluck_trait(catg, "iucn", "categoria"),
    anio_uicn = .pluck_trait(catg, "iucn", "anio"),
    tendencia_poblacional = .clean_trait(evaluation$tendencia_poblacional),
    tendencia_poblacional_comentario = .clean_trait(evaluation$tendencia_poblacional_comentario),
    comentario_habito_habitat_dieta = .clean_trait(eco$habito_habitat_dieta_comentarios),
    fuente_ficha = paste0(.sarem_site, "/ficha/", ficha$id),
    fuente_api = paste0(.sarem_api_base, "/nativas/", ficha$id),
    fecha_descarga = as.character(Sys.Date())
  )
  habitat <- eco$habitat %||% list()
  for (column in names(.habitat_traits)) {
    row[[column]] <- .clean_trait(habitat[[.habitat_traits[[column]]]])
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Download detailed CMA-SAREM species attributes
#'
#' @inheritParams sarem_species
#' @param ids Optional vector of SAREM species identifiers. When supplied,
#'   taxonomic filters are ignored.
#' @param province,ecoregion Optional character vectors used to retain species
#'   whose ranges intersect any requested province or ecoregion.
#' @return A data frame with one species per row. The per-species request log is
#'   available through [sarem_download_log()].
#' @export
download_sarem_traits <- function(
    ids = NULL, order = NULL, family = NULL, genus = NULL, species = NULL,
    category = NULL, environment = c("terrestrial", "marine", "all"),
    province = NULL, ecoregion = NULL,
    refresh = FALSE, quiet = FALSE) {
  environment <- match.arg(environment)
  taxa <- sarem_species(
    order = order, family = family, genus = genus, species = species,
    category = category, environment = environment, refresh = refresh,
    quiet = TRUE
  )
  if (!is.null(ids)) taxa <- taxa[as.character(taxa$id) %in% as.character(ids), , drop = FALSE]
  if (!is.null(province) || !is.null(ecoregion)) {
    spatial <- download_sarem_ranges(
      species = taxa$nombre_cientifico, environment = environment,
      province = province, ecoregion = ecoregion, clip = FALSE,
      refresh = refresh, quiet = TRUE
    )
    taxa <- taxa[as.character(taxa$id) %in% unique(spatial$id_sarem), , drop = FALSE]
  }
  if (!nrow(taxa)) stop("No species matched the requested filters.", call. = FALSE)
  rows <- list(); logs <- vector("list", nrow(taxa)); raw <- vector("list", nrow(taxa)); k <- 0L
  if (!quiet) cli::cli_alert_info("Downloading detailed records for {nrow(taxa)} species.")
  for (i in seq_len(nrow(taxa))) {
    id <- as.character(taxa$id[i]); scientific <- taxa$nombre_cientifico[i]
    result <- tryCatch(
      .sarem_json(paste0("/nativas/", id), attempts = 4),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      logs[[i]] <- data.frame(id_sarem=id, nombre_cientifico=scientific,
        estado="error", mensaje=conditionMessage(result), stringsAsFactors=FALSE)
    } else {
      k <- k + 1L; raw[[k]] <- result; rows[[k]] <- .normalize_traits(result)
      logs[[i]] <- data.frame(id_sarem=id, nombre_cientifico=scientific,
        estado="ok", mensaje=NA_character_, stringsAsFactors=FALSE)
    }
    if (!quiet && (i %% 25L == 0L || i == nrow(taxa))) cli::cli_alert_info("{i}/{nrow(taxa)} records processed.")
  }
  if (!length(rows)) stop("No detailed records could be downloaded.", call. = FALSE)
  out <- do.call(rbind, rows)
  out <- out[order(out$nombre_cientifico, out$id_sarem), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "sarem_download_log") <- do.call(rbind, logs)
  attr(out, "sarem_raw_records") <- raw[seq_len(k)]
  attr(out, "sarem_metadata") <- list(
    source = "CMA-SAREM - detailed species records",
    api = .sarem_api_base,
    site = .sarem_site,
    access_date = as.character(Sys.Date()),
    package = "saremRanges",
    package_version = as.character(utils::packageVersion("saremRanges")),
    requested_species = nrow(taxa),
    downloaded_species = nrow(out),
    failed_species = nrow(taxa) - nrow(out)
  )
  attr(out, "sarem_filters") <- list(
    ids = ids, order = order, family = family, genus = genus,
    species = species, category = category, environment = environment,
    province = province, ecoregion = ecoregion
  )
  if (!quiet) {
    ok <- sum(vapply(logs, function(z) z$estado == "ok", logical(1)), na.rm=TRUE)
    cli::cli_alert_success("{ok} detailed records downloaded successfully.")
    if (ok < nrow(taxa)) cli::cli_alert_warning("{nrow(taxa)-ok} species require review.")
  }
  out
}

#' Export CMA-SAREM attributes to an Excel workbook
#'
#' @param x A data frame returned by [download_sarem_traits()].
#' @param path Destination `.xlsx` file.
#' @param overwrite Replace an existing file.
#' @return `x`, invisibly.
#' @export
write_sarem_traits <- function(x, path, overwrite = FALSE) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required to export Excel files.", call. = FALSE)
  }
  if (file.exists(path) && !overwrite) stop("File already exists: ", path, call.=FALSE)
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  definitions <- stats::setNames(
    rep("Campo extraido de la ficha detallada CMA-SAREM.", ncol(x)),
    names(x)
  )
  definitions[c("id_sarem", "nombre_cientifico", "peso_general", "habito",
                "dieta_general", "patron_actividad")] <- c(
    "Identificador de la ficha CMA-SAREM.",
    "Nombre cientifico de la especie.",
    "Peso informado; conserva texto, unidad y rango originales.",
    "Habito general informado por CMA-SAREM.",
    "Categoria trofica general.",
    "Patron de actividad."
  )
  dictionary <- data.frame(
    columna = names(x), definicion = unname(definitions[names(x)]),
    stringsAsFactors = FALSE
  )
  log <- attr(x, "sarem_download_log")
  metadata <- data.frame(
    elemento=c("fuente","api","sitio","fecha_descarga","especies_descargadas","errores","estructura"),
    valor=c("CMA-SAREM - fichas detalladas",.sarem_api_base,.sarem_site,
      as.character(Sys.Date()),nrow(x),sum(log$estado != "ok",na.rm=TRUE),
      "Una especie por fila y un atributo por columna"), stringsAsFactors=FALSE
  )
  selected_filters <- attr(x, "sarem_filters", exact = TRUE)
  selected_filters <- if (is.null(selected_filters)) {
    data.frame(filtro = character(), valores = character())
  } else {
    data.frame(
      filtro = names(selected_filters),
      valores = vapply(selected_filters, function(z) {
        if (is.null(z)) "Todos" else paste(z, collapse = " | ")
      }, character(1)),
      stringsAsFactors = FALSE
    )
  }
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb,"Matriz de atributos",gridLines=FALSE)
  openxlsx::addWorksheet(wb,"Diccionario",gridLines=FALSE)
  openxlsx::addWorksheet(wb,"Metadatos",gridLines=FALSE)
  openxlsx::addWorksheet(wb,"Filtros",gridLines=FALSE)
  openxlsx::writeDataTable(wb,"Matriz de atributos",x,tableStyle="TableStyleMedium2")
  openxlsx::writeDataTable(wb,"Diccionario",dictionary,tableStyle="TableStyleMedium2")
  openxlsx::writeDataTable(wb,"Metadatos",metadata,tableStyle="TableStyleMedium2")
  openxlsx::writeDataTable(wb,"Filtros",selected_filters,tableStyle="TableStyleMedium2")
  openxlsx::freezePane(wb,"Matriz de atributos",firstRow=TRUE,firstCol=TRUE)
  openxlsx::setColWidths(wb,"Matriz de atributos",cols=1:ncol(x),widths="auto")
  openxlsx::setColWidths(wb,"Diccionario",cols=1:2,widths=c(36,80))
  openxlsx::setColWidths(wb,"Metadatos",cols=1:2,widths=c(25,70))
  openxlsx::setColWidths(wb,"Filtros",cols=1:2,widths=c(25,70))
  openxlsx::saveWorkbook(wb,path,overwrite=overwrite)
  invisible(x)
}
