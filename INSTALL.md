# Instalación y primera prueba

El paquete requiere R 4.2 o posterior y conexión a internet para consultar CMA-SAREM.

## 1. Instalar los paquetes necesarios

En RStudio:

```r
install.packages(c("sf", "httr2", "jsonlite", "cli", "testthat"))
```

En macOS, la instalación de `sf` puede requerir que R y sus herramientas estén
actualizados. Si `sf` ya funciona en tus análisis espaciales, no hace falta
reinstalarlo.

## 2. Instalar saremRanges

Desde el repositorio de GitHub:

```r
install.packages("remotes")
remotes::install_github("agusnov/saremRanges")
```

Si tenés el archivo del paquete, también podés instalarlo localmente:

```r
install.packages(
  "/ruta/completa/saremRanges_0.2.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

También puede instalarse directamente desde la carpeta descomprimida:

```r
install.packages("remotes")
remotes::install_local("/ruta/completa/saremRanges")
```

## 3. Primera prueba: una especie

```r
library(saremRanges)

px <- download_sarem_ranges(
  species = "Phyllotis xanthopygus"
)

px
plot(sf::st_geometry(px))
sarem_download_log(px)
```

## 4. Guardar para QGIS

```r
write_sarem_ranges(
  px,
  "Phyllotis_xanthopygus_SAREM.gpkg",
  overwrite = TRUE
)
```

El archivo `.gpkg` puede abrirse directamente en QGIS o ArcGIS.

## 5. Probar filtros mayores

```r
# Familia Cricetidae
cricetidos <- download_sarem_ranges(family = "Cricetidae")

# Todos los roedores
roedores <- download_sarem_ranges(order = "Rodentia")

# Especies amenazadas
amenazados <- download_sarem_ranges(
  category = c("VU", "EN", "CR")
)
```

La primera consulta puede tardar porque se construye la tabla taxonómica. Las
consultas siguientes reutilizan la información de la sesión y los mapas se
guardan en una caché local.
