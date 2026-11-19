# saremRanges

`saremRanges` descarga mapas de distribución y construye matrices de atributos
de los mamíferos de Argentina a partir de CMA-SAREM.

> Versión 0.2.0 de prueba. No es un producto oficial de SAREM. Antes de
> publicar o redistribuir los datos descargados deben confirmarse la licencia,
> la cita y las condiciones de acceso automatizado de CMA-SAREM.

## Qué permite hacer

- Descargar todas las especies o elegir varios órdenes, familias, géneros o especies.
- Elegir varias categorías de conservación y varios tipos de rango.
- Seleccionar una o varias provincias y ecorregiones.
- Combinar filtros: dentro de cada filtro se usa **O**; entre filtros se usa **Y**.
- Obtener rangos completos (`clip = FALSE`) o recortados (`clip = TRUE`).
- Construir una matriz de atributos con una especie por fila.
- Exportar mapas a GeoPackage y atributos a Excel.

## Instalación

Se necesita R 4.2 o posterior y conexión a internet.

```r
install.packages("remotes")
remotes::install_github("agusnov/saremRanges")
library(saremRanges)
```

## Consultar opciones válidas

```r
sarem_orders()
sarem_families("Rodentia")
sarem_provinces()
sarem_ecoregions()
```

## Descargar mapas

```r
# Una especie
px <- download_sarem_ranges(species = "Phyllotis xanthopygus")

# Varias familias y provincias
x <- download_sarem_ranges(
  family = c("Cricetidae", "Felidae"),
  province = c("Mendoza", "San Juan"),
  clip = TRUE
)

# Varios órdenes, ecorregiones y tipos de rango
x <- download_sarem_ranges(
  order = c("Rodentia", "Carnivora"),
  ecoregion = c("Puna", "Altos Andes"),
  range_type = c("resident", "probable"),
  clip = TRUE
)

write_sarem_ranges(x, "rangos_seleccionados.gpkg", overwrite = TRUE)
```

GeoPackage es preferible a Shapefile porque conserva nombres largos, texto
UTF-8 y geometrías modernas.

## Descargar una matriz de atributos

La salida tiene una fila por especie y una columna por atributo.

```r
atributos <- download_sarem_traits(
  family = c("Cricetidae", "Felidae"),
  province = c("Mendoza", "San Juan")
)

write_sarem_traits(
  atributos,
  "matriz_atributos_Mendoza_San_Juan.xlsx",
  overwrite = TRUE
)
```

El Excel contiene la matriz, un diccionario, metadatos, filtros utilizados y
el registro de descarga.

## Cómo se combinan los filtros

```r
download_sarem_ranges(
  order = c("Rodentia", "Didelphimorphia"),  # Rodentia O Didelphimorphia
  province = c("Salta", "Jujuy"),           # Salta O Jujuy
  ecoregion = c("Selva de Yungas", "Chaco Seco") # Y además una de estas
)
```

`NULL` significa “todos”. Si se indican provincia y ecorregión al mismo tiempo,
se conserva la intersección entre ambos conjuntos de áreas.

## Fuentes geográficas

- Provincias: Servicio Georef de Argentina, CC BY 4.0.
- Ecorregiones terrestres: Atlas Nacional Interactivo de Argentina, Instituto
  Geográfico Nacional.

Las capas se guardan en la caché local y no se descargan nuevamente salvo que
se use `refresh = TRUE`.

## Control de errores

```r
sarem_download_log(x)
```

El registro informa qué especies se descargaron y cuáles requieren revisión.
