#' crear_shp()
#'
#' Función que permite generar un nuevo shp según los parámetros especificados
#' @param shp_data datos del shp
#' @param criterio_seleccion vector o lista con nombres, donde el nombre de cada elemento es el nombre de la columna y el valor es el valor a seleccionar
#' @param agrupar_por columnas por las que se agrupará el shp
#' @param columnas_adicionales columnas adicionales a incluir en el shp
#'
#' @return .shp
#' @export
#'
#' @importFrom dplyr %>% group_by summarise
#' @importFrom sf st_union st_transform
#'
crear_shp <- function(shp_data, criterio_seleccion, agrupar_por, columnas_adicionales = NULL) {
  if (is.null(columnas_adicionales)) {
    columnas_adicionales <- c()
  }

  if (is.vector(criterio_seleccion)) {
    criterio_seleccion <- list(criterio_seleccion)
  }

  for (nombre in names(criterio_seleccion)) {
    shp_data <- shp_data[shp_data[[nombre]] %in% criterio_seleccion[[nombre]], ]
  }

  datos_agrupados <- shp_data %>%
    dplyr::mutate(geometry = sf::st_transform(geometry, '+proj=longlat +datum=WGS84')) %>%
    dplyr::group_by(dplyr::across(c(agrupar_por, columnas_adicionales))) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")

  return(datos_agrupados)
}
