#' crear_shp()
#'
#'Función que permite generar un nuevo shp según los parámetros especificados
#' @param shp_data datos del shp
#' @param municipio municipio a seleccionar
#' @param agrupar_por columnas por las que se agrupará el shp
#' @param columnas_adicionales columnas adicionales a incluir en el shp
#'
#' @return .shp
#' @export
#'
#' @importFrom dplyr %>% group_by summarise
#' @importFrom sf st_union st_transform

#'
#'
crear_shp <- function(shp_data, municipio, agrupar_por, columnas_adicionales = NULL) {
  if (is.null(columnas_adicionales)) {
    columnas_adicionales <- c()
  }
  
  municipio_seleccionado <- shp_data[shp_data$NMUN %in% municipio, c("NMUN", agrupar_por, "geometry", columnas_adicionales)]
  municipio_agrupado <- municipio_seleccionado %>%
    dplyr::mutate(geometry = sf::st_transform(geometry, '+proj=longlat +datum=WGS84')) %>%
    dplyr::group_by(dplyr::across(c(NMUN, agrupar_por, columnas_adicionales))) %>%
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop")
  
  return(municipio_agrupado)
}
