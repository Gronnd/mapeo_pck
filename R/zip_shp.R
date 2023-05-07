#' zip_shp()
#'
#'Función que permite leer un shp contenido en un zip
#' @param file_name nombre del archivo, obligatorio
#' @param zip_pattern formato del archivo comprimido
#' @param shp_pattern formato del shp
#'
#'
#' @return .shp
#' @export
#'
#'
#'
zip_shp <- function(file_name, zip_pattern = ".zip$", shp_pattern = ".shp$") {
  if (missing(file_name) || !is.character(file_name) || length(file_name) != 1) {
    stop("El argumento 'file_name' es obligatorio y debe ser una cadena de caracteres única.")
  }

  temp_dir <- tempfile()
  zip_file <- list.files(getwd(), pattern = paste0(file_name, zip_pattern), full.names = TRUE)

  if (length(zip_file) != 1) {
    stop("Se encontró más de un archivo ZIP o ninguno en el directorio de trabajo. Asegúrate de que solo haya un archivo ZIP.")
  }

  utils::unzip(zip_file, exdir = temp_dir)
  shp_file <- list.files(temp_dir, pattern = shp_pattern, full.names = TRUE)

  if (length(shp_file) != 1) {
    stop("Se encontró más de un archivo SHP o ninguno en el archivo ZIP. Asegúrate de que solo haya un archivo SHP.")
  }

  sf::read_sf(shp_file)
}

