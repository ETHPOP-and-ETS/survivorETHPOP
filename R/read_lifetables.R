
#' read_lifetables
#'
#' @param input_dir
#' @param output_dir
#'
#' @return
#' @export
#'
#' @examples
#'
read_lifetables <- function(input_dir = here::here("raw data"),
                            output_dir = "") {

  dir_names  <- dir(input_dir, full.names = TRUE)
  file_names <- dir_names[grepl("lifetable", dir_names)]

  dat <- map(file_names, read_csv, progress = FALSE)

  return(dat)
}
