
#' read ONS life tables
#'
#' lifetable extracted from: lttemplateew2018.xls
#' https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/
#' lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables
#'
#' @inherit purrr
#' @param input_dir read data folder
#' @param output_dir save data
#'
#' @return list of life tables
#' @export
#'
read_lifetables <- function(input_dir =
                              system.file("extdata", "lttemplateew_single_years",
                                          package = "survivorETHPOP"),
                            output_dir = "") {

  dir_names  <- dir(input_dir, full.names = TRUE)
  file_names <- dir_names[grepl(".xlsx", dir_names)]

  dat <- map(file_names,
             .f = read_lt)

  # birth year for cohort
  names(dat) <- map(dat, ~.$year[1])

  return(dat)
}

# read in single cohort year life table
read_lt <- function(file_name) {

  ltm <- readxl::read_xlsx(file_name, range = "A9:G110")
  ltf <- readxl::read_xlsx(file_name, range = "I9:O110")

  ltm$sex = "M"
  ltf$sex = "F"

  ltm <- ltm %>% rename(age = `Age x`, year = Year)
  ltf <- ltf %>% rename(age = `Age x`, year = Year)

  rbind.data.frame(ltm, ltf)
}
