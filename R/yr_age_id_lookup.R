
#' yr_age_id_lookup
#'
#' @param n_years maximum number of years
#' @inherits reshape2
#'
#' @return
#' @export
#'
#' @examples
#' res <- yr_age_id_lookup(n_years = 101, base_year = 2010)
#'
yr_age_id_lookup <- function(n_years = 201,
                             base_year = 1910){

  mat <- matrix(nrow = n_years,
                ncol = n_years)

  # create diagonal array:
  # 1  2  3 ...
  # NA 1  2 ...
  # NA NA 1 ...
  # ...

  for (i in seq_len(n_years) - 1) {

    mat[i + 1, ] <- c(rep(NA, i), seq_len(n_years - i))
  }

  mat_names <- as.character(seq_len(n_years) + base_year)
  mat <-
    mat %>%
    as.data.frame() %>%
    `colnames<-`(mat_names) %>%
    mutate(age = seq_len(nrow(mat)) - 1)

  res <-
    mat %>%
    melt(id.vars = "age",
         value.name = "id",          # unique group reference
         variable.name = "year") %>%
    arrange(id) %>%
    na.omit() %>%
    mutate(year = as.numeric(as.character(year)),
           yr_age = paste(year, age, sep = "_"))

  return(res)
}
