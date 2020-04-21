
#' Create a year age id complete look-up dataframe
#'
#' @param n_years maximum number of years
#' @inherits reshape2
#'
#' @return dataframe with columns age year id yr_age
#' @export
#' @import reshape2
#'
#' @examples
#' res <- yr_age_id_lookup(n_years = 101, base_year = 2010)
#'
#' #age year id yr_age
#' # 0 1911  1 1911_0
#' # 1 1912  1 1912_1
#' # 2 1913  1 1913_2
#' # 3 1914  1 1914_3
#' # 4 1915  1 1915_4
#' # 5 1916  1 1916_5
#'
yr_age_id_lookup <- function(n_years = 201,
                             base_year = 1910){

  mat <- matrix(nrow = n_years,
                ncol = n_years)

  # create upper diagonal array:
  # 1  2  3 ...
  # NA 1  2 ...
  # NA NA 1 ...
  # ...

  for (i in seq_len(n_years) - 1) {

    mat[i + 1, ] <- c(rep(NA, i), seq_len(n_years - i))
  }

  # include year column names and age column
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
