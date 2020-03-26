
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

  mat <- NULL

  for (i in seq_len(n_years) - 1) {

    mat <- rbind.data.frame(mat,
                            c(rep(NA, i), seq_len(n_years - i)),
                            stringsAsFactors = FALSE)
  }

  names(mat) <- as.character(seq_len(n_years) + base_year)
  mat <- mat %>% mutate(age = seq_len(nrow(mat)) - 1)

  res <-
    melt(mat, id.vars = "age",
         value.name = "id",
         variable.name = "year") %>%
    arrange(id) %>%
    na.omit() %>%
    mutate(year = as.numeric(as.character(year)),
           yr_age = paste(year, age, sep = "_"))

  return(res)
}
