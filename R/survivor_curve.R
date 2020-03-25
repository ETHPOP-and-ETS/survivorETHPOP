
#' survivor_curve
#'
#' @param group
#' @param dat
#'
#' @return
#' @export
#' @inherit readr, dplyr
#'
#' @examples
#'
survivor_curve <- function(dat,
                           group = list(sex = "M",
                                        ETH.group = "WHO",
                                        year = 2040)) {

  ## filter by year of birth, ethnic group, sex
  max_num_yrs <- max(dat$year) - group$year
  year_age <- paste(seq(group$year, length.out = max_num_yrs),
                    seq(0, length.out = max_num_yrs),
                    sep = "_")
  group_dat <-
    dat %>%
    filter(yr_age %in% year_age,
           sex == group$sex,
           ETH.group == group$ETH.group) %>%
    mutate(S = exp(-cumsum(prop)))

  invisible(group_dat)
}
