
#' survivor_curve
#'
#' @param input_dir
#' @param output_dir
#' @param make_plot
#' @param group
#'
#' @return
#' @export
#' @inherit readr, dplyr
#'
#' @examples
#'
survivor_curve <- function(input_dir = here::here("raw data"),
                           output_dir = "",
                           group = list(sex = "M",
                                        ETH.group = "WHO",
                                        year = 2011),
                           make_plot = TRUE) {

  ## read in ETHPOP population data
  pop_dat <- read_csv(paste0(input_dir, "/clean_pop.csv"))

  ## read in ETHPOP death data
  death_dat <- read_csv(paste0(input_dir, "/clean_deaths_ageyrs.csv"))

  # ## use age group
  # death_dat <- read_csv(paste0(input_dir, "/clean_deaths.csv"))
  # pop_dat$agegrp <- cut(pop_dat$age, breaks = seq(0, 105, by = 5), right = FALSE)
  # pop_dat <- pop_dat %>% select(-age)

  ## remove columns
  death_dat <- death_dat %>% select(-X1)
  pop_dat <- pop_dat %>% select(-X1)

  ## join datasets
  full_dat <-
    full_join(death_dat, pop_dat,
              # by = c("year", "agegrp", "sex", "ETH.group")) %>%
              by = c("year", "age", "sex", "ETH.group")) %>%
    mutate(prop = deaths/pop,
           yr_age = paste(year, age, sep = "_"))

  ## filter by year of birth
  max_num_yrs <- 49 - (group$year - 2011)
  year_age <- paste(seq(group$year, length.out = max_num_yrs),
                    seq(0, length.out = max_num_yrs),
                    sep = "_")
  yr_age_dat <-
    full_dat %>%
    filter(yr_age %in% year_age)

  plot_dat <-
    yr_age_dat %>%
    filter(sex == group$sex,
           ETH.group == group$ETH.group)

  if (make_plot) {

    par(mgp = c(2,0.6,0))
    plot(
      x = 100*plot_dat$prop,
      type = 'l',
      # ylim = c(0.1, 5),
      ylim = c(0.001, 50),
      xlab = "age & year",
      ylab = "% dying each year",
      log = "y",
      lwd = 1.8,
      yaxt = "n",
      main = paste(group, collapse = ", ")
    )

    axis(2,
         at = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
         labels = c(0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 50),
         las = 2)
    axis(1, seq(0, length.out = max_num_yrs/10,  by = 10),
         line = 3,
         mgp = c(2,0.5,0),
         labels = seq(group$year, length.out = max_num_yrs/10, by = 10))
  }

  return(plot_dat)
}
