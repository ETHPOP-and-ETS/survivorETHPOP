
#' survivor_curve
#'
#' @return
#' @export
#'
#' @examples
#'
survivor_curve <- function(input_dir = "C:/Users/ngreen1/Documents/R/cleanETHPOP/output_data",
                           output_dir = "") {

  ## read in ETHPOP population data
  pop_dat <- read_csv(paste0(input_dir, "/clean_pop.csv"))

  ## read in ETHPOP death data
  death_dat <- read_csv(paste0(input_dir, "/clean_deaths_ageyrs.csv"))

  # ## use age group
  # death_dat <- read_csv(paste0(input_dir, "/clean_deaths.csv"))
  # pop_dat$agegrp <- cut(pop_dat$age, breaks = seq(0, 105, by = 5), right = FALSE)
  # pop_dat <- pop_dat %>% select(-age)

  death_dat <- death_dat %>% select(-X1)
  pop_dat <- pop_dat %>% select(-X1)

  ## join datasets
  full_dat <-
    full_join(death_dat, pop_dat,
              # by = c("year", "agegrp", "sex", "ETH.group")) %>%
              by = c("year", "age", "sex", "ETH.group")) %>%
    mutate(prop = deaths/pop,
           yr_age = paste(year, age, sep = "_"))

  year_age <- paste(seq(2011, length.out = 49),
                    seq(0, length.out = 49),
                    sep = "_")

  yr_age_dat <-
    full_dat %>%
    filter(yr_age %in% year_age)

  xx <-
    yr_age_dat %>%
    filter(sex == "M",
           ETH.group == "WHO")

  plot(cumsum(xx$prop), type = 'o', xlab = "")
  axis(1, c(0,10,20,30,40,48), line = 3, labels = c(2011,2021,2031,2041,2051,2059))

}
