
#' make_ETHPOP_lifetable
#'
#' @param input_dir
#'
#' @return
#' @export
#'
#' @examples
#'
make_ETHPOP_lifetable <- function(input_dir = here::here("raw data")) {

  ## read in ETHPOP population data
  pop_dat <- read_csv(paste0(input_dir, "/clean_pop.csv"), progress = FALSE)

  ## read in ETHPOP death data
  death_dat <- read_csv(paste0(input_dir, "/clean_deaths_ageyrs.csv"), progress = FALSE)

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

  return(full_dat)
}
