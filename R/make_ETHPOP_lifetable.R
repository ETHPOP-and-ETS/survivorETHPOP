
#' make_ETHPOP_life able
#'
#' @param input_dir location of clean_pop.csv and clean_deaths_ageyrs.csv
#' @param age_name age column heading in input data e.g. age or agegrp
#'
#' @return
#' @export
#'
#' @examples
#'
make_ETHPOP_lifetable <- function(input_dir = here::here("raw data"),
                                  age_name = "age") {

  ## read in ETHPOP population data
  pop_dat <- read_csv(paste0(input_dir, "/clean_pop.csv"), progress = FALSE)

  ## read in ETHPOP death data
  death_dat <- read_csv(paste0(input_dir, "/clean_deaths_ageyrs.csv"), progress = FALSE)

  if(length(setdiff(unique(death_dat$ETH.group),
                    unique(pop_dat$ETH.group))) > 0)
    stop("Ethnic groups don't match between data sets.")

  if(length(setdiff(unique(death_dat$sex),
                    unique(pop_dat$sex))) > 0)
    stop("Sex groups don't match between data sets.")

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
              by = c("year", age_name, "sex", "ETH.group")) %>%
    full_join(yr_age_id_lookup(), by = c("age", "year")) %>%     # join year-age unique group id
    arrange(age, sex, ETH.group, year) %>%
    mutate(death_rate = deaths/pop,                            # raw death rate
           yr_age = paste(year, age, sep = "_"),
           deaths_back = lag(deaths, 1),
           deaths_fwd = lead(deaths, 1),
           pop_back = lag(pop, 1),
           pop_fwd = lead(pop, 1),
           avg_deaths = (deaths_back + deaths + deaths_fwd)/3, # 3 calendar year average
           avg_pop = (pop_back + pop + pop_fwd)/3,
           mx = avg_deaths/avg_pop,                            # central rate of mortality
           mx = ifelse(is.na(mx), yes = death_rate, no = mx),
           qx = 2*mx/(2 + mx)) %>%                             # mortality rate
    filter(year != 2061) %>%                  # remove last year because NA number of deaths
    group_by(ETH.group, sex, id) %>%
    mutate(S = exp(-cumsum(death_rate)),
           S_qx = exp(-cumsum(qx))) %>%
    select(-deaths_back, -deaths_fwd,                          # remove intermediate variables
           -pop_back, -pop_fwd,
           -avg_deaths, -avg_pop)

  return(full_dat)
}
