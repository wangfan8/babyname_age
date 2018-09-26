library(babynames)
library(tidyverse)

nc_est2015_agesex_res %>% 
  filter(SEX == 0 & AGE != 999) %>%
  select(AGE, POPESTIMATE2015) -> us_age_distribution_2015

babynames %>%
  filter(year >= 1915) %>%
  select(year, sex, name, n) %>%
  group_by(year, name) %>%
  summarise(n_babies = sum(n)) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(year_total = sum(n_babies)) %>%
  mutate(prop = n_babies / year_total) %>%
  select(year, name, n = n_babies , prop) -> babynames_new

