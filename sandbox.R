library(babynames)
library(tidyverse)

nc_est2017_agesex_res <- read_csv("data/nc-est2017-agesex-res.csv")
nc_est2017_agesex_res %>% 
  filter(SEX == 0 & AGE != 999) %>%
  select(AGE, POPESTIMATE2015) -> us_age_distribution_2015

# compute the probability of meeting someone born in certain year in 2015
us_age_distribution_2015 %>%
  mutate(born_year = 2015 - AGE) %>%
  mutate(total_pop = sum(POPESTIMATE2015)) %>%
  mutate(prob_born_year = POPESTIMATE2015/total_pop) %>%
  select(born_year, prob_born_year)-> us_age_distribution_2015

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

# compute the marginal probability of meeting someone named X 
babynames_new %>%
  left_join(us_age_distribution_2015, by = c("year" = "born_year")) %>%
  mutate(joint_prob = prop * prob_born_year.x) %>%
  group_by(name) %>%
  summarise(prob_name = sum(joint_prob)) -> prob_name

# attach back to baby name data set
babynames_new %>%
  left_join(prob_name) -> babynames_new

# now we can get the conditional distribution of the age of
# someone we met in 2015 with a specific name
babynames_new %>%
  mutate(cond_prob = prop * prob_born_year / prob_name) %>%
  arrange(name) -> babynames_new

# let's see Charles!
babynames_new %>%
  filter(name == "Charles") %>%
  ggplot(aes(x = year, y = cond_prob)) +
  geom_area()

# let's see Nancy
babynames_new %>%
  filter(name == "Michael") %>%
  ggplot(aes(x = year, y = cond_prob)) +
  geom_area()
