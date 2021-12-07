## day 7; 12/7/21
# getting crabs aligned. should be median distance.
# need to get median, then calculate abs val diff from median for each point, then sum up.

library(tidyverse)
library(magrittr)


crabs <- read_csv('input_day7.csv',col_names = FALSE)
crabs %<>% 
  pivot_longer(everything(),values_to = 'crab_pos') %>%
  select(-name)

crabs %>%
  mutate(med_pos = median(crab_pos),
         dist = abs(crab_pos - med_pos)) %>%
  summarise(tot = sum(dist))

# part 2: changing fuel burning rates (need diff way to optimize...need factorial??)
# As it turns out, crab submarine engines don't burn fuel at a constant rate. Instead, each change of 1 step in horizontal position costs 1 more unit of fuel than the last: the first step costs 1, the second step costs 2, the third step costs 3, and so on.
