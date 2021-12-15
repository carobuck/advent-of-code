## day 7; 12/7/21
# getting crabs aligned. should be median distance.
# need to get median, then calculate abs val diff from median for each point, then sum up.

library(tidyverse)
library(magrittr)
library(glue)

setwd('~/Desktop/Fun_R/advent_of_code/')
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
crabs %>%
  mutate(dist=abs(crab_pos - 1.5),# 501 still too high @ 99,842,044 :/ . 1.5 too high also
         dist_val = abs(dist*(dist+1)/2)) %>%
  summarise(tot = sum(dist_val))
  
## I tried a bunch of calculus on paper with sums and partial derivatives/optimisation...but I kept getting the wrong answer (not sure if I was dealing w/ abs value properly)
# trying a brute force method now, calculate moving cost for all possible positions for all crabs, then sum and find the min

# positions range from 0 to 1955
crabs %>%  summary()
pos <- 0:1955
sum_f <- function(num){(num * (num + 1))/2}

# calculate all the possible positions
for (i in pos) {
  # fyi programmatically naming col: https://stackoverflow.com/questions/17152501/programmatically-adding-new-variables-to-a-dataframe
  crabs[[ paste0('pos',i) ]] <- sum_f(abs(crabs$crab_pos - i))
}

# select relevant cols, then get col sum, pivot and arrange to get min fuel spent
crabs %>%
  select(-crab_pos) %>%
  summarise(across(everything(), sum)) %>%
  pivot_longer(1:1956) %>%
  arrange(value) %>%
  head
