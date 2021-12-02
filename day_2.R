## Day 2: Advent of code
# 12/2/21

# figuring out where the sub is going; need to calc path of sub. total forward  times total depth traversed. 
# (sum all moving forward) * [(sum all moving down) - (sum all moving up)]
library(tidyverse)

day2 <- read_csv('input_day2.csv')

# pt 1
day2 %>%
  separate(input,into= c('direction','amount'),sep = ' ') %>%
  mutate(amount = as.numeric(amount)) %>%
  group_by(direction) %>%
  summarise(tot_amt = sum(amount)) %>%
  # converting this to wide df so I can do arithmetic on rows without having to manually type it out
  pivot_wider(names_from = direction,values_from = tot_amt) %>%
  mutate(ans = forward * (down-up))

# pt 2: order of operations matters now, and need to do one step at a time, and keep track of aim
# aim = + down - up
# horiz = cumulative sum(forward)
# depth = current forward * cumulative aim

# FYI on cumsum (cumulative sum for a col): https://stackoverflow.com/questions/21818427/how-to-add-a-cumulative-column-to-an-r-dataframe-using-dplyr
day2 %>%
  separate(input,into= c('direction','amount'),sep = ' ') %>%
  mutate(amount = as.numeric(amount),
         index = c(1:1000)) %>%
  pivot_wider(names_from = direction,values_from = amount) %>%
  add_row(index = 0, forward = 0, down = 0, up = 0) %>%
  arrange(index) %>%
  replace_na(list(forward=0,down=0,up=0)) %>%
  mutate(downup = down - up) %>%
  select(-c(down,up)) %>%
  mutate(horiz = cumsum(forward),
         aim = cumsum(downup),
         depth = cumsum(forward * aim)) %>%
  tail(10)
# final horiz : 2018; final depth: 969300
# ans  = 2018 * 969300 = 1956047400