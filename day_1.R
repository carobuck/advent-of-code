## 12/1/21
# Day 1 puzzle

library(tidyverse)

input <- read_csv('input_day1.csv')

# PART ONE ----
# Count # of times the next measurement increases from the previous measurement (first instance NA b/c doesn't have prev measurement)
# i.e. change in measurements is >0
input %>%
  mutate(diff = input - lag(input,1),
         diff_direction = if_else(diff > 0,'Increase','Decrease')) %>%
  count(diff_direction)
  
# Increases 1,446 times

# PART TWO ----
# Same thing as part one, but do it in rolling windows of three measurements
input %>%
  mutate(window_sum = input + lead(input,1) + lead(input,2)) %>%
  filter(!is.na(window_sum)) %>%
  mutate(diff = window_sum - lag(window_sum,1),
         diff_direction = if_else(diff > 0,'Increase','Decrease')) %>%
  count(diff_direction)

# Increases 1,486 times  