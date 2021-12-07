# 12/6/21
# Day 6: Lanternfish reproduction

library(tidyverse)
library(magrittr)

# get input/starting fish ages (start w/ 300 fish)
fish_input <- read_csv('input_day6.csv',col_names = FALSE)
fish_input %<>%
  pivot_longer(everything(),values_to = 'fish_age') %>%
  select(-name)

# iterate for 80 days (or 256 days for part 2)
# so turns out this gets big...FAST. so need to break into chunks and run a few separately....
# 125 iterations gives 19377884 fish. 
for(i in 1:125){
  fish_input %<>% mutate(fish_age = fish_age - 1) 
  new_bbs <- sum(fish_input$fish_age == -1)
  
  fish_input %<>%
    rbind(data.frame(fish_age = c(rep(8,new_bbs)))) %>%
    mutate(fish_age = ifelse(fish_age == -1, 6,fish_age))
}

# then break the result of 125 iterations into 4 and run each separately for the remaining, then combine the chunks
nrow(fish_input)/4

# PT1: --> STILL TAKES TOO LONG TO RUN. 
fish1 <- fish_input %>%
  mutate(id = rep(1:4,each=4844471)) %>%
  filter(id == 1) %>%
  select(-id)

for(i in 126:256){
  fish1 %<>% mutate(fish_age = fish_age - 1) 
  new_bbs <- sum(fish1$fish_age == -1)
  
  fish1 %<>%
    rbind(data.frame(fish_age = c(rep(8,new_bbs)))) %>%
    mutate(fish_age = ifelse(fish_age == -1, 6,fish_age))
}


# maybe this is an opportunity to use the methodology above to learn spark??? b/c it is taking way to long to run locally (like prob more than hour... ) and idk how big the df will be and if R will crash from lack of memory. 
# It occurs to me that I could probably write a function to model the exponential growth...rather than making a simulation that takes a looooong time to run...
fish_input %>% 
  group_by(fish_age) %>%
  count(fish_age)
  


## TIDBITS ----
# https://www.geeksforgeeks.org/count-the-frequency-of-a-variable-per-column-in-r-dataframe/

# count number of c in each column
ldply(data_table, function(c) sum(c =="a"))
  