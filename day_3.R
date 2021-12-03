# 12/3/21

# sub diagnostics w/ binary #'s 

library(tidyverse)

input <- read_table('input_day3',col_names = 'input')

# Part 1: get gamma and epsilon ----
# Each bit in the gamma rate can be determined by finding the most common bit in the corresponding position of all numbers in the diagnostic report.
# The epsilon rate is calculated in a similar way; rather than use the most common bit, the least common bit from each position is used.

# Get counts of all the bits
input %>% 
  separate(input, into = paste0('bit',0:12),sep = '') %>%
  select(-bit0) %>%
  mutate(across(everything(),as.numeric)) %>% 
  summarise(across(everything(),sum)) %>%
  # sum is # of 1's in that col
  pivot_longer(1:12,names_to = 'bit',values_to = 'num_1s') %>%
  # gamma rate = most common bit
  # epsilon rate = least common bit
  mutate(gamma = ifelse(num_1s < 500,0,1),
         epsilon = 1-gamma) -> summed
  
# collapse and convert to decimal #'s
gamma <- strtoi(paste0(summed$gamma,collapse = ''),base=2) # 2028
epsilon <- strtoi(paste0(summed$epsilon,collapse = ''),base=2) # 2067

# mult together for ans
gamma * epsilon

# part 2: finding two other rates ----
#To find oxygen generator rating, determine the most common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 1 in the position being considered.
#To find CO2 scrubber rating, determine the least common value (0 or 1) in the current bit position, and keep only numbers with that bit in that position. If 0 and 1 are equally common, keep values with a 0 in the position being considered.

# find oxygen rating
input %>%
  separate(input, into = paste0('bit',0:12),sep = '',remove = FALSE) %>%
  select(-bit0) %>%
  mutate(across(2:13,as.numeric)) %>% 
  # if sum is < nrow/2, means we have more 0's. if ==, will be 1
  mutate(bitcounter = ifelse(sum(bit1)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,1,1)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit2)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,2,2)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit3)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,3,3)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit4)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,4,4)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit5)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,5,5)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit6)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,6,6)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit7)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,7,7)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit8)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,8,8)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit9)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,9,9)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit10)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,10,10)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit11)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,11,11)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit12)< nrow(.)/2,'0','1')) %>%
  filter(str_sub(input,12,12)==bitcounter) %>%
  glimpse()

# oxy: 010101101111

# co2 scrubber
input %>%
  separate(input, into = paste0('bit',0:12),sep = '',remove = FALSE) %>%
  select(-bit0) %>%
  mutate(across(2:13,as.numeric)) %>% 
  # if sum is < nrow/2, means we have more 0's. if ==, will be 1
  mutate(bitcounter = ifelse(sum(bit1)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,1,1)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit2)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,2,2)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit3)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,3,3)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit4)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,4,4)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit5)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,5,5)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit6)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,6,6)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit7)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,7,7)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit8)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,8,8)==bitcounter) %>%
  mutate(bitcounter = ifelse(sum(bit9)< nrow(.)/2,'1','0')) %>%
  filter(str_sub(input,9,9)==bitcounter) %>%
  glimpse()
  
# co2: 100110010111


## mult tog for life support #:
strtoi('010101101111',base=2) * strtoi('100110010111',base = 2)
