## 12/12/21

# Working on day 8 a few days late...alas it is okay
setwd('~/Desktop/Fun_R/advent_of_code/')
library(tidyverse)

# part 1: count how many times a 1, 4, 7 or 8 appears in output values (4 digits after pipe)
# 1: 2 segs; 4: 4 segs; 7: 3 segs; 8: 7 segs
# so we are counting strings w/ 2, 4, 3 or 7 characters

day8 <- read_csv('input_day8.csv',col_names = 'strings')

day8 %>%
  separate(strings, into = paste0('str',0:14),sep = ' ') %>%
  select(str11:str14) %>%
  mutate(str11_len = str_length(str11),
         str12_len = str_length(str12),
         str13_len = str_length(str13),
         str14_len = str_length(str14)) %>%
  select(str11_len:str14_len) %>%
  pivot_longer(1:4) %>%
  count(value) %>%
  filter(value %in% c(2,3,4,7)) %>%
  colSums()

# 237 times we have a 2,3,4 or 7 character string

#  part 2: need to actually decode what all the values are
day8 %>%
  separate(strings, into = paste0('str',0:14),sep = ' ') %>%
  select(-str10) -> day8_clean
  
# alphabetizing may help? then can figure out what numbers are unique?
# make another df with str length for all of the strings
# make another df with the final digit value (eg. str_len = 2 ==> 1 as digit value)

# maybe: --> actually this seems most promising!!?
# alphabetize to standardize all the numbers to be the same (so 1 can't be ab and ba)
# make a df with 7 cols, one for each position upper L, lower L, upper R, lower R, high, mid, low
# fill in with digits I know (from str length), then figure out the rest...
# if I know the pattern of digit 5, I know that it must be made up of the 5 values that correspond to the appropriate positions above 

day8_clean %>%
  slice(1:10) -> tester

# try on a smaller chunk of the dataset
### UHH str_sort and str_order are not working as expected to alphabetize??
tester %>%
  mutate_all(sort) #%>%
str_sort('caroline') # fail
sort(c('caroline')) # fail
paste(sort(unlist(strsplit('caroline',""))), collapse = "") # this works, but need to vectorize

tester %>%
  mutate(str0 = paste(sort(unlist(strsplit(str0,""))), collapse = ""))

## biggest pain to alphabetize but nothing else working, so here's what we've got
tester$combname <- apply(tester[1], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))

# there are 14 original columns to alphabetize.
day8_clean$str0c <- apply(day8_clean[1], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str1c <- apply(day8_clean[2], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str2c <- apply(day8_clean[3], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str3c <- apply(day8_clean[4], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str4c <- apply(day8_clean[5], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str5c <- apply(day8_clean[6], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str6c <- apply(day8_clean[7], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str7c <- apply(day8_clean[8], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str8c <- apply(day8_clean[9], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str9c <- apply(day8_clean[10], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str11c <- apply(day8_clean[11], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str12c <- apply(day8_clean[12], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str13c <- apply(day8_clean[13], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))
day8_clean$str14c <- apply(day8_clean[14], 1, function(x) paste(sort(unlist(strsplit(x,""))), collapse = ""))

## OK. GOOD TO GO TO PROCEED FROM HERE WITH NEXT STEPS (SELECTING CLEAN COLS AND MAKING POS COLS)


