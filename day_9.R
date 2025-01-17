# day 9
# 12/11/21


day9 <- read_csv('input_day9.csv',col_names = 'NUMS')
day9 <- read_csv('/Users/caroline.buck/Desktop/Fun_R/advent_of_code/input_day9.csv',col_names = 'NUMS')

# finding low points in the grid (a # is a low point if it's L/R/Up/Down neighbors are ALL higher. diagonals don't count as neighbors)

day9 %>%
  mutate(str_len = str_length(NUMS))


day9 %>%
  separate(NUMS, into = c(paste0('v',0:100)),sep = '') %>%
  select(-v0) %>%
  mutate_if(is.character,as.numeric) %>%
  #add on fake 10's row at top and bottom
  add_row(v1=10,.before = 1) %>%
  add_row(v1 = 10) %>%
  mutate_all(~ if_else(is.na(.x),10,.x)) %>%
#adding fake columns for left/right calculations
  mutate(v0=10,v101=10) %>%
  select(v0,everything()) -> day9_clean

# need version of day9 input w/o extra zeros
day9 %>%
  separate(NUMS, into = c(paste0('v',0:100)),sep = '') %>%
  select(-v0) %>%
  mutate_if(is.character,as.numeric) -> day9_plain

# get comparisons to the left, and to the right (cols)
# https://stackoverflow.com/questions/50411478/subtracting-consecutive-columns
df_l <- sign(day9_clean[-1] - day9_clean[-ncol(day9_clean)])
df_r <- sign(day9_clean[-ncol(day9_clean)] - day9_clean[-1])

# get comparisons up/down (rows)
df_u <- sign(day9_clean[-1,] - day9_clean[-nrow(day9_clean),])
df_d <- sign(day9_clean[-nrow(day9_clean),] - day9_clean[-1,])

# clean up comparison df's so can run through a loop w/ same i,j values
df_l %>%
  slice(2:101) %>% 
  select(-v101) -> clean_df_l 

df_r %>%
  slice(2:101) %>% 
  select(-v0) -> clean_df_r 

df_u %>%
  select(-c(v0,v101)) %>% 
  slice(1:100) -> clean_df_u
  
df_d %>%
  select(-c(v0,v101)) %>% 
  slice(2:101) -> clean_df_d

# loop to test if neg (less than) in all 4 dfs. 
compare_df <- data.frame()



# get values of low pts, +1
for(i in 1:100) {
  for(j in 1:100){
    compare_df[i,j] = ifelse((clean_df_d[i,j] == -1 &
                                clean_df_u[i,j] == -1 &
                                clean_df_l[i,j] == -1 &
                                clean_df_r[i,j] == -1),
                              day9_plain[i,j] +1, # low pt gets value of low pt + 1
                              0) # high pt gets value of 0
  }
}

# sum up everything that's not 0 (col sum then sum that row?)
compare_df %>% 
  mutate_if(is.character,as.numeric) %>%
  summarise(across(everything(), sum)) %>%
  rowSums()
# 532 is correct 


# part two: flag things that are 9's, then count how many things are in a group 
# things count as part of a group if they touch others in the groups and are bounded by 9's

# maybe flag if neighbors aren't 9's?? the way I did before? or convert 9's to NAs, so when do math, get NA for L/r/u/d neighbor (thus know group ends)
# not totally sure how to do the rest programmatically... will need to ponder. 
devtools::install_github("briatte/ggnet")
library(ggnet)
library(network)
library(sna)
library(ggplot2)

bip = data.frame(event1 = c(1, 2, 1, 0),
                 event2 = c(0, 0, 3, 0),
                 event3 = c(1, 1, 0, 4),
                 row.names = letters[1:4])

# weighted bipartite network
bip = network(bip,
              matrix.type = "bipartite",
              ignore.eval = FALSE,
              names.eval = "weights")
bip
ggnet2(bip)

day9_plain %>%
  select(1:10) %>%
  slice(1:10) -> test

ggnet2(network(test))

# Hmm. I'm not finding a way to easily convert this df into a graph.... so i think I need to get creative/reshape it on my own w/ some copy/paste??
# I'm really not sure if that's the best way (using a network graph) or if it makes more sense to try to mark 9's as NA and then count u/d/l/r 
# https://www.data-imaginist.com/2017/introducing-tidygraph/
install.packages("tidygraph")
library(tidygraph)
stuff <- as_tbl_graph(test)
stuff %>%
  activate(edges)
