# 12/4/21
# Advent of code day 4

# bingo with the giant squid!

library(tidyverse)
library(readxl)
library(magrittr)

numbers_called <- read_xlsx('input_day4.xlsx',sheet = 'nums_called',col_types = 'text')
numbers_called <- numbers_called$numbers
boards <- read_xlsx('input_day4.xlsx',sheet = 'boards')

boards %>% 
  filter(!is.na(boards)) %>%
  # assign a board number, so I can figure out which board something belongs to
  mutate(board_num = rep(1:100,each=5)) %>%
  separate(boards, into = c('A','B','C','D','E'),sep = '\\s+') -> boards #%>% 
  #mutate(across(everything(),as.numeric))-> boards 
  
split(boards,boards$board_num) -> split_boards
  

# function to 1 check for number, 2 mark, 3 check for winner
bingo <- function(board,number){
  board %<>%
    #select(-6) %>%
    mutate(A = if_else(A==number,NA,A),
           B = if_else(B==number,NA,B),
           C = if_else(C==number,NA,C),
           D = if_else(D==number,NA,D),
           E = if_else(E==number,NA,E)) 
  cols <- colSums(is.na(board))
  rows <- rowSums(is.na(board))
  ifelse(5 %in% cols | 5 %in% rows,
         return('Winner'),
         return('nope'))
}

# apply bingo function to all of the boards in the list

for (i in numbers_called){
  
  stuff <- sapply(split_boards,bingo,number=numbers_called[i])
  if('Winner' %in% stuff){
    print(i)
  }
}

board <- split_boards$`1`
board
for (i in numbers_called){
  board %<>% mutate(A = ifelse(A==numbers_called[i],NA,A),
           B = ifelse(B==numbers_called[i],NA,B),
           C = ifelse(C==numbers_called[i],NA,C),
           D = ifelse(D==numbers_called[i],NA,D),
           E = ifelse(E==numbers_called[i],NA,E)) 
  
  cols <- colSums(is.na(board))
  rows <- rowSums(is.na(board))
  print(board)
  print(cols)
  print(i)
  if(5 %in% cols | 5 %in% rows){
    print(glue::glue('Winner',i))
  }
}

cols <- colSums(is.na(board))
rows <- rowSums(is.na(board))
if(5 %in% cols | 5 %in% rows)(
  glue::glue('Winner',i)
)


# to 'mark' a board, make it NA. then if we get 5 NAs in col or row sums, we'll know there is a winner
# colSums(is.na(df))

# write a function to 1. mark number, if there 2. check for winners, then move to next board

# while (no winner){
 # check if number in board
#}