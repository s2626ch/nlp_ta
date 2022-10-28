library(tidyverse)
library(bitTA)
library(wordcloud2)
library(tidytext)

buzz %>% 
  group_by(SECTION) %>% 
  tally()