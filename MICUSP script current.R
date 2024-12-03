#Load packages
library(tidyverse)

#prep data and make sure counts match expected

#test.csv
test <- read_csv(file.choose())

#Display total document counts per TA while preserving the other columns
test2 <- test %>% 
  group_by(filename, discipline, nativeness, year, gender) %>%
  count(TA) 

#subtract all other past forms from past simple
test3 <- test2 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_simple - past_progressive - past_perfect - past_perfect_progressive,  
         TA = 'past_simple', .keep = 'unused') %>% 
  left_join(test2 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused')


#Add column for tense, aspect
test2$tense <- recode(test2$TA, past_perfect = "past", past_progressive = "past", past_perfect_progressive = "past", past_simple = "past")
test2$aspect <- recode(test2$TA, past_perfect = "perfect", past_progressive = "progressive", past_perfect_progresive = "perfect progressive", past_simple = "simple")

#Reorder columns
test2 <- select(test2, TA, tense, aspect, everything()) #TA, tense, aspect, then everything else

#Sum TAs before transforming counts
filter(test2, TA == "past_perfect") %>% group_by(TA) %>% summarize(total = sum(n))
filter(test2, TA == "past_perfect_progressive") %>% group_by(TA) %>% summarize(total = sum(n))

#Sum after transforming counts to check
filter(test3, TA == "past_perfect") %>% group_by(TA) %>% summarize(total = sum(n))
filter(test3, TA == "past_perfect_progressive") %>% group_by(TA) %>% summarize(total = sum(n))
