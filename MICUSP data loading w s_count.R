#Load packages
library(tidyverse)

#Set working directory to source file location
#Download search results for each TA on Sketch Engine as .csv 
#with discipline, gender, ID, level, nativeness, s_count, total_count selected
#adjust column headers and add TA column

###PRESENT###
#Load present simple search results from Sketch Engine
present_simple <- read_csv('present_simple.csv')

#Load present progressive search results from Sketch Engine
present_progressive <- read_csv('present_progressive.csv')

#Load present perfect search results from Sketch Engine
present_perfect <- read_csv('present_perfect.csv')

#Load present perfect progressive search results from Sketch Engine
present_perfect_progressive <- read_csv('present_perfect_progressive.csv')

#Load be going to search results from Sketch Engine (will need this to calculate simple present)
be_going_to <- read_csv('be_going_to.csv')

#Check that number of rows matches expected counts from Sketch Engine


#Fix PRESENT PERFECT counts
#Combine present perfect and present perfect progressive into a single tibble and arrange by filename
present_perfects <- rbind(present_perfect, present_perfect_progressive) %>% 
  arrange(filename)
present_perfects

#Group by text to get TA counts; 
#preserve discipline, nativeness, year, and gender metadata
##This doesn't include s_count and total_count
present_perfects1 <- present_perfects %>% 
  group_by(filename, discipline, nativeness, year, gender, s_count, total_count) %>% 
  count(TA) %>%
  arrange(filename, s_count, total_count)
present_perfects1

#Sum TAs to verify counts before transforming
present_perfects_expected <- present_perfects1 %>%
  group_by(TA) %>%
  summarize(total = sum(n))
present_perfects_expected

#Calculate expected count for present perfect
present_perfect_expected = present_perfects_expected[1, 2] - present_perfects_expected[2, 2]
present_perfect_expected

#Subtract present perfect progressive from present perfect to get an accurate count 
present_perfects1_adjusted <- present_perfects1 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = present_perfect - present_perfect_progressive, 
         TA = 'present_perfect', .keep = 'unused') %>% 
  left_join(present_perfects1 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused') %>%
  filter(n > 0) #This removes 0 values but could keep them in if I remove this
present_perfects1_adjusted

#Sum TAs and check against expected
present_perfects_actual <- present_perfects1_adjusted %>%
  group_by(TA) %>%
  summarize(total = sum(n))
present_perfects_actual

#Save actual present perfect value
present_perfect_actual = present_perfects_actual[1, 2] 
present_perfect_actual

#Subtract expected from actual (should be 0)
present_perfect_actual - present_perfect_expected


#Fix PRESENT SIMPLE counts


#Step 1: Subtract be going to from simple present count

#Combine present simple and be going to into a single tibble and arrange by filename
present_simple_be_going_to <- rbind(present_simple, be_going_to) %>% 
  arrange(filename)
present_simple_be_going_to

#Group by text to get TA counts; 
#preserve discipline, nativeness, year, and gender metadata
##This doesn't include s_count and total_count
present_simple_be_going_to1 <- present_simple_be_going_to %>% 
  group_by(filename, discipline, nativeness, year, gender, s_count, total_count) %>% 
  count(TA) %>%
  arrange(filename, s_count, total_count)
present_simple_be_going_to1

#Sum TAs to verify counts before transforming
present_simple_be_going_to_expected <- present_simple_be_going_to1 %>%
  group_by(TA) %>%
  summarize(total = sum(n))
present_simple_be_going_to_expected

#Calculate expected count for future simple
present_simple_step1_expected = present_simple_be_going_to_expected[2, 2] - present_simple_be_going_to_expected[1, 2]
present_simple_step1_expected

#Subtract be going to from present simple to get an accurate count 
present_simple_step1_adjusted <- present_simple_be_going_to1 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = present_simple - be_going_to, 
         TA = 'present_simple', .keep = 'unused') %>% 
  left_join(present_simple_be_going_to1 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused') %>%
  filter(n > 0) #This removes 0 values but could keep them in if I remove this
present_simple_step1_adjusted

#Sum TAs and check against expected
present_simple_step1_actual <- present_simple_step1_adjusted %>%
  group_by(TA) %>%
  summarize(total = sum(n))
present_simple_step1_actual

#Save actual present perfect value
present_simple_step1_actual = present_simple_step1_actual[2, 2] 
present_simple_step1_actual

#Subtract expected from actual (should be 0)
present_simple_step1_actual - present_simple_step1_expected

#Filter out rows containing be going to
present_simple_step1_adjusted1 <- present_simple_step1_adjusted %>%
  filter(TA == "present_simple")
present_simple_step1_adjusted1



#Step 2: Subtract all other present tenses from simple present

#Group present progressive by text to get TA counts; 
#preserve discipline, nativeness, year, and gender metadata
#This doesn't include s_count and total_count 
present_progressive1 <- present_progressive %>% 
  group_by(filename, discipline, nativeness, year, gender, s_count, total_count) %>% 
  count(TA) %>%
  arrange(filename, s_count, total_count)
present_progressive1

#Combine present progressive, present simple adjusted, and present perfects into a single tibble and arrange by filename
present <- rbind(present_progressive1, present_simple_step1_adjusted1, present_perfects1_adjusted) %>% 
  arrange(filename, s_count, total_count)
present

#Sum TAs to verify counts before transforming
present_expected <- present %>%
  group_by(TA) %>%
  summarize(total = sum(n))
present_expected

#Calculate expected count for present simple
present_simple_expected = present_expected[4, 2] - present_expected[2, 2] - present_expected[3, 2] - present_expected[1, 2]
present_simple_expected

#Subtract all other present tenses from present simple to get an accurate count 
present_adjusted <- present %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = present_simple - present_progressive - present_perfect - present_perfect_progressive, 
         TA = 'present_simple', .keep = 'unused') %>% 
  left_join(present %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused') %>%
  filter(n > 0) #This removes 0 values but could keep them in if I remove this
present_adjusted

#Sum TAs and check against expected
present_actual <- present_adjusted %>%
  group_by(TA) %>%
  summarize(total = sum(n))
present_actual

#Save actual present simple value
present_simple_actual = present_actual[4, 2] 
present_simple_actual

#Subtract expected from actual (should be 0)
present_simple_actual - present_simple_expected



###PAST###
#Load past simple search results from Sketch Engine
past_simple <- read_csv('past_simple.csv')

#Load past progressive search results from Sketch Engine
past_progressive <- read_csv('past_progressive.csv')

#Load past perfect search results from Sketch Engine
past_perfect <- read_csv('past_perfect.csv')

#Load past perfect progressive search results from Sketch Engine
past_perfect_progressive <- read_csv('past_perfect_progressive.csv')

#Check that number of rows matches expected counts from Sketch Engine

#Fix PAST PERFECT counts
#Combine past perfect and past perfect progressive into a single tibble and arrange by filename
past_perfects <- rbind(past_perfect, past_perfect_progressive) %>% 
  arrange(filename)
past_perfects

#Group by text to get TA counts;
#preserve discipline, nativeness, year, and gender metadata
##This doesn't include s_count and total_count
past_perfects1 <- past_perfects %>% 
  group_by(filename, discipline, nativeness, year, gender, s_count, total_count) %>% 
  count(TA) %>%
  arrange(filename, s_count, total_count)
past_perfects1

#Sum TAs to verify counts before transforming
past_perfects_expected <- past_perfects1 %>%
  group_by(TA) %>%
  summarize(total = sum(n))
past_perfects_expected

#Calculate expected count for future simple
past_perfect_expected = past_perfects_expected[1, 2] - past_perfects_expected[2, 2]
past_perfect_expected

#Subtract past perfect progressive from past perfect to get an accurate count 
past_perfects1_adjusted <- past_perfects1 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_perfect - past_perfect_progressive, 
         TA = 'past_perfect', .keep = 'unused') %>% 
  left_join(past_perfects1 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused') %>%
  filter(n > 0) #This removes 0 values but could keep them in if I remove this
past_perfects1_adjusted

#Sum TAs and check against expected
past_perfects_actual <- past_perfects1_adjusted %>%
  group_by(TA) %>%
  summarize(total = sum(n))
past_perfects_actual

#Save actual past perfect value
past_perfect_actual = past_perfects_actual[1, 2] 
past_perfect_actual

#Subtract expected from actual (should be 0)
past_perfect_actual - past_perfect_expected

#Fix PAST SIMPLE counts

#Combine past simple and past progressive into a single tibble and arrange by filename
past_simp_prog <- rbind(past_simple, past_progressive) %>% 
  arrange(filename)
past_simp_prog

#Group past simple and past progressive by text to get TA counts; 
#preserve discipline, nativeness, year, and gender metadata
#This doesn't include s_count and total_count because that leads to an error in counts
past_simp_prog1 <- past_simp_prog %>% 
  group_by(filename, discipline, nativeness, year, gender, s_count, total_count) %>% 
  count(TA) %>%
  arrange(filename, s_count, total_count)
past_simp_prog1

#Combine past simple & progressive and past perfects into a single tibble and arrange by filename
past <- rbind(past_simp_prog1, past_perfects1_adjusted) %>% 
  arrange(filename)
past

#Sum TAs to verify counts before transforming
past_expected <- past %>%
  group_by(TA) %>%
  summarize(total = sum(n))
past_expected

#Calculate expected count for past simple
past_simple_expected = past_expected[4, 2] - past_expected[3, 2] - past_expected[2, 2] - past_expected[1, 2]
past_simple_expected

#Subtract all other past tenses from past simple to get an accurate count 
past_adjusted <- past %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_simple - past_progressive - past_perfect - past_perfect_progressive, 
         TA = 'past_simple', .keep = 'unused') %>% 
  left_join(past %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused') %>%
  filter(n > 0) #This removes 0 values but could keep them in if I remove this
past_adjusted

#Sum TAs and check against expected
past_actual <- past_adjusted %>%
  group_by(TA) %>%
  summarize(total = sum(n))
past_actual

#Save actual past simple value
past_simple_actual = past_actual[4, 2] 
past_simple_actual

#Subtract expected from actual (should be 0)
past_simple_actual - past_simple_expected



###FUTURE###
#Load future simple search results from Sketch Engine
future_simple <- read_csv('future_simple.csv')

#Load future progressive search results from Sketch Engine
future_progressive <- read_csv('future_progressive.csv')

#Load future perfect search results from Sketch Engine
future_perfect <- read_csv('future_perfect.csv')

#Check that number of rows matches expected counts from Sketch Engine

#No future perfect progressive, so no need to subtract here

#Combine into a single tibble and arrange by filename
future <- rbind(future_simple, future_progressive, future_perfect) %>% 
  arrange(filename)
future

#Group by text to get TA counts; 
#preserve discipline, nativeness, year, and gender metadata
future1 <- future %>% 
  group_by(filename, discipline, nativeness, year, gender, s_count, total_count) %>% 
  count(TA) %>%
  arrange(filename, s_count, total_count)
future1

#Sum TAs to verify counts before transforming
future_expected <- future1 %>%
  group_by(TA) %>%
  summarize(total = sum(n))
future_expected

#Calculate expected count for future simple
future_simple_expected = future_expected[3, 2] - future_expected[2, 2] - future_expected[1, 2]
future_simple_expected

#Subtract future perfect and future progressive from future simple to get an accurate count 
future1_adjusted <- future1 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = future_simple - future_progressive - future_perfect, 
         TA = 'future_simple', .keep = 'unused') %>% 
  left_join(future1 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused') %>%
  filter(n > 0) #This removes 0 values but could keep them in if I remove this
future1_adjusted

#Sum TAs and check against expected
future_actual <- future1_adjusted %>%
  group_by(TA) %>%
  summarize(total = sum(n))
future_actual

#Save actual future simple value
future_simple_actual = future_actual[3, 2] 
future_simple_actual

#Subtract expected from actual (should be 0)
future_simple_actual - future_simple_expected



#Combine present, past, future into a single tibble
MICUSP_by_filename <- rbind(future1_adjusted, past_adjusted, present_adjusted) %>% 
  arrange(filename, s_count, total_count)
MICUSP_by_filename

#Add missing combinations of TA and filename with with 0 value for each
MICUSP_by_filename <- MICUSP_by_filename %>%
  ungroup() %>% 
  complete(TA, filename, fill = list(n = 0L)) %>%
  arrange(filename, s_count, total_count)

#Fill in missing NA values
MICUSP_by_filename <- MICUSP_by_filename %>%
  group_by(filename) %>% 
  fill(discipline:gender, .direction = "downup") %>%
  ungroup

#Add column for tense
MICUSP_by_filename <- MICUSP_by_filename %>%
  mutate(tense = case_when(
    TA == "present_simple" | TA == "present_progressive" | 
      TA == "present_perfect" | TA == "present_perfect_progressive" ~ "present",
    TA == "past_simple" | TA == "past_progressive" | 
      TA == "past_perfect" | TA == "past_perfect_progressive" ~ "past",
    TA == "future_simple" | TA == "future_progressive" | 
      TA == "future_perfect" ~ "future"
  ))
MICUSP_by_filename


#Add column for aspect
MICUSP_by_filename <- MICUSP_by_filename %>%
  mutate(aspect = case_when(
    TA == "present_simple" | TA == "past_simple" | 
      TA == "future_simple" ~ "simple",
    TA == "present_progressive" | TA == "past_progressive" | 
      TA == "future_progressive" ~ "progressive",
    TA == "present_perfect" | TA == "past_perfect" | 
      TA == "future_perfect" ~ "perfect",
    TA == "present_perfect_progressive" | TA == "past_perfect_progressive" ~ "perfect_progressive"
  ))
MICUSP_by_filename

#Rename n to frequency
MICUSP_by_filename <- MICUSP_by_filename %>%
  rename(frequency = n)
MICUSP_by_filename

#Reorder columns
MICUSP_by_filename <- select(MICUSP_by_filename, filename, discipline, nativeness, 
                             year, gender, s_count, total_count, tense, aspect, TA, frequency) 
MICUSP_by_filename

#Save as CSV
write.csv(MICUSP_by_filename, 'MICUSP_by_filename_sentences.csv')


#Calculate sums

#Sum TAs and check against expected
MICUSP_sums <- MICUSP_by_filename %>%
  group_by(TA) %>%
  summarize(frequency = sum(frequency))
MICUSP_sums

#Arrange in descending order 
MICUSP_sums <- MICUSP_sums %>%
  arrange(desc(frequency))

#Add 0 value for future perfect progressive
MICUSP_sums[12,1] <- "future_perfect_progressive"
MICUSP_sums[12, 2] <- 0
MICUSP_sums

