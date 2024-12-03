#Load packages
library(tidyverse)

#Set working directory to source file location
#Download search results for each TA on Sketch Engine as .csv 
#with discipline, gender, ID, level, nativeness, s_count, total_count selected
#adjust column headers and add TA column

###PRESENT###
#Load present simple search results from Sketch Engine
present_simple <- read_csv('present_simple1.csv')

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
present_perfects1 <- present_perfects %>% 
  group_by(filename, move) %>% 
  count(TA) 
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
present_simple_be_going_to1 <- present_simple_be_going_to %>% 
  group_by(filename, move) %>% 
  count(TA) 
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
present_progressive1 <- present_progressive %>% 
  group_by(filename, move) %>% 
  count(TA) 
present_progressive1

#Combine present progressive, present simple adjusted, and present perfects into a single tibble and arrange by filename
#Filter out rows that are not labeled with a rhetorical move (5 rows total)
present <- rbind(present_progressive1, present_simple_step1_adjusted1, present_perfects1_adjusted) %>% 
  arrange(filename) %>%
  filter(move != "rhetorical-moves")
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
#Count is off by 1 despite several fixes; count is correct when grouped by filename
present_simple_actual - present_simple_expected



###PAST###
#Load past simple search results from Sketch Engine
past_simple <- read_csv('past_simple.csv')

#Load past progressive search results from Sketch Engine
past_progressive <- read_csv('past_progressive.csv')

#Load past perfect search results from Sketch Engine
past_perfect <- read_csv('past_perfect.csv')


#Check that number of rows matches expected counts from Sketch Engine

#No need to fix PAST PERFECT counts because there is no PPP

#Fix PAST SIMPLE counts

#Combine past simple, past progressive, and past perfect into a single tibble and arrange by filename
pasts <- rbind(past_simple, past_progressive, past_perfect) %>% 
  arrange(filename)
pasts

#Group past simple and past progressive by text to get TA counts; 
#preserve discipline, nativeness, year, and gender metadata
#This doesn't include s_count and total_count because that leads to an error in counts
past <- pasts %>% 
  group_by(filename, move) %>% 
  count(TA) 
past

#Sum TAs to verify counts before transforming
past_expected <- past %>%
  group_by(TA) %>%
  summarize(total = sum(n))
past_expected

#Calculate expected count for past simple
past_simple_expected = past_expected[3, 2] - past_expected[2, 2] - past_expected[1, 2]
past_simple_expected

#Subtract all other past tenses from past simple to get an accurate count 
past_adjusted <- past %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_simple - past_progressive - past_perfect, 
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
past_simple_actual = past_actual[3, 2] 
past_simple_actual

#Subtract expected from actual (should be 0)
past_simple_actual - past_simple_expected



###FUTURE###
#Load future simple search results from Sketch Engine
future_simple <- read_csv('future_simple.csv')

#Load future progressive search results from Sketch Engine
future_progressive <- read_csv('future_progressive.csv')


#Check that number of rows matches expected counts from Sketch Engine


#Combine into a single tibble and arrange by filename
future <- rbind(future_simple, future_progressive) %>% 
  arrange(filename)
future

#Group by text to get TA counts; 
future1 <- future %>% 
  group_by(filename, move) %>% 
  count(TA) 
future1

#Sum TAs to verify counts before transforming
future_expected <- future1 %>%
  group_by(TA) %>%
  summarize(total = sum(n))
future_expected

#Calculate expected count for future simple
future_simple_expected = future_expected[2, 2] - future_expected[1, 2]
future_simple_expected

#Subtract future perfect and future progressive from future simple to get an accurate count 
future1_adjusted <- future1 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = future_simple - future_progressive, 
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
future_simple_actual = future_actual[2, 2] 
future_simple_actual

#Subtract expected from actual (should be 0)
future_simple_actual - future_simple_expected


#Combine
#Combine present, past, future into a single tibble
#Filter out rows with no rhetorical move label (5 rows)
STP_by_filename <- rbind(future1_adjusted, past_adjusted, present_adjusted) %>% 
  arrange(filename) %>%
  filter(move != "rhetorical-moves")
STP_by_filename

#Do I want to keep this??
#Add missing combinations of TA and filename with with 0 value for each
STP_by_filename <- STP_by_filename %>%
  ungroup() %>% 
  complete(TA, filename, move, fill = list(n = 0L)) %>%
  arrange(filename)
STP_by_filename


#Add column for tense
STP_by_filename <- STP_by_filename %>%
  mutate(tense = case_when(
    TA == "present_simple" | TA == "present_progressive" | 
      TA == "present_perfect" | TA == "present_perfect_progressive" ~ "present",
    TA == "past_simple" | TA == "past_progressive" | 
      TA == "past_perfect" | TA == "past_perfect_progressive" ~ "past",
    TA == "future_simple" | TA == "future_progressive" | 
      TA == "future_perfect" ~ "future"
  ))
STP_by_filename


#Add column for aspect
STP_by_filename <- STP_by_filename %>%
  mutate(aspect = case_when(
    TA == "present_simple" | TA == "past_simple" | 
      TA == "future_simple" ~ "simple",
    TA == "present_progressive" | TA == "past_progressive" | 
      TA == "future_progressive" ~ "progressive",
    TA == "present_perfect" | TA == "past_perfect" | 
      TA == "future_perfect" ~ "perfect",
    TA == "present_perfect_progressive" | TA == "past_perfect_progressive" ~ "perfect_progressive"
  ))
STP_by_filename

#Add column for discipline
library(stringi)
STP_by_filename <- STP_by_filename %>%
  mutate(discipline = case_when(
    stri_detect_fixed(filename, "Bio") ~ "Biology",
    stri_detect_fixed(filename, "Chem") ~ "Chemistry",
    stri_detect_fixed(filename, "PolSci") ~ "Political_Science",
    stri_detect_fixed(filename, "Psych") ~ "Psychology"))
STP_by_filename

#Add column for school
STP_by_filename <- STP_by_filename %>%
mutate(school = case_when(
  stri_detect_fixed(filename, "Manoa") ~ "University of Hawaii at Manoa",
  stri_detect_fixed(filename, "Gustavus") ~ "Gustavus Adolphus College",
  stri_detect_fixed(filename, "EMich") ~ "Eastern Michigan University",
  stri_detect_fixed(filename, "Lafayette") ~ "University of Lousiana at Lafayette",
  stri_detect_fixed(filename, "UVM") ~ "University of Vermont",
  stri_detect_fixed(filename, "CSU") ~ "Colorado State University",
  stri_detect_fixed(filename, "Andrews") ~ "Andrews University",
  stri_detect_fixed(filename, "UCF") ~ "University of Central Florida",
  stri_detect_fixed(filename, "WSU") ~ "Washington State University",
  stri_detect_fixed(filename, "UNC") ~ "University of North Carolina at Charlotte",
  stri_detect_fixed(filename, "Arkansas") ~ "University of Arkansas",
  stri_detect_fixed(filename, "Roanoke") ~ "Roanoke College",
  stri_detect_fixed(filename, "WLiberty") ~ "West Liberty University",
  stri_detect_fixed(filename, "Dartmouth") ~ "Dartmouth College",
  stri_detect_fixed(filename, "Berkeley") ~ "University of California, Berkeley",
  stri_detect_fixed(filename, "SJU") ~ "College of Saint Benedict & Saint John's University",
  stri_detect_fixed(filename, "Bates") ~ "Bates College",
  stri_detect_fixed(filename, "BYU") ~ "Brigham Young University"
))
STP_by_filename

#Rename n to frequency
STP_by_filename <- STP_by_filename %>%
  rename(frequency = n)
STP_by_filename

#Reorder columns
STP_by_filename <- select(STP_by_filename, filename, discipline, school, move, tense, aspect, TA, frequency) 
STP_by_filename

#Save as CSV
write.csv(STP_by_filename, 'STP_by_filename1.csv')


#Calculate sums

#Sum TAs and check against expected
STP_sums <- STP_by_filename %>%
  group_by(TA) %>%
  summarize(frequency = sum(frequency))
STP_sums

#Arrange in descending order 
STP_sums <- STP_sums %>%
  arrange(desc(frequency))

#Add 0 value for missing TAs 
STP_sums[10,1] <- "past_perfect"
STP_sums[10, 2] <- 0

STP_sums[11,1] <- "future_perfect"
STP_sums[11, 2] <- 0

STP_sums[12,1] <- "future_perfect_progressive"
STP_sums[12, 2] <- 0
STP_sums

#Add percentage column
STP_sums <- STP_sums %>%
  mutate(percentage = frequency / sum(frequency))


#Calculate sums for tense and aspect separately

#Sum tense
STP_by_filename %>%
  group_by(tense) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = frequency / sum(frequency)) %>% 
  arrange(desc(frequency))

#Sum aspect
STP_by_filename %>%
  group_by(aspect) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = frequency / sum(frequency)) %>% 
  arrange(desc(frequency))

#SUM By discipline
#Biology
BIO <- STP_by_filename %>%
  filter(discipline == "Biology") %>%
  group_by(TA) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = round(frequency / sum(frequency) * 100, digits = 2))
BIO

#Chemistry
CHEM <- STP_by_filename %>%
  filter(discipline == "Chemistry") %>%
  group_by(TA) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = round(frequency / sum(frequency) * 100, digits = 2))
CHEM

#Political Science
POLSCI <- STP_by_filename %>%
  filter(discipline == "Political_Science") %>%
  group_by(TA) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = round(frequency / sum(frequency) * 100, digits = 2))
POLSCI

#Psychology
PSYC <- STP_by_filename %>%
  filter(discipline == "Psychology") %>%
  group_by(TA) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = round(frequency / sum(frequency) * 100, digits = 2))
PSYC

#SUM by discipline
disciplines <- STP_by_filename %>%
  group_by(discipline, TA) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = (frequency / sum(frequency)))
disciplines

#SUM by move
moves <- STP_by_filename %>%
  group_by(move, TA) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = (frequency / sum(frequency)))
moves

#Plotting

#Compare top 5 TAs by corpus
Percent_by_corpus <- read_csv('Percent_by_corpus1.csv') 


#Plot by corpus
ggplot(Percent_by_corpus, aes(x= corpus, y = percentage, fill = fct_reorder(TA, percentage, .desc = FALSE)))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(name = "TA", palette = "Spectral") +
  theme_minimal() +
  labs(
    x = "Corpus",
    y = "Percentage",
    title = "Percentage of finite verbs in the top 5 TAs for each corpus"
  )

#Plot by move
#Filter for top 5
moves1 <- moves %>%
  filter(TA == "present_simple" | TA == "past_simple" | TA == "future_simple" |
           TA == "present_perfect" | TA == "present_progressive")
#Plot
ggplot(moves1, aes(x= move, y = percentage, fill = fct_reorder(TA, percentage, .desc = FALSE)))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(name = "TA", palette = "Spectral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = c('achievement','benefits','competence', 'gap', 'goal', 
                              'hypothesis', 'importance', 'means', 'previous research', 
                              'territory', 'timeline')) +
  labs(
    x = "Move",
    y = "Percentage",
    title = "Percentage of finite verbs in the top 5 TAs for each move"
  )

#Don't filter for top 5
ggplot(moves, aes(x= move, y = percentage, fill = fct_reorder(TA, percentage, .desc = FALSE)))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(name = "TA", palette = "Spectral") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = c('achievement','benefits','competence', 'gap', 'goal', 
                              'hypothesis', 'importance', 'means', 'previous research', 
                              'territory', 'timeline')) +
  labs(
    x = "Move",
    y = "Percentage",
    title = "Percentage of finite verbs in each TA for each move"
  )



#Plot by discipline
#Filter for top 5
disciplines1 <- disciplines %>%
  filter(TA == "present_simple" | TA == "past_simple" | TA == "future_simple" |
           TA == "present_perfect" | TA == "present_progressive")
#Plot
ggplot(disciplines1, aes(x= discipline, y = percentage, fill = fct_reorder(TA, percentage, .desc = FALSE)))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(name = "TA", palette = "Spectral") +
  theme_minimal() +
  labs(
    x = "Discipline",
    y = "Percentage",
    title = "Percentage of finite verbs in the top 5 TAs for each discipline"
  )

#Modeling
#Boxplot of frequency by TA
ggplot(STP_by_filename, aes(x= fct_reorder(TA, frequency, mean), y = frequency, color = tense)) +
  geom_boxplot()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "TA",
    y = "Frequency",
    title = "Frequency count of each TA by text in STP corpus"
  )

#Logtransform frequency
STP_by_filename <- STP_by_filename %>%
  mutate(logfrequency = log10(frequency + 1)) 
#Boxplot of logfrequency by TA
ggplot(STP_by_filename, aes(x= fct_reorder(TA, frequency, mean), y = logfrequency, color = tense)) +
  geom_boxplot()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "TA",
    y = "Frequency",
    title = "Log frequency count of each TA by text in STP corpus"
  )


#Trying models

#Transform TA to factor variable
STP_by_filename <- STP_by_filename %>%
  mutate(TA = as.factor(TA))
#relevel TA
STP_by_filename$TA <-  relevel(STP_by_filename$TA, ref = "present_perfect")

#TA only
TA_model <-  glm(frequency ~ TA, data = STP_by_filename, family = 'poisson')
summary(TA_model)

#TA + discipline
TA_disc_model <-  glm(frequency ~ TA + discipline, data = STP_by_filename, family = 'poisson')
summary(TA_disc_model)

#TA * discipline
TA_disc_inter_model <-  glm(frequency ~ TA * discipline, data = STP_by_filename, family = 'poisson')
summary(TA_disc_inter_model)

#Compare models
anova(TA_disc_model, TA_disc_inter_model, test = "Chisq")
#TA * discipline is better


#TA + move
TA_move_model <-  glm(frequency ~ TA + move, data = STP_by_filename, family = 'poisson')
summary(TA_move_model)

#TA * move
TA_move_inter_model <-  glm(frequency ~ TA * move, data = STP_by_filename, family = 'poisson')
summary(TA_move_inter_model)

#Compare models
anova(TA_move_model, TA_move_inter_model, test = "Chisq")
#TA * move is better


#TA * discipline * move
TA_disc_move_model <-  glm(frequency ~ TA * discipline * move, data = STP_by_filename, family = 'poisson')
summary(TA_disc_move_model)

#TA * discipline + TA * move
TA_disc_and_move_model <-  glm(frequency ~ TA + discipline + move + TA:discipline + TA:move, data = STP_by_filename, family = 'poisson')
summary(TA_disc_and_move_model)

#Compare models
anova(TA_disc_and_move_model, TA_disc_move_model, test = "Chisq")
#TA * discipline * move is better
