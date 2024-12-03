##MICSUP script- archive

library(tidyverse)


#File located in OneDrive -> CL -> MICUSP -> Search Results
TA_combined <- read_csv('MICUSP_TA_combined.csv')

#Inspect
head(TA_combined)
tail(TA_combined)
sample_n(TA_combined, 10) #random sample of 10 rows

#Show particular rows
filter(TA_combined, TA == 'future_perfect') %>% #all future perfect rows
  filter(Discipline == 'MEC') # all future perfect AND MEC rows

#Subset for disciplines (or anything else)
MEC <- filter(TA_combined, Discipline == "MEC")
freq_MEC <-  MEC %>% count(TA)

#Subset for a particular file: IOE.G0.03.2
#Look up the doc ID in Sketch Engine
IOE.G0.03.2 <- filter(TA_combined, Reference == 'doc#172') 
freq_specificfile <- IOE.G0.03.2 %>% count(TA) %>% arrange(desc(TA))

#Count TAs in each document (would need to fix the counts)
test %>% 
  group_by(filename) %>%
  count(TA)

#test.csv
test <- read_csv(file.choose())

#Display total document counts per TA while preserving the other columns
test2 <- test %>% 
  group_by(filename, discipline, nativeness, year, gender) %>%
  count(TA) 

#Add column for tense, aspect
test2$tense <- recode(test2$TA, past_perfect = "past", past_progressive = "past", past_perfect_progressive = "past", past_simple = "past")
test2$aspect <- recode(test2$TA, past_perfect = "perfect", past_progressive = "progressive", past_perfect_progresive = "perfect progressive", past_simple = "simple")

#Reorder columns
test2 <- select(test2, TA, tense, aspect, everything()) #TA, tense, aspect, then everything else

#correct the values
test3 <- test2 %>%
  ungroup %>% 
  select(-tense, -aspect) %>%
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_simple - past_progressive - past_perfect - past_perfect_progressive,  
         TA = 'past_simple', .keep = 'unused') %>% 
  left_join(test2 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused')

##START HERE##

#Count frequency
freq_TA_combined <- TA_combined %>% count(TA)
freq_TA_combined <- arrange(freq_TA_combined, desc(TA)) #Arrange in descending alphabetical order

#Need to fix totals
#Add a column for the correct frequencies (with subtraction)
freq_TA_combined <- mutate(freq_TA_combined,
       Freq = n)

#Correct value for present perfect
freq_TA_combined[4, 3] <- freq_TA_combined[4, 2] - freq_TA_combined[3, 2]

#Correct value for simple present
freq_TA_combined[1, 3]<- freq_TA_combined[1, 3] - freq_TA_combined[2, 3] - freq_TA_combined[3, 3] - freq_TA_combined[4, 3] - freq_TA_combined[10, 3]

#Correct value for past perfect
freq_TA_combined[8, 3] <- freq_TA_combined[8, 2] - freq_TA_combined[7, 2]

#Correct value for simple past
freq_TA_combined[5, 3]<- freq_TA_combined[5, 3] - freq_TA_combined[6, 3] - freq_TA_combined[7, 3] - freq_TA_combined[8, 3] 

#Add row for simple future
add_row(freq_TA_combined)
freq_TA_combined[13,1] <- 'future_simple'
freq_TA_combined[13,2] <- freq_TA_combined[9, 2] + freq_TA_combined[10, 2]

#Delete individual rows for will and going to
freq_TA_combined <- freq_TA_combined[-c(9,10), ]

#Correct value for simple future
freq_TA_combined[11, 3]<- freq_TA_combined[11, 2] - freq_TA_combined[10, 2] - freq_TA_combined[9, 3]

#Delete n column
freq_TA_combined <- freq_TA_combined[, -2]

#Add row for future perfect progressive
add_row(freq_TA_combined)
freq_TA_combined[12,1] <- 'future_perfect_progressive'
freq_TA_combined[12,2] <- 0

#Percentage of total
total_verbs <- sum(freq_TA_combined$Freq) #Total number of hits
freq_TA_combined <- mutate(freq_TA_combined,
                   Percentage = (Freq / total_verbs) * 100)
freq_TA_combined$Percentage <- round(freq_TA_combined$Percentage, 2) #round to 2 decimal places

#Arrange in descending order by frequency
freq_TA_combined <- arrange(freq_TA_combined, desc(Freq)) 

#Save this tibble as csv
write.table(freq_TA_combined, 'freq_TA_combined_MICUSP.csv')
#To load again
read_csv('freq_TA_combined_MICUSP.csv')

#Merge tibbles for comparison
all_disciplines <- left_join(freq_TA_combined, freq_BIO) %>% left_join(freq_CEE) %>%
  left_join(freq_ECO) %>% left_join(freq_EDU) %>% left_join(freq_IOE) %>% 
  left_join(freq_MEC) %>% left_join(freq_NUR) %>% left_join(freq_PHY) %>% 
  left_join(freq_POL) %>% left_join(freq_PSY) %>% left_join(freq_SOC)

#Frequency only
freq_all_disciplines <- all_disciplines[ , c(1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)]
#Save this tibble as csv
write.table(freq_all_disciplines, 'freq_all_disciplines_MICUSP.csv')

#Percentage only
percent_all_disciplines <- all_disciplines[ , c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)]
#Save this tibble as csv
write.table(percent_all_disciplines, 'percent_all_disciplines_MICUSP.csv')


##Stack overflow
#dput for stack overflow
tib <- dput(test2[1:10, ])

tib <- structure(
  list(
    TA = c(
      "past_perfect",
      "past_progressive",
      "past_simple",
      "past_simple",
      "past_simple",
      "past_simple",
      "past_simple",
      "past_simple",
      "past_perfect",
      "past_progressive"
    ),
    tense = c(
      "past",
      "past",
      "past",
      "past",
      "past",
      "past",
      "past",
      "past",
      "past",
      "past"
    ),
    aspect = c(
      "perfect",
      "progressive",
      "simple",
      "simple",
      "simple",
      "simple",
      "simple",
      "simple",
      "perfect",
      "progressive"
    ),
    filename = c(
      "BIO.G0.01.1",
      "BIO.G0.01.1",
      "BIO.G0.01.1",
      "BIO.G0.02.1",
      "BIO.G0.02.2",
      "BIO.G0.02.4",
      "BIO.G0.02.5",
      "BIO.G0.02.6",
      "BIO.G0.03.1",
      "BIO.G0.03.1"
    ),
    discipline = c(
      "BIO",
      "BIO",
      "BIO",
      "BIO",
      "BIO",
      "BIO",
      "BIO",
      "BIO",
      "BIO",
      "BIO"
    ),
    nativeness = c("NS", "NS", "NS",
                   "NS", "NS", "NS", "NS", "NS", "NS", "NS"),
    year = c("G0",
             "G0", "G0", "G0", "G0", "G0", "G0", "G0", "G0", "G0"),
    gender = c("F",
               "F", "F", "M", "M", "M", "M", "M", "F", "F"),
    n = c(2L, 2L,
          57L, 39L, 3L, 4L, 49L, 103L, 1L, 1L)
  ),
  class = c("grouped_df",
            "tbl_df", "tbl", "data.frame"),
  row.names = c(NA,-10L),
  groups = structure(
    list(
      filename = c(
        "BIO.G0.01.1",
        "BIO.G0.02.1",
        "BIO.G0.02.2",
        "BIO.G0.02.4",
        "BIO.G0.02.5",
        "BIO.G0.02.6",
        "BIO.G0.03.1"
      ),
      discipline = c("BIO", "BIO", "BIO", "BIO", "BIO", "BIO",
                     "BIO"),
      nativeness = c("NS", "NS", "NS", "NS", "NS", "NS",
                     "NS"),
      year = c("G0", "G0", "G0", "G0", "G0", "G0", "G0"),
      gender = c("F", "M", "M", "M", "M", "M", "F"),
      .rows = structure(
        list(1:3, 4L, 5L, 6L, 7L, 8L, 9:10),
        ptype = integer(0),
        class = c("vctrs_list_of",
                  "vctrs_vctr", "list")
      )
    ),
    class = c("tbl_df", "tbl", "data.frame"),
    row.names = c(NA,-7L),
    .drop = TRUE
  )
)

#Creating a fake tibble to practice with
test1 <- test %>% slice_sample(n = 20) #past_sample.csv

#Simplify the number of filenames
test1[1:5,8] <- 'NUR.G0.06.1'
test1[6:10,8] <- 'NUR.G0.07.1'
test1[11:14,8] <- 'NUR.G0.08.1'
test1[15:20,8] <- 'NUR.G0.09.1'
test1

#Change the TA values
test1[1:3,1] <- 'past_perfect'
test1[6:10,1] <- 'past_perfect'
test1[11:12,1] <- 'past_perfect'
test1[1:3,1] <- 'past_perfect'
test1[16:20,1] <- 'past_perfect'
test1[4:5,1] <- 'past_perfect_progressive'
test1[13:15,1] <- 'past_perfect_progressive'
test1

#Standardize nativeness and year
test1[1:20,3] <- 'NS'
test1[1:20,4] <- 'G0'
test1

#Display total document counts per TA while preserving the other columns
test2 <- test1 %>% 
  group_by(filename, discipline, nativeness, year, gender) %>%
  count(TA) 
test2

#Subtract past perfect progressive from past perfect
test3 <- test2 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_perfect - past_perfect_progressive,  
         TA = 'past_perfect', .keep = 'unused') %>% 
  left_join(test2 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused')

#Format a tibble for past simple and past progressive
test1_1 <- test1
test1_1
test1_1[1:3,1] <- 'past_simple'
test1_1[6:10,1] <- 'past_simple'
test1_1[11:12,1] <- 'past_simple'
test1_1[1:3,1] <- 'past_simple'
test1_1[16:20,1] <- 'past_simple'
test1_1[4:5,1] <- 'past_progressive'
test1_1[13:15,1] <- 'past_progressive'
test1_1

#Display total document counts per TA while preserving the other columns
test2_1 <- test1_1 %>% 
  group_by(filename, discipline, nativeness, year, gender) %>%
  count(TA) 
test2_1

#Merge them
test4 <- rbind(test2_1, test3) %>% arrange(filename)

#Make the values of past simple higher
test4[2, 7] <- 8
test4[5, 7] <- 10
test4[8, 7] <- 6
test4[12, 7] <- 11
test4

#subtract all other past forms from past simple
test5 <- test4 %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_simple - past_progressive - past_perfect - past_perfect_progressive,  
         TA = 'past_simple', .keep = 'unused') %>% 
  left_join(test4 %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused')
test5

#Add column for tense, aspect
test5$tense <- recode(test5$TA, past_perfect = "past", past_progressive = "past", past_perfect_progressive = "past", past_simple = "past")
test5$aspect <- recode(test5$TA, past_perfect = "perfect", past_progressive = "progressive", past_perfect_progresive = "perfect progressive", past_simple = "simple")
test5

#Return tibble to the format of one verb observation per row
test5 %>% uncount(n)

# Playing around with sentence level
#Display total document counts per TA while preserving the other columns
sentences <- test1 %>% 
  group_by(filename, discipline, nativeness, year, gender, s_count, total_count) %>%
  count(TA)  

#Adjust sentence counts to put PPP and PP in the same sentences
sentences[2, 6] <- 3
sentences[2, 7] <- 8
sentences[3, 6] <- 6
sentences[3, 7] <- 6
sentences

sentences_sample <- sentences[1:9, ]
sentences_sample

#Can run the same code successfully
sentences3 <- sentences_sample %>%
  ungroup %>% 
  pivot_wider(names_from = TA, values_from = n, values_fill = 0) %>% 
  mutate(n1 = past_perfect - past_perfect_progressive,  
         TA = 'past_perfect', .keep = 'unused') %>% 
  left_join(sentences_sample %>% ungroup, .) %>% 
  mutate(n = coalesce(n1, n), .keep = 'unused')

#filter for 4-6 sentence paragraphs
sentences3 %>%
  filter(total_count == c(4, 5, 6))

#counts
sentences3 %>%
  group_by(filename) %>% #or whatever else
  count(TA)

#What to do about past perfect = 0? Are these are instances of this in the data? Should I have 0's elsewhere?
#Could filter out, like so:
filter(sentences3, n > 0)

#Check for rows where n = 0 and arrange by filename, s_count, and total_count
future1_adjusted %>% filter(n == 0) %>% arrange(filename, s_count, total_count)

#Check for rows where n is not 1 and sort in descending order
future1_adjusted %>% filter(n != 1) %>% arrange(desc(n))

#Linear model attempts
glm(n ~ TA, data = sentences3, family = "poisson") #Works? But don't know what it means
glm(n ~ TA + nativeness, data = test5, family = "poisson")

#convert to factor variables for this to work?
test5_duplicate <- test5
test5_duplicate$TA <-  as.factor(test5_duplicate$TA)
test5_duplicate$nativeness <-  as.factor(test5_duplicate$nativeness)
test5_duplicate

glm(n ~ TA, data = test5_duplicate, family = "poisson") #Works? But don't know what it means
glm(n ~ TA + nativeness, data = test5_duplicate, family = "poisson") #Doesn't work even after converting to factor


##Stack overflow
#dput for stack overflow
dput(test[1:15, ])
