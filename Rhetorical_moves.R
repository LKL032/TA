##Loading & analyzing rhetorical moves from STP corpus 

#Load packages
library(tidyverse)
library(dplyr)
library(stringi)
library(ggplot2)
library(forcats)
library(pals)

#Set WD to file location

#Load lists by move from UAM Corpus Tool
territory <- read_csv('territory.csv')
gap <- read_csv('gap.csv')
goal <- read_csv('goal.csv')
means <- read_csv('means.csv')
previous <- read_csv('previous_research.csv')
achievement <- read_csv('achievement.csv')
benefits <- read_csv('benefits.csv')
competence <- read_csv('competence.csv')
importance <- read_csv('importance.csv')
hypothesis <- read_csv('hypothesis.csv')
timeline <- read_csv('timeline.csv')

#Merge into a single tibble
rhetorical_moves <- rbind(territory, gap, goal, means, previous, achievement, 
                          benefits, competence, importance, hypothesis, timeline)

#Create discipline column
rhetorical_moves <- rhetorical_moves %>% mutate(discipline = case_when(
  stri_detect_fixed(filename, "Bio") ~ "Biology",
  stri_detect_fixed(filename, "Chem") ~ "Chemistry",
  stri_detect_fixed(filename, "PolSci") ~ "Polticial Science",
  stri_detect_fixed(filename, "Psych") ~ "Psychology"))

#Add schools column
rhetorical_moves <- rhetorical_moves %>% mutate(school = case_when(
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

#Reorder columns
rhetorical_moves <- select(rhetorical_moves, move, filename, discipline, school, text)


#Count moves, arrange in descending order, and add percentages
moves_counted <-rhetorical_moves %>%
  count(move) %>%
  arrange(desc(n)) %>%
  mutate(percentage = n / sum(n))



#Plotting attempts
#Plot a histogram (doesn't really make sense)
ggplot(moves_counted, aes(n)) +
  geom_histogram()

#Plot a scatterplot (better as a table)
ggplot(moves_counted, aes(x = move, y = n)) +
  geom_point()


#Group by discipline
moves_by_discipline <- rhetorical_moves %>%
  group_by(discipline)%>% 
  count(move)
moves_by_discipline

#Bar plot of frequency of each move by discipline
ggplot(moves_by_discipline, aes(x= fct_reorder(move, n, mean), y = n, fill = discipline)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Rhetorical move",
    y = "Frequency",
    title = "Frequency of each move by discipline"
  )

#Bar plot of frequency of each move within each discipline- need to find a better color palette
ggplot(moves_by_discipline, aes(x= discipline, y = n, fill = move)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  scale_fill_brewer(name = "move", palette = "Paired") +
  labs(
    x = "Discipline",
    y = "Frequency",
    title = "Frequency of each move in each discipline"
  )


#Group by filename
moves_by_filename <- rhetorical_moves %>%
  group_by(filename, discipline, school)%>% 
  count(move)
moves_by_filename

#Expand so that there are 0's for missing values
moves_expanded <- moves_by_filename %>% 
  ungroup() %>% 
  complete(move, filename, fill = list(n = 0L)) %>%
  arrange(filename)
moves_expanded

#Fill in missing NA values
#discipline
moves_expanded <- moves_expanded %>% mutate(discipline = case_when(
  stri_detect_fixed(filename, "Bio") ~ "Biology",
  stri_detect_fixed(filename, "Chem") ~ "Chemistry",
  stri_detect_fixed(filename, "PolSci") ~ "Polticial Science",
  stri_detect_fixed(filename, "Psych") ~ "Psychology"))

#school
moves_expanded <- moves_expanded %>% mutate(school = case_when(
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
moves_expanded

#Boxplot of moves per text 
ggplot(moves_expanded, aes(x= fct_reorder(move, n, median), y = n)) +
  geom_boxplot()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Rhetorical move",
    y = "Frequency",
    title = "Frequency count of each move by text in STP corpus"
  )


#Filter for rows where n = 0 and count by move
zero_counts <- moves_expanded %>%
  filter(n == 0) %>%
  count(move)
#The only one without a 0 value is means, meaning that's the only one 100% obligatory

#Calculate percentage of texts that each move appears in
text_counts <- zero_counts %>%
  mutate(total_texts = 32 - n,
         percent_texts = total_texts / 32) %>%
  select(-n) %>% 
  add_row(move = "means", total_texts = 32, percent_texts = 1.0) %>%
  arrange(desc(percent_texts))
#importance, achievement, previous_research, goal, territory are "conventional"
#hypothesis, gap, competence are really close (round up to .6)
#benefits and timeline are optional

#Bar plot of frequency of each move within each text by percentage of texts
ggplot(text_counts, aes(x= move, y = percent_texts)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "Rhetorical move",
    y = "Percentage of texts",
    title = "Percentage of texts in which each move appears"
  )


##Get info on texts per school
#Count number of texts per school (there's probably a more efficient way to do this!)
texts_per_school <- rhetorical_moves %>% 
  group_by(filename) %>% 
  count(school) %>% 
  group_by(school) %>% 
  count(school)

#Rename columns
texts_per_school <- texts_per_school %>%
  rename("School" = "school",
         "Texts" = "n")

#Add column for type of school
texts_per_school$Type <- c("Faith-based", "Liberal Arts", "Faith-based", 
                                 "Faith-based", "Public", "Ivy League", "Public", 
                                 "Faith-based", "Liberal Arts", "Public", "Public",
                                 "Public", "Public", "Public", "Public", "Public", 
                                 "Public", "Public")

#Add column for location
texts_per_school$Location <- c("Berrien Springs, MI", "Lewiston, MA", 
                                     "Provo, UT", "St. Joseph, MN", "Fort Collins, CO", 
                                     "Hanover, NH", "Ypsilanti, MI", "St. Peter, MN",
                                     "Salem, VA", "Fayetteville, AR", "Berkeley, CA",
                                     "Orange County, FL", "Honolulu, HI", "Lafayette, LA", 
                                     "Charlotte, NC", "Burlington, VT", "Pullman, WA",
                                     "West Liberty, WV")

#Reorder columns
texts_per_school <- select(texts_per_school, School, Type, Location, Texts)

#Export as CSV
write.csv(texts_per_school, file = "proposal_schools.csv")
