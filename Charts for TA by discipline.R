#Counting and plotting MICUSP frequencies compared by discipline

#Plotting and modeling MICUSP data (updated)

#Set working directory to source file location

#Load packages
library(tidyverse)
library(ggplot2)
library(forcats)

#Load data
Percent_by_disc <- read_csv('Percent_by_disc.csv') 

#Present simple
present_simple <- Percent_by_disc %>%
  select(discipline, present_simple)

ggplot(present_simple, aes(x = fct_reorder(discipline, present_simple, .desc = TRUE), y = present_simple)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_cartesian(ylim = c(0.5, 0.8)) +
  labs(
    x = "Discipline",
    y = "Percentage",
    title = "Percentage of finite verbs in present simple for each discipline"
  )

#Past simple
past_simple <- Percent_by_disc %>%
  select(discipline, past_simple)

ggplot(past_simple, aes(x = fct_reorder(discipline, past_simple, .desc = TRUE), y = past_simple)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_cartesian(ylim = c(0.1, 0.35)) +
  labs(
    x = "Discipline",
    y = "Percentage",
    title = "Percentage of finite verbs in past simple for each discipline"
  )

#Present perfect
present_perfect <- Percent_by_disc %>%
  select(discipline, present_perfect)

ggplot(present_perfect, aes(x = fct_reorder(discipline, present_perfect, .desc = TRUE), y = present_perfect)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.08)) +
  labs(
    x = "Discipline",
    y = "Percentage",
    title = "Percentage of finite verbs in present perfect for each discipline"
  )

#Future simple
future_simple <- Percent_by_disc %>%
  select(discipline, future_simple)

ggplot(future_simple, aes(x = fct_reorder(discipline, future_simple, .desc = TRUE), y = future_simple)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.08)) +
  labs(
    x = "Discipline",
    y = "Percentage",
    title = "Percentage of finite verbs in future simple for each discipline"
  )


#Present progressive
present_progressive <- Percent_by_disc %>%
  select(discipline, present_progressive)

ggplot(present_progressive, aes(x = fct_reorder(discipline, present_progressive, .desc = TRUE), y = present_progressive)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 0.025)) +
  labs(
    x = "Discipline",
    y = "Percentage",
    title = "Percentage of finite verbs in present progressive for each discipline"
  )


##Stacked bar plot for all disciplines and top 5 TAs
#Pivot and filter data
percent_pivoted <- Percent_by_disc %>%
  pivot_longer(
    cols = -discipline,
    names_to = "TA",
    values_to = "percentage"
  ) %>%
  na.omit() %>% #Remove the weird extra rows added 
  #Filter for top 5 TAs 
  filter(TA == "present_simple" | TA == "past_simple" | TA == "present_perfect" |
           TA == "future_simple" | TA == "present_progressive") %>% 

#Plot
ggplot(percent_pivoted, aes(x= discipline, y = percentage, fill = fct_reorder(TA, percentage, .desc = FALSE)))+
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(name = "TA", palette = "Spectral") +
  theme_minimal() +
  labs(
    x = "Discipline",
    y = "Percentage",
  title = "Percentage of finite verbs in the top 5 TAs for each discipline"
  )


#A way to calculate manually from R
#By discipline
#BIO
BIO <- MICUSP_by_filename1 %>%
  filter(discipline == "BIO") %>%
  group_by(TA) %>%
  summarize(frequency = sum(frequency)) %>%
  mutate(percentage = round(frequency / sum(frequency) * 100, digits = 2))

#CEE

#ECO

#EDU

#IOE

#MEC

#NUR

#PHY

#POL

#PSY

#SOC
