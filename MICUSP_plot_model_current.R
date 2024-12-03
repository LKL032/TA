#Plotting and modeling MICUSP data (updated)

#Set working directory to source file location

#Load packages
library(tidyverse)
library(lme4)
library(broom)
library(ggplot2)
library(dplyr)
library(forcats)
library(afex)
library(optimx)
library(minqa)
library(dfoptim)
library(car)
library(pscl)
library(MASS)


#Load and inspect data
MICUSP_by_filename1 <- read_csv('MICUSP_by_filename1.csv') #deleted the first column manually before saving
head(MICUSP_by_filename1)
str(MICUSP_by_filename1)

#Transform filename, discipline, nativeness, TA, tense, aspect to factor variables
MICUSP_by_filename1 <- MICUSP_by_filename1 %>%
  mutate(filename = as.factor(filename),
         discipline = as.factor(discipline),
         nativeness = as.factor(nativeness),
         tense = as.factor(tense),
         aspect = as.factor(aspect),
         TA = as.factor(TA))

#Exploratory plots

#Boxplot of frequency by TA
ggplot(MICUSP_by_filename1, aes(x= fct_reorder(TA, frequency, mean), y = frequency, color = tense)) +
  geom_boxplot()+
  theme_classic() +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "TA",
    y = "Frequency",
    title = "Frequency count of each TA by text in MICUSP"
  )
  
#Logtransform frequency
MICUSP_by_filename1 <- MICUSP_by_filename1 %>%
  mutate(logfrequency = log10(frequency + 1)) 
#Boxplot of logfrequency by TA
ggplot(MICUSP_by_filename1, aes(x= fct_reorder(TA, frequency, mean), y = logfrequency, color = tense)) +
  geom_boxplot()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    x = "TA",
    y = "Frequency",
    title = "Log frequency count of each TA by text in MICUSP"
  )

#Models

#relevel TA?  
MICUSP_by_filename1$TA <-  relevel(MICUSP_by_filename1$TA, ref = "present_perfect")
MICUSP_by_filename1$tense <- relevel(MICUSP_by_filename1$tense, ref = "present")
MICUSP_by_filename1$aspect <- relevel(MICUSP_by_filename1$aspect, ref = "perfect")

#tense + aspect
tense_aspect_model <-  glm(frequency ~ tense + aspect , data = MICUSP_by_filename1, family = 'poisson')
tense_aspect_model_df <- tidy(tense_aspect_model, conf.int = TRUE) 
summary(tense_aspect_model)
anova(tense_aspect_model, test = "Chisq")

#TA (how I wrote about)
TA_model <-  glm(frequency ~ TA, data = MICUSP_by_filename1, family = 'poisson')
TA_model_df <- tidy(TA_model)
summary(TA_model)
anova(TA_model, test = "Chisq")

#TA (separately)

TA_mod <-  glm(frequency ~ tense * aspect, data = MICUSP_by_filename1, family = 'poisson')
TA_mod_df <- tidy(TA_mod)
summary(TA_mod)

anova(tense_aspect_model, TA_mod, test = "Chisq")

#Compare models
anova(tense_aspect_model, TA_model, test = "Chisq")
#Suggests combined TA is a better model


#TA + discipline
TA_disc_model <-  glm(frequency ~ TA + discipline, data = MICUSP_by_filename1, family = 'poisson')
TA_disc_model_df <- tidy(TA_disc_model) %>%
  #quickly see which p.values are significant
  mutate(significance = case_when(
    p.value < 0.05 ~ "YES"))
summary(TA_disc_model)

#TA * discipline
TA_disc_inter_model <-  glm(frequency ~ TA * discipline, data = MICUSP_by_filename1, family = 'poisson')
TA_disc_inter_model_df <- tidy(TA_disc_inter_model)  %>%
  #quickly see which p.values are significant
  mutate(significance = case_when(
    p.value < 0.05 ~ "YES"))
summary(TA_disc_inter_model)

#Compare models
anova(TA_disc_model, TA_disc_inter_model, test = "Chisq")
#Suggests TA * discipline is a better model


#T + A + discipline
T_A_disc_model <-  glm(frequency ~ tense + aspect + discipline, data = MICUSP_by_filename1, family = 'poisson')
T_A_disc_model_df <- tidy(T_A_disc_model) %>%
  #quickly see which p.values are significant
  mutate(significance = case_when(
    p.value < 0.05 ~ "YES"))
summary(T_A_disc_model)

#T * discipline + A * discipline
T_A_disc_inter_model <-  glm(frequency ~ tense * discipline + aspect * discipline, data = MICUSP_by_filename1, family = 'poisson')
T_A_disc_inter_model_df <- tidy(T_A_disc_inter_model)  %>%
  #quickly see which p.values are significant
  mutate(significance = case_when(
    p.value < 0.05 ~ "YES"))
summary(T_A_disc_inter_model)

anova(T_A_disc_model, T_A_disc_inter_model, test = "Chisq" )
#Suggests the one with interactions is better

anova(T_A_disc_inter_model, TA_disc_inter_model, test = "Chisq" )
#Suggests TA (not T_A) is better

#Correct order- last is best
anova(T_A_disc_model, TA_disc_model, T_A_disc_inter_model, TA_disc_inter_model, test = "Chisq" )

anova(TA_disc_inter_model, TA_disc_inter_model_ctrl, test = "Chisq")

#TA * discipline + nativeness
TA_disc_inter_model_ctrl <-  glm(frequency ~ TA * discipline + nativeness, data = MICUSP_by_filename1, family = 'poisson')
TA_disc_inter_model_ctrl_df <- tidy(TA_disc_inter_model_ctrl) %>%
  #quickly see which p.values are significant
  mutate(significance = case_when(
    p.value < 0.05 ~ "YES"))
summary(TA_disc_inter_model_ctrl)

#Just TA and nativeness
TA_nativeness <- glm(frequency ~ TA + nativeness, data = MICUSP_by_filename1, family = "poisson")
summary(TA_nativeness)

anova(TA_nativeness, TA_disc_inter_model_ctrl, test = "Chisq")


#Testing the model

#To show that both variables are still significant, Levshina p. 152
drop1(TA_disc_inter_model_ctrl, test = "F") 

#step 
step(TA_disc_inter_model_ctrl, direction = "back")

#anova
anova(TA_disc_inter_model_ctrl, test= "Chisq")



#Fit negative binomial model
TA_disc_inter_model_ctrl_nb <-  glm.nb(frequency ~ TA * discipline + nativeness, 
                                 data = MICUSP_by_filename1)
summary(TA_disc_inter_model_ctrl_nb)

#Check for overdispersion
odTest(TA_disc_inter_model_ctrl_nb)

#Check for collinearity
vif(TA_disc_inter_model_ctrl_nb)

#Plot residuals- how to interpret this?
plot(TA_disc_inter_model_ctrl_nb, which = 1)
plot(TA_disc_inter_model_ctrl_nb, which = 2)

#Check for autocorrelation between residuals
durbinWatsonTest(TA_disc_inter_model_ctrl_nb)

#Check for normality of residuals
shapiro.test(residuals(TA_disc_inter_model_ctrl_nb))

#Something new, couldn't run it without errors
mixed_model <- glmer(frequency ~ TA * discipline + nativeness + (1|filename), 
                     data = MICUSP_by_filename1, family = "poisson", control=glmerControl
                     (optimizer="bobyqa", optCtrl=list(maxfun=100000)))
summary(mixed_model)

#Try to run it step by step (also didn't work)
random <- glmer(frequency ~ (1|filename), data = MICUSP_by_filename1, family = "poisson")
summary(random)

m1 <- update(random,.~. + nativeness)
summary(m1)

m2 <- update(m1,.~. + TA * discipline)
#1: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
#failure to converge in 10000 evaluations
#2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations
#3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#unable to evaluate scaled gradient
#4: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#Model failed to converge: degenerate  Hessian with 5 negative eigenvalues

#Trying to optomize
mixed_model <- glmer(frequency ~ TA * discipline + nativeness + (1|filename), 
                     data = MICUSP_by_filename1, family = "poisson")

fitted <- all_fit(mixed_model)

fitted_OK <- fitted[sapply(fitted, is, "merMod")]
lapply(fitted_OK, function(x) x@optinfo$conv$lme4$messages)

