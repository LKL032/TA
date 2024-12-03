#Modeling MICUSP data

#Set working directory to source file location

#Load packages
library(tidyverse)
library(lme4)
library(broom)
library(ggplot2)
library(dplyr)
library(emmeans)
library(mixed)

#Load and inspect data
MICUSP_by_filename1 <- read_csv('MICUSP_by_filename1.csv') #deleted the first column manually before saving
head(MICUSP_by_filename)
str(MICUSP_by_filename)

#Exploratory plots
#Boxplot of frequency per text for each TA
ggplot(MICUSP_by_filename1, aes(TA, frequency)) +
  geom_boxplot()

#Same boxplot with log frequency
#Logtransform
MICUSP_by_filename1 <- MICUSP_by_filename1 %>%
  mutate(logfrequency = log10(frequency + 0.001)) #note this later
#Plot
  ggplot(MICUSP_by_filename1, aes(TA, logfrequency)) +
  geom_boxplot()
#Add theme_classic(), + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Boxplot comparing NS and NNs 
ggplot(MICUSP_by_filename1, aes(TA, logfrequency)) +
  geom_boxplot()+
  facet_wrap(~nativeness)

#Models

#Frequency as a function of TA
#Note that "intercept" in this model is the average future perfect and everything else is added or subtracted from that average 
TA_model <-  glm(frequency ~ TA, data = MICUSP_by_filename1, family = 'poisson')
tidy(TA_model) #To see estimate, standard error, p value
tidy(TA_model, conf.int = TRUE) #To see CIs

#Other stuff to explore
summary(TA_model)
coef(TA_model) #Extract coefficients
exp(coef(TA_model)) #Extract and exponentiate coefficients
confint(TA_model) # Confidence intervals

#Model comparison
null_model <- glm(frequency ~ 1, data = MICUSP_by_filename1, family = 'poisson')
tidy(null_model, exponentiate = TRUE)

#Compare to null hypothesis
anova(null_model, TA_model, test = "Chisq") 
anova(TA_model)
glance(TA_model)

#Predict values based on this model (this just gives you the mean of each TA)
prediction_data <- MICUSP_by_filename1 %>%
  mutate(
    predictions = exp(predict(
      TA_model, MICUSP_by_filename1)))


#Model with average per category
TA_model_percategory <- glm(frequency ~ TA - 1, data = MICUSP_by_filename1, family = 'poisson')
tidy(TA_model_percategory, exponentiate = TRUE) 

#Pairwise comparisons with emmeans
emmeans(TA_model, list(pairwise ~ TA),
        adjust = 'bonferroni')
#all but 3 have significant p-values but what does this mean?

#Not sure what this output means
emmeans(TA_model, "TA", type = "discipline")

#Frequency as a function of interaction between TA and discipline
TA_plus_disc_model <-  glm(frequency ~ TA + discipline, data = MICUSP_by_filename1, family = 'poisson')
TA_plus_disc_model_tib <- tidy(TA_inter_disc_model, exponentiate = TRUE)

#Frequency as a function of interaction between TA and discipline
TA_inter_disc_model <-  glm(frequency ~ TA * discipline, data = MICUSP_by_filename1, family = 'poisson')
TA_inter_disc_model_tib <- tidy(TA_inter_disc_model, exponentiate = TRUE)

#Compare the models with anova
anova(TA_plus_disc_model, TA_inter_disc_model, test = "Chisq") 

#Pairwise comparisons with emmeans
emmeans(TA_inter_disc_model, list(pairwise ~ TA | discipline),
        adjust = 'bonferroni')

#tense and aspect vs. TA
tense_aspect_model <-  glm(frequency ~ tense + aspect + TA, data = MICUSP_by_filename1, family = 'poisson')
tidy(tense_aspect_model, exponentiate = TRUE, conf.int = TRUE) #To see exponentiated values and CIs

#Frequency as a function of TA and random slopes for discipline
TA_disc_model <- glmer(frequency ~ TA + (1 + TA|discipline), data = MICUSP_by_filename1, family = 'poisson')
TA_disc_model
coef(TA_disc_model)

#Warning messages 
#1: In commonArgs(par, fn, control, environment()) :
  #maxfun < 10 * length(par)^2 is not recommended.
#2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
  #convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
#3: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
  #failure to converge in 10000 evaluations
#4: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
  #convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations
#5: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #unable to evaluate scaled gradient
#6: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
  #Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

#With adjustment based on above warning
glmer(frequency ~ TA + (1 + TA|discipline), data = MICUSP_by_filename1, family = 'poisson', control=glmerControl
      (optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#Only one warning message this time
#In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
#convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded

#With additional adjustment based on above warning
glmer(frequency ~ TA + (1 + TA|discipline), data = MICUSP_by_filename1, family = 'poisson', control=glmerControl
      (optimizer="bobyqa", optCtrl=list(maxfun=1000000)))
#couldn't run this, took too long

#Frequency as a function of TA and random slopes for discipline (with intercept) 
TA_disc_model_inter <- glmer(frequency ~ 1 + TA + (1 + TA|discipline), data = MICUSP_by_filename1, family = 'poisson')
TA_disc_model_inter

#for nativeness (only 2 levels)
glmer(frequency ~ TA + (1 + TA|nativeness), data = MICUSP_by_filename1, family = 'poisson')

#Warning messages:
#1: In commonArgs(par, fn, control, environment()) :
 # maxfun < 10 * length(par)^2 is not recommended.
#2: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
  #convergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded
#3: In (function (fn, par, lower = rep.int(-Inf, n), upper = rep.int(Inf,  :
   #failure to converge in 10000 evaluations
#4: In optwrap(optimizer, devfun, start, rho$lower, control = control,  :
    #convergence code 4 from Nelder_Mead: failure to converge in 10000 evaluations
