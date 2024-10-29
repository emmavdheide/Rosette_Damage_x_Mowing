#Survival analysis, based on https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html

#Start with based analysis of survival

#load packages
library(knitr)
library(dplyr)
library(survival)
library(ggplot2)
library(tibble)
#library(tidycmprsk)

#load data
#data should be set up so that each plant has survival time in days (since the beginning of the year to start, could also use start of experiment or planting date)
#treatment
#censoring status: 0=censored (made it to the end), 1=dead
survdat<-read.csv("SurvDataAlt.csv")

#Make treatment into a factor (status does not need to be a factor for this analysis)
survdat$Treatment<-as.factor(survdat$Treatment)

#exploratory plots
plot(survdat$Treatment, survdat$TimeOfDeath)
survdat %>%
ggplot(aes(x=Treatment, y=TimeOfDeath, fill=Treatment)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Time of Death (day of year) by Treatment") +
  xlab("")

#Create a survival curv using the Kaplan-Meier method and survival package
#look at the first 10 observations to make sure this is coded correctly
Surv(survdat$TimeOfDeath, survdat$Status)[1:10]
#so the first plant survived 216 days, 2nd 205 days, etc.

s1 <- survfit(Surv(TimeOfDeath, Status) ~ 1, data = survdat)
str(s1) #look at structure of s1

#Kaplan-Meier plots
library(ggsurvfit)

survfit2(Surv(TimeOfDeath, Status) ~ 1, data = survdat) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+ 
  add_confidence_interval()


#compare survival times by treatment using a log-rank test
survdiff(Surv(TimeOfDeath, Status) ~ Treatment, data = survdat)
#this chisquare test tells us that there is a significant difference from expected for at least one group

#Try it with time-dependent covariates
#for this bit I'm adding columns to the dataset with time of rosette damage, early mow, and late mow, and the status of each (0=did not happen, 1=happened)
#UPDATE THIS ONCE I'M LOOKING AT TREATMENT RECORDS, BECAUSE I DON'T THINK EVERY PLANT GOT EVERY ASSIGNED EVENT

#Try something with treatment as a regression factor
s2 <- survfit(Surv(TimeOfDeath, Status) ~ Treatment, data = survdat)
str(s2) #look at structure of s2

#plot s2
survfit2(Surv(TimeOfDeath, Status) ~ Treatment, data = survdat) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+ 
  add_confidence_interval()
#This seems okay. x-axis is too long but otherwise this isn't bad
#Could probably also subset data by rosette damage/early/late and do the same thing for those individual treatment components?
  #that would be kind of weird in terms of correcting for multiple tests, though

crr(Surv(TimeOfDeath, Status)~Treatment, data=survdat)
