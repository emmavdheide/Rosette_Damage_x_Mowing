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
plot(survdat$Treatment, survdat$TimeOfDeath_doe)
hist(survdat$TimeOfDeath_doe)
survdat %>%
ggplot(aes(x=Treatment, y=TimeOfDeath_doe, fill=Treatment)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) + #would include jittered points
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Time of Death (day of experiment) by Treatment") +
  xlab("Treatment")+
  ylab("Date of Death")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #angle text 45 degrees

#Create a survival curv using the Kaplan-Meier method and survival package
#look at the first 10 observations to make sure this is coded correctly
Surv(survdat$TimeOfDeath_doe, survdat$Status)[1:10]
#so the first plant survived 216 days, 2nd 205 days, etc.

s1 <- survfit(Surv(TimeOfDeath_doe, Status) ~ 1, data = survdat)
str(s1) #look at structure of s1

plot(s1, ylab="Survivorship", xlab="Day of Experiment")

#Kaplan-Meier plots (Kaplan-Meier is non-parametric)
library(ggsurvfit)

survfit2(Surv(TimeOfDeath_doe, Status) ~ 1, data = survdat) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+ 
  add_confidence_interval()


#compare survival times by treatment using a log-rank test
survdiff(Surv(TimeOfDeath_doe, Status) ~ Treatment, data = survdat)
#this chisquare test tells us that there is a significant difference from expected for at least one group

#Try it with time-dependent covariates
#for this bit I'm adding columns to the dataset with time of rosette damage, early mow, and late mow, and the status of each (0=did not happen, 1=happened)
#UPDATE THIS ONCE I'M LOOKING AT TREATMENT RECORDS, BECAUSE I DON'T THINK EVERY PLANT GOT EVERY ASSIGNED EVENT

#Try something with treatment as a regression factor
s2 <- survfit(Surv(TimeOfDeath_doe, Status) ~ Treatment, data = survdat)
str(s2) #look at structure of s2
summary(s2)

plot(s2, ylab="survivorship", xlab="Day of Experiment")

#plot s2
survfit2(Surv(TimeOfDeath_doe, Status) ~ Treatment, data = survdat) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+ 
  add_confidence_interval()
#This seems okay. x-axis is too long but otherwise this isn't bad
#Could probably also subset data by rosette damage/early/late and do the same thing for those individual treatment components?
  #that would be kind of weird in terms of correcting for multiple tests, though

#Parametric models with survreg
#NEED TO PICK A DISTRIBUTION BASED ON THE HAZARD
#EXPONENTIAL ASSUMES CONSTANT HAZARD
s3<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat, dist="exponential")
summary(s3)

#Weibull is the default for survreg, non-constant hazard
s4<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat)
summary(s4)
#This is better because the expected treatment is significantly different from the control
#try to figure out what scale = 0.132 means - R book suggests that less than 1 indicates hazard decreasing with age (pg 803)

#compare s3 and s4
anova(s3, s4)
#s4 is a significant improvement over s3
#however, this likelihood ratio test only works with nested models
#AIC should work with non-nested models
AIC(s3,s4) #this indicates that s4 is better

#try a gaussian distribution
s5<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat, dist="gaussian")
summary(s5)

#compare gaussian to weibull
#likelihood ratio test won't work because these aren't nested
AIC(s4, s5)
#s5 (gaussian) is a bit better

#try logistic
s6<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat, dist="logistic")
summary(s6)

AIC(s5, s6) 
#s6 is better

#try lognormal
s7<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat, dist="lognormal")
summary(s7)

AIC(s6, s7)
#s6 is better

#try loglogistic
s8<-survreg(Surv(TimeOfDeath_doe, Status)~Treatment, data=survdat, dist = "loglogistic")
summary(s8)

AIC(s6, s8) 
#s6 is better

#try separate hazards for each treatment
s9<-survreg(Surv(TimeOfDeath_doe, Status)~strata(Treatment), data=survdat)
summary(s9)
#not sure what this is doing, possibly not useful?
#I think strata are not appropriate for treatment, since treatment is the main factor we care about in this study
#It would maybe be more appropriate if we were looking at who applied the treatment to each plot, I think, but that's not relevant for this study


#Also try Cox proportional hazards model