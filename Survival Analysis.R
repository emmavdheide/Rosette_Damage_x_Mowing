#Survival analysis, based on https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html

#Start with based analysis of survival

#load packages
#library(knitr)
library(dplyr) 
library(survival)
library(ggplot2)
#library(tibble)

#load data
#data should be set up so that each plant has survival time in days (since the beginning of the year to start, could also use start of experiment or planting date)
#treatment
#censoring status: 0=censored (made it to the end), 1=dead
survdat<-read.csv("SurvDataAlt.csv")

#Make treatment, etc. into factors (status does not need to be a factor for this analysis)
survdat$Block <- as.factor(survdat$Block)
survdat$SubBlock <- as.factor(survdat$SubBlock)
survdat$Treatment <- as.factor(survdat$Treatment)
survdat$RosDam <- as.factor(survdat$RosDam)
survdat$EarlyMow <- as.factor(survdat$EarlyMow)
survdat$LateMow <- as.factor(survdat$LateMow)

#Add Treatment Abbreviations to Data for Exploratory Plots
survdat<-mutate(survdat, TreatmentAbb=case_when(
  RosDam == "n" & EarlyMow == "n" & LateMow == "n" ~ "Control",
  RosDam == "y" & EarlyMow == "n" & LateMow == "n" ~ "RD",
  RosDam == "y" & EarlyMow == "y" & LateMow == "n" ~ "RD+EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "n" ~ "EM",
  RosDam == "n" & EarlyMow == "y" & LateMow == "y" ~ "EM+LM",
  RosDam == "n" & EarlyMow == "n" & LateMow == "y" ~ "LM",
  RosDam == "y" & EarlyMow == "y" & LateMow == "y" ~ "RD+EM+LM",
  RosDam == "y" & EarlyMow == "n" & LateMow == "y" ~ "RD+LM",
))

#exploratory plots
hist(survdat$TimeOfDeath_doe)
ToDPlot<-survdat %>%
ggplot(aes(x=TreatmentAbb, y=TimeOfDeath_doe, fill=TreatmentAbb)) +
  geom_boxplot() +
  #geom_jitter(color="black", size=0.4, alpha=0.9) + #would include jittered points
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Time of Death by Treatment") +
  xlab("Treatment")+
  ylab("Time of Death (day of experiment)")#+
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) #angle text 45 degrees
ToDPlot

ToDPlot<-survdat %>%
  ggplot(aes(x=TreatmentAbb, y=TimeOfDeath_doe, fill=TreatmentAbb)) +
  geom_boxplot_pattern(aes(fill=Treatment, pattern=Treatment), pattern_fill = "black",
                       pattern_density = 0.05,
                       pattern_spacing = 0.05,
                       pattern_angle = 45,
                       pattern_key_scale_factor = 0.6) +
  scale_pattern_manual(values=c("none","none","none","none", "stripe","stripe","stripe","stripe"))+
  scale_fill_manual(values = c("gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3", "gray51", "lightskyblue2", "deepskyblue3","darkolivegreen3"))+
  theme(
    legend.position="none",panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1), axis.title.y = element_text(size = 25), axis.text = element_text(size = 20), plot.title = element_text(size=30)) +
  ggtitle("Time of Death") +
  xlab("Treatment")+
  ylab("Time of Death (day of experiment)")#+
#theme(axis.text.x = element_text(angle = 45, hjust = 1)) #angle text 45 degrees
ToDPlot

#look at the first 10 observations to make sure this is coded correctly
Surv(survdat$TimeOfDeath_doe, survdat$Status)[1:10]
#so the first plant survived 94 days, 2nd 83 days, etc.

#fit a survival curve to all observations and plot using Kaplan-Meier method (non-parametric)
s1 <- survfit(Surv(TimeOfDeath_doe, Status) ~ 1, data = survdat)

plot(s1, ylab="Survivorship", xlab="Day of Experiment")

#alternative plot, with confidence interval
library(ggsurvfit)

survfit2(Surv(TimeOfDeath_doe, Status) ~ 1, data = survdat) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+ 
  add_confidence_interval()


#compare survival times by treatment using a log-rank test
survdiff(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat)
#this chisquare test tells us that there is a significant difference from expected for at least one group

#Fit a survival regression with treatment as a predictor
s2 <- survfit(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat)
summary(s2)

#plot s2
survfit2(Surv(TimeOfDeath_doe, Status) ~ TreatmentAbb, data = survdat) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )+
  scale_color_manual(values =  c("coral1", "darkgoldenrod2", "olivedrab3", "springgreen3", 
                                "darkturquoise", "deepskyblue", "darkorchid1", "hotpink"))  # Change colors
  #geom_line(size = 1.5, alpha = 0.7)+ #line thickness and transparency, THIS DOESN'T WORK
  #scale_linetype_manual(values = c("solid", "dashed", "dotted", "solid", "dashed", "dotted", "solid", "dashed"))  # Change line types - THIS DOESN'T WORK
  #add_confidence_interval()
#This illustrates the different survival curves by treatment, along with 95% (FACT CHECK) confidence intervals
  
#To figure out which treatments were significantly different, we will fit parametric survival regressions with different hazard models and compare with AIC
  #We could compare some options with analysis of deviance (anova()), but that will only work for nested models
#exponential

s3<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat, dist="exponential")
summary(s3)

#Weibull (the default for survreg)
s4<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat)
summary(s4)
#try to figure out what scale = 0.132 means - R book suggests that less than 1 indicates hazard decreasing with age (pg 803)

#Gaussian
s5<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat, dist="gaussian")
summary(s5)

#Logistic
s6<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat, dist="logistic")
summary(s6)

#Lognormal
s7<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat, dist="lognormal")
summary(s7)

#Loglogistic
s8<-survreg(Surv(TimeOfDeath_doe, Status)~TreatmentAbb, data=survdat, dist = "loglogistic")
summary(s8)

#compare these options
AIC(s3, s4, s5, s6, s7, s8)
#s6 has the lowest AIC, so it looks like s6 is the best model
  #logistic hazard fits the best

#Since s6 is the best model, get pairwise comparisons
emmTOD <- emmeans(s6, specs=pairwise~TreatmentAbb, type="response")
emmTOD

#get compact letter display for pairwise comparisons
cld<-cld(emmTOD, Letters=letters)
cld

#Add these letters to the earlier ToDPlot
ToDPlot+
  geom_text(data = cld, aes(x=TreatmentAbb, y=65, label = .group), size = 5, color = "black")

#Other ways to evaluate fit would be to graph the model-based cumulative hazard against the Kaplan-Meier estimated cumulative hazard function
  #If specified form is correct, this should go through the origin with slope 1
  #But AIC should also do the trick


#try separate hazards for each treatment
s9<-survreg(Surv(TimeOfDeath_doe, Status)~strata(Treatment), data=survdat)
summary(s9)
#not sure what this is doing, possibly not useful?
#I think strata are not appropriate for treatment, since treatment is the main factor we care about in this study
#It would maybe be more appropriate if we were looking at who applied the treatment to each plot, I think, but that's not relevant for this study


#Also try Cox proportional hazards model (non-parametric)
s10<-coxph(Surv(TimeOfDeath_doe, Status)~Treatment, data = survdat)
summary(s10)
#this model gives the same result - specifically, that rosette damage/late mow is the only treatment significantly different from the control
#not sure what the second part of the output (with exp(coef), etc.) refers to

#could do some two-sample Kolmogorov-Smirnov tests for time of death?
#K-S test will only compare two distributions, but I have 8 treatments, so this isn't optimal
#the comparisons of interest would be treatments with and without rosette damage
#I already have a data frame with time of death, but I think I need proportion alive at each time point
  #Note that I previously tried KS test with Time of Death, and it gives different results

#Based on Trevor's analysis, getting proportion data
#Load individual suvival data
surv_dat<-read.csv("SurvDataIndividual.csv")

#Make row, group, column, and treatment into factors
surv_dat$Row<-as.factor(surv_dat$Row)
surv_dat$Group<-as.factor(surv_dat$Group)
surv_dat$Plant<-as.factor(surv_dat$Plant)
surv_dat$Treatment<-as.factor(surv_dat$Treatment)

#Trevor's functions to take average number (and SD and SEM) of plants remaining at a given time
na.mean <- function(x){
  mean(x)}
na.sd <- function(x){
  sd(x)}
na.sem <- function(x){
  na.sd(x)/sqrt(length(x))}

# Trevor's function to apply above functions to each treatment group
time.means <- function(df){ #function take 1 argument, a dataframe
  df_means <- apply(df[, 6:ncol(df)], MARGIN = 2, FUN = na.mean) #the next 3 lines calculate stats for each column from column 6 to the end, margin=2 tells function to apply to columns (=1 if rows)
  df_sd <- apply(df[, 6:ncol(df)], MARGIN = 2, FUN = na.sd)
  df_sem <- apply(df[, 6:ncol(df)], MARGIN = 2, FUN = na.sem)
  df_new <- data.frame(cbind(c(seq(0, 13, by = 1)), df_means, df_sem)) #create new dataframe with time column with sequence of numbers 0-13 by 1 (because we have 14 time points), also calculated means and standard errors
  names(df_new) <- c("Time", "Mean", "SEM") #name the columns in df_new
  return(df_new)} #return df_new
time.means(surv_dat)

# Trevor's function to evaluate whether two survival curves are different
# Use 2-sided K-S test, with code adapted from ks.test function
# Set function environment to "stats" for calling internally-compiled code
time.ks <- function(df1, df2){ #function that takes two dataframes as input
  #df1 <- select(df1, -Block) #in each df, remove column block (I don't think we need this)
  #df2 <- select(df2, -Block)
  df_means1 <- apply(df1[, 6:ncol(df1)], MARGIN = 2, FUN = na.mean) #calculate means of each column from column 5 to the end
  df_means2 <- apply(df2[, 6:ncol(df2)], MARGIN = 2, FUN = na.mean)
  D <- max(abs((1 - df_means1) - (1 - df_means2))) #calculates the maximum absolute difference between the transformed means from both data frames. The transformation involves subtracting each mean from 1 (originally divided df_means/25 because 25 seeds) and then taking the absolute value. This quantifies the largest deviation between the two data sets
  n <- length(6:ncol(df1)) #counts the number of columns (from the 5th to the last) in df1
  pkstwo <- function(x, tol = 1e-06, D = D){ #function for K-S test that takes argument, defines tolerance level, and maximum difference
    p <- rep(0, length(x)) #initializes a vector of zeros with the same length as x
    i <- which(x > 0) #finds the indices where x is greater than 0.
    if(length(i)){ #if positive values in x, calculate p-values for those indices
      p[i] <- .Call(C_pKS2, p = x[i], tol)}
    return(p)} #return p values
  pval <- min(1, max(0, 1 - pkstwo(sqrt(n)*D))) #p-value for the test
  return(pval)}
environment(time.ks) <- asNamespace("stats") # sets the environment of the time.ks function to be within the stats namespace, which can be useful for function visibility and scoping

#use Trevor's function to determine if survival curves are different, returned value should be p-value
time.ks(surv_dat_control, surv_dat_LM) 
  #shows that these curves are different, probably wouldn't hold up under family-wise error rate, though
  #Bonferroni correction would need 0.05/4 = .0125 for 4 tests

#Test each individual treatment vs. same thing + rosette damage, using Trevor's code
time.ks(surv_dat_control, surv_dat_RD)
time.ks(surv_dat_EM, surv_dat_RDEM)
time.ks(surv_dat_LM, surv_dat_RDLM) #this is the only significantly different pair
time.ks(surv_dat_EMLM, surv_dat_RDEMLM)


#Alternatively, the ks.test function gives us different p values
#first, subset the big dataframe by treatment
surv_dat_control<-subset(surv_dat, Treatment =="1. Control")
surv_dat_RD<-subset(surv_dat, Treatment=="2. Rosette Damage")
surv_dat_RDEM<-subset(surv_dat, Treatment=="3. Rosette Damage/Early Mow")
surv_dat_RDLM<-subset(surv_dat, Treatment=="4. Rosette Damage/Late Mow")
surv_dat_RDEMLM<-subset(surv_dat, Treatment=="5. Rosette Damage/Early Mow/Late Mow")
surv_dat_EM<-subset(surv_dat, Treatment=="6. Early Mow")
surv_dat_EMLM<-subset(surv_dat, Treatment=="7. Early Mow/Late Mow")
surv_dat_LM<-subset(surv_dat, Treatment=="8. Late Mow")

#get the means and sems of each subset data frame, and rename the columns to be more specific
control_mean_sem<-time.means(surv_dat_control)
names(control_mean_sem)<-c("Time", "Mean_Control", "SEM_Control")
RD_mean_sem<-time.means(surv_dat_RD)
names(RD_mean_sem)<-c("Time", "Mean_RD", "SEM_RD")
RDEM_mean_sem<-time.means(surv_dat_RDEM)
names(RDEM_mean_sem)<-c("Time", "Mean_RDEM", "SEM_RDEM")
RDLM_mean_sem<-time.means(surv_dat_RDLM)
names(RDLM_mean_sem)<-c("Time", "Mean_RDLM", "SEM_RDLM")
RDEMLM_mean_sem<-time.means(surv_dat_RDEMLM)
names(RDEMLM_mean_sem)<-c("Time", "Mean_RDEMLM", "SEM_RDEMLM")
EM_mean_sem<-time.means(surv_dat_EM)
names(EM_mean_sem)<-c("Time", "Mean_EM", "SEM_EM")
EMLM_mean_sem<-time.means(surv_dat_EMLM)
names(EMLM_mean_sem)<-c("Time", "Mean_EMLM", "SEM_EMLM")
LM_mean_sem<-time.means(surv_dat_LM)
names(LM_mean_sem)<-c("Time", "Mean_LM", "SEM_LM")

#Compile all of these into a single data frame
mean_sem_df<-as.data.frame(c(control_mean_sem, RD_mean_sem[,2:3], RDEM_mean_sem[,2:3], RDLM_mean_sem[,2:3], RDEMLM_mean_sem[,2:3], EM_mean_sem[,2:3], EMLM_mean_sem[,2:3], LM_mean_sem[,2:3]))

#put the pairs of interest through ks.test
ks.test(mean_sem_df$Mean_Control, mean_sem_df$Mean_RD) #control vs RD
ks.test(mean_sem_df$Mean_EM, mean_sem_df$Mean_RDEM) #EM vs RDEM
ks.test(mean_sem_df$Mean_LM, mean_sem_df$Mean_RDLM) #LM vs RDLM
ks.test(mean_sem_df$Mean_EMLM, mean_sem_df$Mean_RDEMLM) #EMLM vs RDEMLM
#These p-values are different from the ones I get with Trevor's method - not sure why
#None of these are significantly different in pairs

ks.test(mean_sem_df$Mean_Control, mean_sem_df$Mean_RDLM) #control vs. RDLM, not significant, especially with FwER correction





#Time dependent covariates? I think this is important because plants were "exposed" to different treatments at different times
  #potentially competing risks analysis because each treatment application can be thought of as a risk/death could theoretically result from any of them
#see https://cran.r-project.org/web/packages/survival/vignettes/timedep.pdf for time dependent covariates
#ChatGPT also suggests longitudinal survival models

#It might be possible to incorporate the timing of each treatment, its status, and the interaction of statuses
s11<-coxph(Surv(TimeOfDeath_doy, Status)~TimeRD_doy+StatusRD+TimeEM_doy+StatusEM+TimeLM_doy+StatusLM+StatusRD*StatusEM*StatusLM, data = survdat)
summary(s11)

#get pairwise comparisons
emmTOD_timing <- emmeans(s11, specs=pairwise~StatusRD*StatusEM*StatusLM, type="response")
emmTOD_timing
#Note that this takes the p-values out of significance territory, but control/RDLM is still the strongest contrast

#get compact letter display for pairwise comparisons
cld(emmTOD_timing, Letters=letters)

#Try for time-dependent covariates with two data structures
TDCsurvdat1<-read.csv("Time Dependent Survival Data.csv") #in this one, for example, RD has an indicator of 0 when EM is applied in RDEM
TDCsurvdat1$Status<-as.factor(TDCsurvdat1$Status)
TDCsurvdat1$RD<-as.factor(TDCsurvdat1$RD)
TDCsurvdat1$EM<-as.factor(TDCsurvdat1$EM)
TDCsurvdat1$LM<-as.factor(TDCsurvdat1$LM)
TDCsurvdat1$PlantID<-as.factor(TDCsurvdat1$PlantID)
#remove R7G1P3 because death date is same as LM application date
TDCsurvdat1 <- TDCsurvdat1[!(TDCsurvdat1$Row == 7 & TDCsurvdat1$Group == 1 & TDCsurvdat1$Plant == 3), ]


TDCsurvdat2<-read.csv("Time Dependent Survival Data 2.csv") #in this one, RD has an indicator of 1 when EM is applied in RDEM
TDCsurvdat2$Status<-as.factor(TDCsurvdat2$Status)
TDCsurvdat2$RD<-as.factor(TDCsurvdat2$RD)
TDCsurvdat2$EM<-as.factor(TDCsurvdat2$EM)
TDCsurvdat2$LM<-as.factor(TDCsurvdat2$LM)
#remove R7G1P3 because death date is same as LM application date
TDCsurvdat2 <- TDCsurvdat2[!(TDCsurvdat2$Row == 7 & TDCsurvdat2$Group == 1 & TDCsurvdat2$Plant == 3), ]

#fit a cox proportional hazard model with time-dependent covariates
#with the first data structure
s12<-coxph(Surv(Time1_doe, Time2_doe, Status)~RD+EM+LM+strata(PlantID), data = TDCsurvdat1)
#might need an id variable for every row in the data set, rather than every plant?

#or analysis with time of treatment? Fit a treatment specific model, treatment*timing, and timing only and compare models with anova?
#Look at Rui's paper and her frequency vs timing analysis - would we still conclude that timing is more important than frequency?
#Is that true if we analyze the data all together or just the early/late mow stuff?
#and then bring in rosette damage to say timing is important, but so is legacy

#And also hurdle models for survival? Like given that the plant survived past a certain date, how did it do?