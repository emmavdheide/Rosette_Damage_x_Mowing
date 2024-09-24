#set working directory
#setwd("~/Desktop") #for Olivia
setwd("~/Grad School/Penn State/Project Information/Thistle Nursery for Summer 2024") #for Emma

#load data
dat <- read.csv("ManuscriptData1.csv")

#make data into factors
dat$Block <- as.factor(dat$Block)
dat$SubBlock <- as.factor(dat$SubBlock)
dat$Treatment <- as.factor(dat$Treatment)

#fit flowering date model
fitfd <- lm(FloweringDate~Treatment, data = dat)
summary(fitfd)

#ANOVA
anova(fitfd)

#residuals
plot(fitfd)


#fit height at flowering model
fithf <- lm(HeightatFlowering~Treatment, data = dat)
summary(fithf)

#ANOVA
anova(fithf)

#residuals- look worse?idk
plot(fithf)

#install.packages("lme4")
#install.packages("Matrix")
library(lme4)
fit1 <- lmer(HeightatFlowering ~ Treatment + (1|Block), data=dat)
fit2 <- lmer(HeightatFlowering ~ Treatment + FloweringDate + (1|Block), data = dat)



fit3 <- glmer(MaxCapitula ~ Treatment + (1|Block), data = dat, family = "poisson")
fit4 <- lmer(MaxCapitula ~ Treatment + (1|Block), data = dat)


install.packages("DHARMa")
library(DHARMa)

sim.fit3 <- simulateResiduals(fit3)
plot(sim.fit3)

sim.fit4 <- simulateResiduals(fit4)
plot(sim.fit4)
#is there a diff between residuals 3 and 4? family poisson?
