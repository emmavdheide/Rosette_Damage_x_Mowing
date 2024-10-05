#set working directory
setwd("~/Desktop")

#load data
dat <- read.csv("ManuscriptData1.csv")

#make data into factors
dat$Block <- as.factor(dat$Block)
dat$SubBlock <- as.factor(dat$SubBlock)
dat$Treatment <- as.factor(dat$Treatment)
dat$RosDam <- as.factor(dat$RosDam)
dat$EarlyMow <- as.factor(dat$EarlyMow)
dat$LateMow <- as.factor(dat$LateMow)
dat$SurviveJuly5 <- as.factor(dat$SurviveJuly5)

#library all packages
library(lme4)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(ggplot2)

fit1 <- lmer(HeightatFlowering ~ Treatment + (1|Block), data=dat)
fit2 <- lmer(HeightatFlowering ~ Treatment + FloweringDate + (1|Block), data = dat)

fit3 <- glmer(MaxCapitula ~ Treatment + (1|Block), data = dat, family = "poisson")
fit4 <- lmer(MaxCapitula ~ Treatment + (1|Block), data = dat)

fit5 <- glmmTMB(MaxCapitula ~ Treatment + LLLMay2 + (1|Block), data = dat, family = "poisson")
summary(fit5)

sim.fit5 <- simulateResiduals(fit5)
plot(sim.fit5)

emmMaxCap <- emmeans(fit5, specs=pairwise~Treatment, type="response")
emmMaxCap

MaxCap<-as.data.frame(emmMaxCap[1])

MaxCapPlot<-ggplot(MaxCap,aes(y=emmeans.rate, x=emmeans.Treatment))+
  geom_bar(stat = "identity")+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Capitula Production")+
  ylim(0, 12)+
  ggtitle("Maximum Number of Capitula Present on Plant")+
  geom_errorbar(aes(x=emmeans.Treatment, ymin=emmeans.asymp.LCL, ymax=emmeans.asymp.UCL))
MaxCapPlot

#change order of magnitude on date numbers
dat$SmallDate <- dat$FloweringDate/1000
#issue with magnitude, use transformed version smalldate!

#this is bad
fit6 <- glmmTMB(FloweringDate ~ Treatment + (1|Block), data = dat, family = "nbinom2")
summary(fit6)
sim.fit6 <- simulateResiduals(fit6)
plot(sim.fit6)

#this is better
fit7 <- glmmTMB(SmallDate ~ Treatment + (1|Block), data = dat, family = "gaussian")
summary(fit7)
sim.fit7 <- simulateResiduals(fit7)
plot(sim.fit7)


#height at flowering by treatment
fit8 <- glmmTMB(HeightatFlowering ~ RosDam*EarlyMow*LateMow + (1|Block) + LLLMay2, data = dat, family = "gaussian")
summary(fit8)

sim.fit8 <- simulateResiduals(fit8)
plot(sim.fit8)

emmHtFl <- emmeans(fit8, specs=pairwise~RosDam*EarlyMow*LateMow, type="response")
emmHtFl

fit8B <- glmmTMB(HeightatFlowering ~ Treatment + (1|Block) + LLLMay2, data = dat, family = "gaussian")
summary(fit8B)

emmHtFl2 <- emmeans(fit8B, specs=pairwise~Treatment, type="response")
emmHtFl2

HtFl<-as.data.frame(emmHtFl2[1])

HtFlPlot<-ggplot(HtFl,aes(y=emmeans.emmean, x=emmeans.Treatment))+
  geom_bar(stat = "identity")+
  theme(panel.background = element_blank(), panel.border = element_rect(color = "black",fill=NA, size=1))+
  labs(x="Treatment", y="Height at Flowering")+
  ylim(0, 120)+
  ggtitle("Height at Flowering")+
  geom_errorbar(aes(x=emmeans.Treatment, ymin=emmeans.lower.CL, ymax=emmeans.upper.CL))
HtFlPlot

#survivorship by treatment and LLL
#including LLL gives NaN in summary
fit9 <- glmmTMB(SurviveJuly5 ~ Treatment + (1|Block), data = dat, family = "binomial")
summary(fit9)
sim.fit9 <- simulateResiduals(fit9)
plot(sim.fit9)
#rosette damage and late mow give NaN
fit10 <- glmmTMB(SurviveJuly5 ~ EarlyMow + (1|Block), data = dat, family = "binomial")
summary(fit10)
