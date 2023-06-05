library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix) ; library(vioplot)

setwd("F:/CSU/JSG 2022/Physiology/LI-6800")


df<-read.csv("Leaf mass and area.csv")
# df<-subset(df, KEEP == "Y")
df$Plot<-as.factor(df$Plot)
df$Date<-as.Date(df$ï..Date, ("%m/%d/%Y"))
df$Date.a<-as.factor(df$Date)

m1<-lm(LMA~Plot*Date.a,df)
anova(m1)
plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Date.a))
