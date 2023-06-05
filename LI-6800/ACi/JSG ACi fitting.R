library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects)

# Make sure you install packages

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

############################################## 

###################################################################################
###################################################################################

setwd("F:/CSU/JSG 2022/Physiology/LI-6800/ACi")

# Above you will need to edit to your home PC

###################################################################################

df<-read.csv("June 16 ACi_cleaned.csv")
df<-subset(df, KEEP == "y")
df$Plot<-as.factor(df$Plot)

dat<-subset(df, ID == "n1")
plot(dat$A ~ dat$Ci)

dat<-subset(dat, CO2_r >= 410)
dat<-subset(dat, CO2_r <= 430)
plot(dat$A ~ dat$Ci)


dat$gsw
dat$A

plot(dat$A ~ dat$Ci)

object<-fitaci(dat, varnames = list(ALEAF = "A", Tleaf = "Tleaf", Ci = "Ci", PPFD = "Qin"),
               fitmethod = 'default', Tleaf = 25, Tcorrect = T)
object$pars

object

# object$GammaStar
# 
# object$Photosyn(Ca=393.5186)
# # 
# # dat$Photo_out/dat$Cond_out
# 
# dat$GasEx_gsw_mol.m.U.207B.?.s.U.207B.?
#   dat$Photo
# dat$CO2S
# dat$Tleaf
# 
# Kc=exp(38.05-79.43/(1.842471*(32.9591+273.15)));
# 
# Ko=exp(20.30-36.38/(1.842471*(32.9591+273.15)));
# 
# Km=Kc*(1+(210/Ko));
# 
# 
# model Vcmax = (Apred*(CO2S+Km))/(CO2S-Gstar);
# 
# 
# 
# Apred=predicted photosynthesis
# 
# 
# 
# Stomatal limitation (L) = 1- (observed A/predicted A)



df<-read.csv("JSG ACi oneatatime.csv")
df<-subset(df, KEEP == "y")
df$Plot<-(as.factor(df$ï..Plot))
# dum<-subset(df, fert == "Post")
# dum<-subset(df, treatment == "n")
# dum<-subset(df, leafage == "new" & fit.method == "bilinear")

m1<-lm(gs~Plot, df)
anova(m1)
summary(m1)
plot(allEffects(m1))
# 















# 
# df<-read.csv("Backup_One at a time values_WETFERT.csv")
# 
# dum<-subset(df, fert == "Post")
# dum<-subset(df, treatment == "n")
# dum<-subset(df, leafage == "new" & fit.method == "bilinear")
# 
# m1<-lm(vcmax~fert, dum)
# anova(m1)
# summary(m1)
# plot(allEffects(m1))
# 
# m1<-lm(jmax~timepoint, df)
# anova(m1)
# summary(m1)
# plot(allEffects(m1))

dev.off()
