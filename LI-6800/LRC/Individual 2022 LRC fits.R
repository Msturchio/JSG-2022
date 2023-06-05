library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix);library(minpack.lm); library(mosaic); library(photosynthesis);library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects)

library(photosynthesis)


setwd("F:/CSU/JSG 2022/Physiology/LI-6800/LRC")


jsg<-read.csv("July 13 LRC_cleaned.csv")


dum<-subset(jsg, ID  == "control" & KEEP == "y")

plot(dum$A~dum$Qin)


fit <- fit_aq_response(dum,
                       varnames = list(A_net = "A",
                                       PPFD = "Qin",
                                       Q_cut = 400))
fit


summary(fit[[1]])


#?? efficiency 





############
##############
#############
############
###########





dum<-subset(jsg, Light == "light" & Foot == "6" & KEEP == "Y")

plot(dum$Photo~dum$PARi)


fit <- fit_aq_response(dum,
                       varnames = list(A_net = "Photo",
                                       PPFD = "PARi",
                                       Q_cut = 250))
fit
#################

dum<-subset(jsg, Light == "light" & Foot == "8" & KEEP == "Y")

plot(dum$Photo~dum$PARi)


fit <- fit_aq_response(dum,
                       varnames = list(A_net = "Photo",
                                       PPFD = "PARi",
                                       Q_cut = 250))

fit
################

dum<-subset(jsg, Light == "shade" & Foot == "6" & KEEP == "Y")

plot(dum$Photo~dum$PARi)


fit <- fit_aq_response(dum,
                       varnames = list(A_net = "Photo",
                                       PPFD = "PARi",
                                       Q_cut = 250))
fit
##############

dum<-subset(jsg, Light == "shade" & Foot == "8" & KEEP == "Y")

plot(dum$Photo~dum$PARi)


fit <- fit_aq_response(dum,
                       varnames = list(A_net = "Photo",
                                       PPFD = "PARi",
                                       Q_cut = 250))

fit
###########