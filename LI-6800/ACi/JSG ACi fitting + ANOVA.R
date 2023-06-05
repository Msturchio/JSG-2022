library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects)

library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix) ; library(vioplot)


# Make sure you install packages

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

############################################## 

###################################################################################
###################################################################################

setwd("F:/CSU/JSG 2022/Physiology/LI-6800/ACi")

# Above you will need to edit to your home PC

###################################################################################

df<-read.csv("July 25 ACi_cleaned.csv")
df<-subset(df, KEEP == "y")
df$Plot<-as.factor(df$Plot)

dat<-subset(df, ID == "s2")
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
df$Plot<-(as.factor(df$Plot))


m1<-lm(vcmax~Plot, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))
# 


tiff(file = "JSGACI_2022.tiff", height = 10, width = 6, res = 400, units = "in", compression = "zip+p")
m1<-lm(vcmax~Plot, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))
par(mfrow = c(2,1), omi = c(0.8, 1, 0.4, 1), mar = c(1.5,2,0.5,1))


xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(20,100), xlim=c(0,16))

dum<-subset(d1, Plot == "1")
rect(02, 00, 04, mean(dum$emmean), col = "grey69", border = "grey69")
ablineclip(v=3, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")


dum<-subset(d1, Plot == "2")
rect(04, 00, 06, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "3")
rect(06, 00, 08, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=7, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "4")
rect(08, 00, 10, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Plot == "22")
rect(012, 00, 014, mean(dum$emmean), col = "black", border = "black")

ablineclip(v=13, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")

mtext(side = 2, expression(italic(V)[cmax]~(mu*mol~m^-2~s^-1)), cex = 1.5, padj = -2.5, outer= F)
axis(2, at = seq(20,100,20), las = 2, cex.axis = 1.5, labels = T)
legend("topright", c("Between", "East Edge", "Beneath", "West Edge", "Control"), col= c("grey69", "cornflowerblue", "aquamarine3", "indianred4","black"), pch = 15, cex = 1.2, horiz = F, bty='n')
box()


m1<-lm(jmax~Plot, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(20,180), xlim=c(0,16))

dum<-subset(d1, Plot == "1")
rect(02, 00, 04, mean(dum$emmean), col = "grey69", border = "grey69")
ablineclip(v=3, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")


dum<-subset(d1, Plot == "2")
rect(04, 00, 06, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "3")
rect(06, 00, 08, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=7, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "4")
rect(08, 00, 10, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Plot == "22")
rect(012, 00, 014, mean(dum$emmean), col = "black", border = "black")

ablineclip(v=13, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")

mtext(side = 2, expression(italic(J)[max]~(mu*mol~m^-2~s^-1)), cex = 1.5, padj = -2.5, outer= F)

axis(2, at = seq(20,180,40), las = 2, cex.axis = 1.5, labels = T)

box()
dev.off()


# legend("topleft", c("(a)"), bty = "n", cex = 1.5)

# mtext(side = 2, expression(italic(A)[net]~(mu*mol~m^-2~s^-1)), cex = 1.6, padj = -2, outer= F)

legend("top", c("Between Panels", "Beneath Panels"), density=c(0,30), cex = 2, horiz = F, bty='n')

mtext(side = 2, expression(italic(A)[sat]~(mu*mol~m^-2~s^-1)), cex = 3, padj = -1.3, outer= F)

legend("topleft",col = c("royalblue1") , pch = 15, cex = 3, bty = "n", c(expression(italic(A)[sat])))

legend("topright",col = c("springgreen4") , pch = 15, cex = 3, bty = "n", c(expression(italic(phi)~CO[2])))


# legend("topright",col = c("mediumaquamarine","springgreen4") , pch = 15, cex = 2, bty = "n",
#        c(expression(italic(phi)[psII]~ (6~foot)), expression(italic(phi)[psII]~ (8~foot))))
##############
#############
#############

m1<-lm(phi_j~Light,df)
anova(m1)
# plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Light))


##
par(new=T)
plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0.02,0.12), xlim=c(0,14))

# dum<-subset(d1, Light == "light" )
# rect(08, 00, 09, mean(dum$emmean), col = "mediumaquamarine", border = "mediumaquamarine")
# ablineclip(v=8.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")
# 
# 
# dum<-subset(d1, Light == "shade" )
# rect(09, 00, 10, mean(dum$emmean), col = "mediumaquamarine", border = "mediumaquamarine")
# rect(09, 00, 10, mean(dum$emmean), density = 10, lwd = 2, col = "white" , border = "black")
# ablineclip(v=9.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Light == "light" )
rect(8, 00, 10, mean(dum$emmean), col = "springgreen4", border = "springgreen4")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Light == "shade" )
rect(10, 00, 12, mean(dum$emmean), col = "springgreen4", border = "springgreen4")
rect(10, 00, 12, mean(dum$emmean), density = 10, lwd = 2, col = "white" , border = "black")
ablineclip(v=11, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


box()

axis(4, at = seq(0.02,0.12,0.02), las = 2, cex.axis = 2.2, labels = T)

# legend("topleft", c("(b)"), bty = "n", cex = 1.5)
# mtext(side = 4, expression(italic(phi)[psII]~(mol*CO[2]~mol^-1~quanta)), cex = 2, padj = 2.2, outer= F)
mtext(side = 4, expression(italic(phi)~CO[2]), cex = 2.8, padj = 3.2, outer= F)
mtext(side = 4, expression((mol~CO[2]~~mol^-1~quanta)), cex = 2, padj = 4.6, outer= F)


dev.off()














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
