library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects)
library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix);
library(emmeans);library(multcompView);library(sjPlot)

setwd("F:/CSU/JSG 2022/Physiology/LI-600")
df<-read.csv("Master_LI600_8.csv")

df$Date<-as.Date(df$Date, ("%m/%d/%Y"))
df$Timepoint<-as.factor(df$Timepoint)
df$Transect<-as.factor(df$Transect)
df$Plot<-as.factor(df$Position)
df$Tleaf<-as.numeric(df$Tleaf)
# df$UniqueID<-as.factor(df$UniqueID)
df$PhiPS2<-as.numeric(df$PhiPS2)


nine<-subset(df, Timepoint == "9")
eleven<-subset(df, Timepoint == "11")
one<-subset(df, Timepoint == "13")
three<-subset(df, Timepoint == "15")
five<-subset(df, Timepoint == "17")

# df<-subset(df, Date.a == "5/17/2022")

# ####SUMMARY FIGURE######################################################
# tiff(file = "Tleaf diurnal means.tiff", height = 10, width = 10, res = 400, units = "in", compression = "zip+p")

##############
############
##############
#############
sdf<-summaryBy(Tleaf + gsw + PhiPS2 ~ Plot + Timepoint +Date, FUN = c(mean,std.error), na.rm = T, df)
# sdf$Timepoint<-as.numeric(sdf$Timepoint)


par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))


plot(Tleaf.mean ~ gsw.mean , sdf, pch = 1, cex= 2,  col= "white",  xlim = c(0,0.3), ylim = c(15,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

points(Tleaf.mean ~ gsw.mean, sdf, pch = 1,cex = 2, col = "grey", xlim = c(0,0.3), ylim = c(15,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

asdf<-subset(sdf, Plot == "1")
points(Tleaf.mean ~ gsw.mean, asdf, pch = 1,cex = 2, col = "black", xlim = c(0,0.3), ylim = c(15,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

bsdf<-subset(sdf, Plot == "2")
points(Tleaf.mean ~ gsw.mean, bsdf, pch = 1,cex = 2, col = "cornflowerblue", xlim = c(0,0.3), ylim = c(15,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

csdf<-subset(sdf, Plot == "3")
points(Tleaf.mean ~ gsw.mean, csdf, pch = 1,cex = 2, col = "aquamarine3", xlim = c(0,0.3), ylim = c(15,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

dsdf<-subset(sdf, Plot == "4")
points(Tleaf.mean ~ gsw.mean, dsdf, pch = 1,cex = 2, col = "indianred4", xlim = c(0,0.3), ylim = c(15,40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")


m1<-lm(Tleaf.mean ~ gsw.mean, sdf)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("gsw.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(sdf$gsw.mean,na.rm = TRUE),x2=max(sdf$gsw.mean ,na.rm = TRUE), lty = 1, lwd=4, col = "grey69")

m1<-lm(Tleaf.mean ~ gsw.mean, asdf)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("gsw.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(asdf$gsw.mean,na.rm = TRUE),x2=max(asdf$gsw.mean ,na.rm = TRUE), lty = 1, lwd=4, col = "black")

m1<-lm(Tleaf.mean ~ gsw.mean, bsdf)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("gsw.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(bsdf$gsw.mean,na.rm = TRUE),x2=max(bsdf$gsw.mean ,na.rm = TRUE), lty = 1, lwd=4, col = "cornflowerblue")

m1<-lm(Tleaf.mean ~ gsw.mean, csdf)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("gsw.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(csdf$gsw.mean,na.rm = TRUE),x2=max(csdf$gsw.mean ,na.rm = TRUE), lty = 1, lwd=4, col = "aquamarine3")

m1<-lm(Tleaf.mean ~ gsw.mean, dsdf)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("gsw.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(dsdf$gsw.mean,na.rm = TRUE),x2=max(dsdf$gsw.mean ,na.rm = TRUE), lty = 1, lwd=4, col = "indianred4")

# m1<-lm(Tleaf.mean ~ PhiPS2.mean, sdf)
# anova(m1)
# summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("PhiPS2.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(sdf$gsw.mean,na.rm = TRUE),x2=max(sdf$gsw.mean ,na.rm = TRUE), lty = 1, lwd=4, col = "grey69")

################################################################################################

par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))


plot(gsw ~ Tleaf , df, pch = 16, cex= 2,  col= "white",  xlim = c(15,40), ylim = c(0,0.4), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

points(gsw ~ Tleaf, df, pch = 16,cex = 2, col = "grey", xlim = c(15,40), ylim = c(0,0.4), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

adf<-subset(df, Plot == "1")
points(gsw ~ Tleaf, adf, pch = 16,cex = 2, col = "black", xlim = c(15,40), ylim = c(0,0.4), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

bdf<-subset(df, Plot == "2")
points(gsw ~ Tleaf, bdf, pch = 16,cex = 2, col = "cornflowerblue", xlim = c(15,40), ylim = c(0,0.4), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

bdf<-subset(df, Plot == "3")
points(gsw ~ Tleaf, bdf, pch = 16,cex = 2, col = "aquamarine3", xlim = c(15,40), ylim = c(0,0.4), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

bdf<-subset(df, Plot == "4")
points(gsw ~ Tleaf, bdf, pch = 16,cex = 2, col = "indianred4", xlim = c(15,40), ylim = c(0,0.4), xaxt = "n", yaxt = "n", xlab = "", ylab = "")



axis(1, at = seq(0,0.4,0.05), las = 1, cex.axis = 1.4, labels = T)
axis(2, at = seq(15,40,5), las = 2, cex.axis = 1.4, labels = T)

# mtext(side = 2, expression(italic(g)[sw]~(mol~m^-2~s^-1)), cex = 2, padj = -2.2, outer= F)
# mtext(side = 2, expression(paste(psi~(MPa))), cex = 3, padj = -1.5, outer= F)

# mtext(side = 1, expression("9am          11am         1pm           3pm           5pm"), cex = 2.2, padj = 1.25, outer= F)
mtext(side = 1, expression(italic(T)[leaf]~(degree*C)), cex = 3.5, padj = -1.5, outer= F)
mtext(side = 1, expression("Time of Day"), cex = 3, padj = 2, outer= T)


legend("bottomright", c("Control", "Plot 1 (Between panels)","Plot 2 (EDE)","Plot 3 (Beneath Panels)", "Plot 4 (WDE)"), col=c("grey69", "black", "cornflowerblue", "aquamarine3", "indianred4"), pch= c(16),lty =c(2) , cex = 1.7, horiz = F, bty='n')
legend("bottomleft", c("Shade","Light"), col=c("grey69"), pch= c(16,1), cex = 2, horiz = F, bty='n')


m1<-lm(gsw ~ Tleaf * Plot, df)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("gsw"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(df$gsw,na.rm = TRUE),x2=max(df$gsw ,na.rm = TRUE), lty = 1, lwd=4, col = "grey69")
