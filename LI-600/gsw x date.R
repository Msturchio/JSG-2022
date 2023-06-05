library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects)
library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix);
library(emmeans);library(multcompView);library(sjPlot)

setwd("F:/CSU/JSG 2022/Physiology/LI-600")
df<-read.csv("Master_LI600_2.csv")

df$Date<-as.Date(df$Date, ("%m/%d/%Y"))
df$Timepoint<-as.factor(df$Timepoint)
df$Transect<-as.factor(df$Transect)
df$Plot<-as.factor(df$Position)
df$gsw<-as.numeric(df$gsw)
# df$UniqueID<-as.factor(df$UniqueID)
df$PhiPS2<-as.numeric(df$PhiPS2)


nine<-subset(df, Timepoint == "9")
eleven<-subset(df, Timepoint == "11")
one<-subset(df, Timepoint == "13")
three<-subset(df, Timepoint == "15")
five<-subset(df, Timepoint == "17")

df$Date.a<-as.Date(df$Date.a, ("%m/%d/%Y"))


####SUMMARY FIGURE######################################################

sdf<-summaryBy(gsw ~ Date.a+Plot , FUN = c(mean,std.error), na.rm = T, df)


tiff(file = "gsw x date.tiff", height = 8, width = 8, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.75, 0.3, 0.3, 0.1), mar = c(0.6,2.75,1,0.25))

###############################

############################### 
# 
plotCI(sdf$Date.a, sdf$gsw.mean, sdf$gsw.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0,0.5))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf,Plot == "1")
plotCI(dum$Date.a, dum$gsw.mean, dum$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 2, col = "black", ylim = c(0,0.3))
points(dum$gsw.mean ~ dum$Date.a, col = "black", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf,Plot == "2")
plotCI(dum$Date.a, dum$gsw.mean, dum$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 2, col = "dodgerblue2", ylim = c(0,0.3))
points(dum$gsw.mean ~ dum$Date.a, col = "dodgerblue2", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf,Plot == "3")
plotCI(dum$Date.a, dum$gsw.mean, dum$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 2, col = "green", ylim = c(0,0.3))
points(dum$gsw.mean ~ dum$Date.a, col = "green", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf,Plot == "4")
plotCI(dum$Date.a, dum$gsw.mean, dum$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 2, col = "firebrick", ylim = c(0,0.3))
points(dum$gsw.mean ~ dum$Date.a, col = "firebrick", lty = 1, type = "l")

legend("topleft", c("Plot 1 (Between panels)","Plot 2 (EDE)","Plot 3 (Beneath Panels)", "Plot 4 (WDE)"), col=c("black", "dodgerblue2", "green", "firebrick"), pch= c(16),lty =c(2) , cex = 1.5, horiz = F, bty='n')

axis(2, at = seq(2,10,2),las = 2, cex.axis = 1.1)
axis.Date(1, sdf$Date.a, at = seq(min(sdf$Date.a), max(sdf$Date.a), "month"), las=2, labels = T) # Special axis command for dates

mtext(side = 1, expression("Date"), cex = 2, padj = 1.4, outer= T)
mtext(side = 2, expression(italic(g)[sw]~(mol~m^-2~s^-1)), cex = 2, padj = -2.2, outer= F)


dev.off()
