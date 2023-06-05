library(dplyr); library(readxl); library(lubridate)
library(minpack.lm); library(lattice)
library(plantecophys); library(effects)
library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix);
library(emmeans);library(multcompView);library(sjPlot)

setwd("E:/CSU/JSG 2022/Physiology/LI-600")
df<-read.csv("Master_LI600.csv")

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

####SUMMARY FIGURE######################################################

sdf<-summaryBy(gsw ~ Plot+Timepoint+Shade , FUN = c(mean,std.error), na.rm = T, df)
sdf$Timepoint<-as.numeric(sdf$Timepoint)
sone<-subset(sdf, Plot == "1")
stwo<-subset(sdf, Plot == "2")
sthree<-subset(sdf, Plot == "3")
sfour<-subset(sdf, Plot == "4")


tiff(file = "conductance plots 9am to 5pm.tiff", height = 8, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))

# plot(Water.potential.mean ~ Timepoint, sdf, pch = 2, col= "lightskyblue1",  xlim = c(0,5), ylim = c(10, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")


plotCI(sdf$Timepoint, sdf$gsw.mean, sdf$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "white", xlim = c(1,5), ylim = c(0,0.25))

par(new=T)
sone<-subset(sdf, Plot == "1")
plotCI(sone$Timepoint, sone$gsw.mean, sone$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 1.7, col = "black", xlim = c(1,5),  ylim = c(0,0.25))
points(sone$gsw.mean ~ sone$Timepoint, col = "black", lty = 2, lwd = 2, type = "l")
par(new=T)
sone<-subset(sdf, Plot == "1" & Shade == "y")
plotCI(sone$Timepoint, sone$gsw.mean, sone$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "black", xlim = c(1,5),  ylim = c(0,0.25))


par(new=T)
stwo<-subset(sdf, Plot == "2")
plotCI(stwo$Timepoint, stwo$gsw.mean, stwo$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 1.7, col = "dodgerblue2", xlim = c(1,5),  ylim = c(0,0.25))
points(stwo$gsw.mean ~ stwo$Timepoint, col = "dodgerblue2", lty = 2, lwd = 2, type = "l")
par(new=T)
stwo<-subset(sdf, Plot == "2" & Shade == "y")
plotCI(stwo$Timepoint, stwo$gsw.mean, stwo$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "dodgerblue2", xlim = c(1,5),  ylim = c(0,0.25))



par(new=T)
sthree<-subset(sdf, Plot == "3")
plotCI(sthree$Timepoint, sthree$gsw.mean, sthree$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 1.7, col = "green", xlim = c(1,5),  ylim = c(0,0.25))
points(sthree$gsw.mean ~ sthree$Timepoint, col = "green", lty = 2, lwd = 2, type = "l")
par(new=T)
sthree<-subset(sdf, Plot == "3" & Shade == "y")
plotCI(sthree$Timepoint, sthree$gsw.mean, sthree$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "green", xlim = c(1,5),  ylim = c(0,0.25))



par(new=T)
sfour<-subset(sdf, Plot == "4")
plotCI(sfour$Timepoint, sfour$gsw.mean, sfour$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 1.7, col = "firebrick", xlim = c(1,5),  ylim = c(0,0.25))
points(sfour$gsw.mean ~ sfour$Timepoint, col = "firebrick", lty = 2, lwd = 2, type = "l")
par(new=T)
sfour<-subset(sdf, Plot == "4" & Shade == "y")
plotCI(sfour$Timepoint, sfour$gsw.mean, sfour$gsw.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 1.7, col = "firebrick", xlim = c(1,5),  ylim = c(0,0.25))




axis(1, at = seq(1,5,1), las = 1, cex.axis = 1.4, labels = F)
axis(2, at = seq(0,0.25,0.05), las = 2, cex.axis = 1.5)

mtext(side = 2, expression(italic(g)[sw]~(mol~m^-2~s^-1)), cex = 2, padj = -2.2, outer= F)
# mtext(side = 2, expression(paste(psi~(MPa))), cex = 3, padj = -1.5, outer= F)
mtext(side = 1, expression("Time of Day"), cex = 2, padj = 3, outer= F)
mtext(side = 1, expression("9am           11am          1pm           3pm           5pm"), cex = 1.7, padj = 1.25, outer= F)

legend("topleft", c("Plot 1 (Between panels)","Plot 2 (EDE)","Plot 3 (Beneath Panels)", "Plot 4 (WDE)"), col=c("black", "dodgerblue2", "green", "firebrick"), pch= c(16),lty =c(2) , cex = 1.6, horiz = F, bty='n')
legend("topright", c("Shade","Light"), col=c("grey69"), pch= c(16,1), cex = 1.6, horiz = F, bty='n')
dev.off()
# 
# write.csv(sdf, "conductance.csv")
