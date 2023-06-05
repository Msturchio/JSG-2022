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
df$PhiPS2<-as.numeric(df$PhiPS2)
# df$UniqueID<-as.factor(df$UniqueID)
df$PhiPS2<-as.numeric(df$PhiPS2)


nine<-subset(df, Timepoint == "9")
eleven<-subset(df, Timepoint == "11")
one<-subset(df, Timepoint == "13")
three<-subset(df, Timepoint == "15")
five<-subset(df, Timepoint == "17")

# df<-subset(df, Date.a == "5/17/2022")

####SUMMARY FIGURE######################################################
tiff(file = "PhiPS2 diurnal means.tiff", height = 10, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(1.5, 1.5, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))
sdf<-summaryBy(PhiPS2 ~ Plot+Timepoint+Shade , FUN = c(mean,std.error), na.rm = T, df)
sdf$Timepoint<-as.numeric(sdf$Timepoint)



plotCI(sdf$Timepoint, sdf$PhiPS2.mean, sdf$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "white", xlim = c(1,5), ylim = c(0,1))


par(new=T)
sfour<-subset(sdf, Plot == "22")
plotCI(sfour$Timepoint, sfour$PhiPS2.mean, sfour$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "grey69", xlim = c(1,5),  ylim = c(0,1))
points(sfour$PhiPS2.mean ~ sfour$Timepoint, col = "grey69", lty = 2, lwd = 3, type = "l")
par(new=T)
sfour<-subset(sdf, Plot == "22" & Shade == "y")
plotCI(sfour$Timepoint, sfour$PhiPS2.mean, sfour$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "grey69", xlim = c(1,5),  ylim = c(0,1))


par(new=T)
sone<-subset(sdf, Plot == "1")
plotCI(sone$Timepoint, sone$PhiPS2.mean, sone$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "black", xlim = c(1,5),  ylim = c(0,1))
points(sone$PhiPS2.mean ~ sone$Timepoint, col = "black", lty = 2, lwd = 3, type = "l")
par(new=T)
sone<-subset(sdf, Plot == "1" & Shade == "y")
plotCI(sone$Timepoint, sone$PhiPS2.mean, sone$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "black", xlim = c(1,5),  ylim = c(0,1))


par(new=T)
stwo<-subset(sdf, Plot == "2")
plotCI(stwo$Timepoint, stwo$PhiPS2.mean, stwo$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "cornflowerblue", xlim = c(1,5),  ylim = c(0,1))
points(stwo$PhiPS2.mean ~ stwo$Timepoint, col = "cornflowerblue", lty = 2, lwd = 3, type = "l")
par(new=T)
stwo<-subset(sdf, Plot == "2" & Shade == "y")
plotCI(stwo$Timepoint, stwo$PhiPS2.mean, stwo$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "cornflowerblue", xlim = c(1,5),  ylim = c(0,1))



par(new=T)
sthree<-subset(sdf, Plot == "3")
plotCI(sthree$Timepoint, sthree$PhiPS2.mean, sthree$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "aquamarine3", xlim = c(1,5),  ylim = c(0,1))
points(sthree$PhiPS2.mean ~ sthree$Timepoint, col = "aquamarine3", lty = 2, lwd = 3, type = "l")
par(new=T)
sthree<-subset(sdf, Plot == "3" & Shade == "y")
plotCI(sthree$Timepoint, sthree$PhiPS2.mean, sthree$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "aquamarine3", xlim = c(1,5),  ylim = c(0,1))



par(new=T)
sfour<-subset(sdf, Plot == "4")
plotCI(sfour$Timepoint, sfour$PhiPS2.mean, sfour$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "indianred4", xlim = c(1,5),  ylim = c(0,1))
points(sfour$PhiPS2.mean ~ sfour$Timepoint, col = "indianred4", lty = 2, lwd = 3, type = "l")
par(new=T)
sfour<-subset(sdf, Plot == "4" & Shade == "y")
plotCI(sfour$Timepoint, sfour$PhiPS2.mean, sfour$PhiPS2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "indianred4", xlim = c(1,5),  ylim = c(0,1))





axis(1, at = seq(1,5,1), las = 1, cex.axis = 1.4, labels = F)
axis(2, at = seq(0,1,0.2), las = 2, cex.axis = 2, labels = T)

# mtext(side = 2, expression(italic(g)[sw]~(mol~m^-2~s^-1)), cex = 2, padj = -2.2, outer= F)
# mtext(side = 2, expression(paste(psi~(MPa))), cex = 3, padj = -1.5, outer= F)

mtext(side = 1, expression("9:00          11:00          13:00          15:00         17:00"), cex = 1.5, padj = 1.25, outer= F)
mtext(side = 2, expression(italic(phi)[PSII]), cex = 4.5, padj = -1.5, outer= F)
mtext(side = 1, expression("Time of Day"), cex = 3, padj = 1.4, outer= T)


# legend("top", c("Control", "Plot 1 (Between panels)","Plot 2 (EDE)","Plot 3 (Beneath Panels)", "Plot 4 (WDE)"), col=c("grey69", "black", "cornflowerblue", "aquamarine3", "indianred4"), pch= c(16),lty =c(2) , cex = 1.7, horiz = F, bty='n')
legend("bottom", c("Shade","Sun"), col=c("black"), pch= c(16,1), cex = 2, horiz = F, bty='n')

dev.off()

