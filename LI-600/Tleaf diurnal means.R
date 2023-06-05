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

####SUMMARY FIGURE######################################################
tiff(file = "Tleaf diurnal means.tiff", height = 10, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(1.5, 1.5, 0.4, 0.1), mar = c(1.5,3.5,0.5,0.5))
sdf<-summaryBy(Tleaf ~ Plot+Timepoint+Shade , FUN = c(mean,std.error), na.rm = T, df)
sdf$Timepoint<-as.numeric(sdf$Timepoint)
bigav<-summaryBy(Tleaf ~ Plot+Shade , FUN = c(mean,std.error), na.rm = T, df)
# write.csv(bigav, "Tleaf within array vs. control.csv")

plotCI(sdf$Timepoint, sdf$Tleaf.mean, sdf$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "white", xlim = c(1,5), ylim = c(20,35))


par(new=T)
sfour<-subset(sdf, Plot == "22")
plotCI(sfour$Timepoint, sfour$Tleaf.mean, sfour$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "black", xlim = c(1,5),  ylim = c(20,35))
points(sfour$Tleaf.mean ~ sfour$Timepoint, col = "black", lty = 2, lwd = 3, type = "l")



par(new=T)
sone<-subset(sdf, Plot == "1")
plotCI(sone$Timepoint, sone$Tleaf.mean, sone$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "grey69", xlim = c(1,5),  ylim = c(20,35))
points(sone$Tleaf.mean ~ sone$Timepoint, col = "grey69", lty = 1, lwd = 3, type = "l")
par(new=T)
sone<-subset(sdf, Plot == "1" & Shade == "y")
plotCI(sone$Timepoint, sone$Tleaf.mean, sone$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "grey69", xlim = c(1,5),  ylim = c(20,35))


par(new=T)
stwo<-subset(sdf, Plot == "2")
plotCI(stwo$Timepoint, stwo$Tleaf.mean, stwo$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "cornflowerblue", xlim = c(1,5),  ylim = c(20,35))
points(stwo$Tleaf.mean ~ stwo$Timepoint, col = "cornflowerblue", lty = 1, lwd = 3, type = "l")
par(new=T)
stwo<-subset(sdf, Plot == "2" & Shade == "y")
plotCI(stwo$Timepoint, stwo$Tleaf.mean, stwo$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "cornflowerblue", xlim = c(1,5),  ylim = c(20,35))



par(new=T)
sthree<-subset(sdf, Plot == "3")
plotCI(sthree$Timepoint, sthree$Tleaf.mean, sthree$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "aquamarine3", xlim = c(1,5),  ylim = c(20,35))
points(sthree$Tleaf.mean ~ sthree$Timepoint, col = "aquamarine3", lty = 1, lwd = 3, type = "l")
par(new=T)
sthree<-subset(sdf, Plot == "3" & Shade == "y")
plotCI(sthree$Timepoint, sthree$Tleaf.mean, sthree$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "aquamarine3", xlim = c(1,5),  ylim = c(20,35))



par(new=T)
sfour<-subset(sdf, Plot == "4")
plotCI(sfour$Timepoint, sfour$Tleaf.mean, sfour$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, cex = 3.0, col = "indianred4", xlim = c(1,5),  ylim = c(20,35))
points(sfour$Tleaf.mean ~ sfour$Timepoint, col = "indianred4", lty = 1, lwd = 3, type = "l")
par(new=T)
sfour<-subset(sdf, Plot == "4" & Shade == "y")
plotCI(sfour$Timepoint, sfour$Tleaf.mean, sfour$Tleaf.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=16, cex = 3.0, col = "indianred4", xlim = c(1,5),  ylim = c(20,35))





axis(1, at = seq(1,5,1), las = 1, cex.axis = 1.4, labels = F)
axis(2, at = seq(20,35,5), las = 2, cex.axis = 2, labels = T)

# mtext(side = 2, expression(italic(g)[sw]~(mol~m^-2~s^-1)), cex = 2, padj = -2.2, outer= F)
# mtext(side = 2, expression(paste(psi~(MPa))), cex = 3, padj = -1.5, outer= F)

mtext(side = 1, expression("9am          11am         1pm           3pm           5pm"), cex = 2.2, padj = 1.25, outer= F)
# mtext(side = 2, expression(italic(T)[leaf]~(degree*C)), cex = 3.5, padj = -1.5, outer= F)
mtext(side = 2, expression(Leaf~Temperature~("°C")), cex = 3, padj = -1.6, outer= F)
mtext(side = 1, expression("Time of Day"), cex = 3, padj = 2, outer= T)


legend("bottomright", c(expression(W [edge]),"Beneath" , expression(E [edge]), "Between", "Control" ), col=c("indianred4", "aquamarine3", "cornflowerblue", "grey69", "black"), bty = "n", pch = 16,cex = 2)
legend("bottomleft", c("Shade","Light"), col=c("grey69"), pch= c(16,1), cex = 2, horiz = F, bty='n')

dev.off()

###########
###########
#############
############


df<-read.csv("Tleaf differences.csv")
df$Plot<-as.factor(df$Plot)


tiff(file = "Tleaf within PV and control.tiff", height = 6, width = 6, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,2.5,0.5,0.5))



xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(20,35), xlim=c(0,5))



dum<-subset(df, Plot == "within")
rect(01, 00, 02, mean(dum$Tleaf), col = "lightslateblue", border = "lightslateblue")
ablineclip(v=1.5, y1=as.numeric(mean(dum$Tleaf) + (dum$Tleaf.std.error)), y2=as.numeric(mean(dum$Tleaf) - (dum$Tleaf.std.error)),lwd = 4, col = "grey69")


dum<-subset(df, Plot == "control")
rect(03, 00, 04, mean(dum$Tleaf), col = "black", border = "black")
ablineclip(v=3.5, y1=as.numeric(mean(dum$Tleaf) + (dum$Tleaf.std.error)), y2=as.numeric(mean(dum$Tleaf) - (dum$Tleaf.std.error)),lwd = 4, col = "grey69")


text(1.5, 30, expression('***'),cex=2)
axis(2, at = seq(0,35,5), las = 2, cex.axis = 1.5, labels = T)

legend("topleft", c("PV array", "Control" ), col=c("lightslateblue", "black"), bty = "n", pch = 15,cex = 2)
box()

# legend("bottomleft", c("(a)"), bty = "n", cex = 1.5)
# axis(1, cex.axis = 1.4, labels = F)
mtext(side = 2, expression(Leaf~Temperature~("°C")), cex = 2, padj = -1.25, outer= F)

dev.off()
