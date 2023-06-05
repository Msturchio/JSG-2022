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
tiff(file = "shadevnoshade Phips2 inset.tiff", height = 8, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(0.5,2.5,1.5,0.5))

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,1), xlim=c(0,18))

sdf<-summaryBy(PhiPS2 ~ Plot+Timepoint+Shade , FUN = c(mean,std.error), na.rm = T, df)
sdf$Timepoint<-as.numeric(sdf$Timepoint)


m1<-lm(PhiPS2~Plot, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))

m1<-lm(PhiPS2~Plot*Shade, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot*Shade))

dum<-subset(d1, Plot == "1" & Shade == "n")
rect(02, 00, 03, mean(dum$emmean), col = "grey69", border = "grey69")
rect(02, 00, 03, mean(dum$emmean), col = "grey69", border = "grey69")

ablineclip(v=02.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
dum<-subset(d1, Plot == "1" & Shade == "y")
rect(03, 00, 04, mean(dum$emmean),density = 10, lwd = 2, col = "grey69" , border = "grey69")
ablineclip(v=03.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
#######
dum<-subset(d1, Plot == "2" & Shade == "n")
rect(05, 00, 06, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=05.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

dum<-subset(d1, Plot == "2" & Shade == "y")
rect(06, 00, 07, mean(dum$emmean), density = 10, lwd = 2, col = "cornflowerblue" , border = "cornflowerblue")
ablineclip(v=06.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
###########
dum<-subset(d1, Plot == "3" & Shade == "n")
rect(08, 00, 9, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=08.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

dum<-subset(d1, Plot == "3" & Shade == "y")
rect(09, 00, 10, mean(dum$emmean), density = 10, lwd = 2, col = "aquamarine3" , border = "aquamarine3")
ablineclip(v=09.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
##############

dum<-subset(d1, Plot == "4" & Shade == "n")
rect(11, 00, 12, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=11.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

dum<-subset(d1, Plot == "4" & Shade == "y")
rect(12, 00, 13, mean(dum$emmean), density = 10, lwd = 2, col = "indianred4" , border = "indianred4")
ablineclip(v=12.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
##############

dum<-subset(d1, Plot == "22" & Shade == "n")
rect(14, 00, 16, mean(dum$emmean), col = "black", border = "black")
ablineclip(v=15, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")

# axis(1, at = seq(1,18,1), las = 1, cex.axis = 1.4, labels = F)
axis(2, at = seq(0,1,0.2), las = 2, cex.axis =2)
box()

mtext(side = 2, expression(italic(phi)[PSII]), cex = 3.5, padj = -1.5, outer= F)
legend("topright", c("Sun", "Shade"), density=c(10000,20), cex = 3, horiz = F, bty='n')

dev.off()


####SUMMARY FIGURE######################################################
tiff(file = "diurnal mean Phips2 inset.tiff", height = 10, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(0.5,2.5,1.5,0.5))

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,1), xlim=c(0,12))

sdf<-summaryBy(PhiPS2 ~ Plot+Timepoint+Shade , FUN = c(mean,std.error), na.rm = T, df)
sdf$Timepoint<-as.numeric(sdf$Timepoint)


m1<-lm(PhiPS2~Plot, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))

# m1<-lm(PhiPS2~Plot*Shade, df)
# anova(m1)
# summary(m1)
# # plot(allEffects(m1))
# d1=cld(emmeans(m1, ~ Plot*Shade))

dum<-subset(d1, Plot == "1" )
rect(02, 00, 03, mean(dum$emmean), col = "grey69", border = "grey69")
rect(02, 00, 03, mean(dum$emmean), col = "grey69", border = "grey69")

ablineclip(v=02.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

#######
dum<-subset(d1, Plot == "2" )
rect(04, 00, 05, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=4.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")


###########
dum<-subset(d1, Plot == "3" )
rect(06, 00, 7, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=06.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")


##############

dum<-subset(d1, Plot == "4" )
rect(8, 00, 9, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=8.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

##############

dum<-subset(d1, Plot == "22" )
rect(10, 00, 11, mean(dum$emmean), col = "black", border = "black")
ablineclip(v=10.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")

# axis(1, at = seq(1,18,1), las = 1, cex.axis = 1.4, labels = F)
axis(2, at = seq(0,1,0.2), las = 2, cex.axis =2)
box()


text(2.5, 0.52, expression('a'),cex=2)
text(4.5, 0.52, expression('a'),cex=2)
text(6.5, 0.62, expression('b'),cex=2)
text(8.5, 0.64, expression('b'),cex=2)
text(10.5, 0.50, expression('a'),cex=2)


mtext(side = 2, expression(italic(phi)[PSII]), cex = 3.5, padj = -1.5, outer= F)
# legend("topright", c("Sun", "Shade"), density=c(10000,20), cex = 3, horiz = F, bty='n')

dev.off()
