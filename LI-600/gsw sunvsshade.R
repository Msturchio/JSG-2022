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
tiff(file = "shadevnoshade gs inset.tiff", height = 8, width = 10, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(0.5,2.5,1.5,0.5))

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,0.18), xlim=c(2,17))

sdf<-summaryBy(gsw ~ Plot+Timepoint+Shade , FUN = c(mean,std.error), na.rm = T, df)
sdf$Timepoint<-as.numeric(sdf$Timepoint)


m1<-lm(gsw~Plot, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))

m1<-lm(gsw~Plot*Shade, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot*Shade))

dum<-subset(d1, Plot == "22" & Shade == "n")
rect(02, 00, 03, mean(dum$emmean), col = "black", border = "black")
ablineclip(v=02.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")
#######
dum<-subset(d1, Plot == "4" & Shade == "y")
rect(05, 00, 06, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=05.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

dum<-subset(d1, Plot == "4" & Shade == "n")
rect(06, 00, 07, mean(dum$emmean), density = 0, lwd = 2, col = "indianred4" , border = "indianred4")
ablineclip(v=06.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
###########
dum<-subset(d1, Plot == "3" & Shade == "y")
rect(08, 00, 9, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=08.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

dum<-subset(d1, Plot == "3" & Shade == "n")
rect(09, 00, 10, mean(dum$emmean), density = 0, lwd = 2, col = "aquamarine3" , border = "aquamarine3")
ablineclip(v=09.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
##############

dum<-subset(d1, Plot == "2" & Shade == "y")
rect(11, 00, 12, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=11.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

dum<-subset(d1, Plot == "2" & Shade == "n")
rect(12, 00, 13, mean(dum$emmean), density = 0, lwd = 2, col = "cornflowerblue" , border = "cornflowerblue")
ablineclip(v=12.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
##############


dum<-subset(d1, Plot == "1" & Shade == "y")
rect(14, 00, 15, mean(dum$emmean), col = "grey69", border = "grey69")
rect(15, 00, 15, mean(dum$emmean), col = "grey69", border = "grey69")

ablineclip(v=14.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
dum<-subset(d1, Plot == "1" & Shade == "n")
rect(15, 00, 16, mean(dum$emmean),density = 0, lwd = 2, col = "grey69" , border = "grey69")
ablineclip(v=15.5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
# axis(1, at = seq(1,18,1), las = 1, cex.axis = 1.4, labels = F)
# axis(1, at = seq(1,18,1), las = 1, cex.axis = 1.4, labels = F)
axis(2, at = seq(0,0.18,0.06), las = 2, cex.axis =2, labels = T)
box()

# mtext(side = 1, expression("Between               E edge                  Beneath                W edge              Control"), cex = 1.1, padj = -1.1, outer= F)

legend("topleft", c("Sun", "Shade"), density=c(0,100000), cex = 3, horiz = F, bty='n')

dev.off()

#
#########
############
##########
tiff(file = "mean gs inset.tiff", height = 10, width = 8, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(1,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(0.5,2.5,1.5,0.5))

xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,0.20), xlim=c(1,16))

sdf<-summaryBy(gsw ~ Plot+Timepoint+Shade , FUN = c(mean,std.error), na.rm = T, df)
sdf$Timepoint<-as.numeric(sdf$Timepoint)


m1<-lm(gsw~Plot, df)
anova(m1)
summary(m1)
# plot(allEffects(m1))
d1=cld(emmeans(m1, ~ Plot))

# m1<-lm(gsw~Plot*Shade, df)
# anova(m1)
# summary(m1)
# # plot(allEffects(m1))
# d1=cld(emmeans(m1, ~ Plot*Shade))

dum<-subset(d1, Plot == "22" )
rect(01, 00, 03, mean(dum$emmean), col = "black", border = "black")
ablineclip(v=2, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")



#######
dum<-subset(d1, Plot == "4")
rect(04, 00, 06, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=05, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")


###########
dum<-subset(d1, Plot == "3" )
rect(07, 00, 09, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=08, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

##############

dum<-subset(d1, Plot == "2" )
rect(10, 00, 12, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=11, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")


##############
dum<-subset(d1, Plot == "1")
rect(13, 00, 15, mean(dum$emmean), col = "grey69", border = "grey69")
rect(13, 00, 15, mean(dum$emmean), col = "grey69", border = "grey69")

ablineclip(v=14, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")

# axis(1, at = seq(1,18,1), las = 1, cex.axis = 1.4, labels = F)
axis(2, at = seq(0,0.20,0.05), las = 2, cex.axis =2, labels = T)
box()

# mtext(side = 1, expression("Between        E edge       Beneath         W edge        Control"), cex = 1.3, padj = -1.1, outer= F)
# mtext(side = 1, expression(Plot), cex = 2, padj = 1.5, outer= F)
mtext(side = 2, expression(Growing~Season~Mean~(italic(g)[sw])), cex = 2, padj = -2.2, outer= F)
# legend("topleft", c("Sun", "Shade"), density=c(10000,20), cex = 3, horiz = F, bty='n')
text(14, 0.115, expression('b'),cex=2)
text(11, 0.135, expression('c'),cex=2)
text(8, 0.105, expression('b'),cex=2)
text(5, 0.115, expression('b'),cex=2)
text(2, 0.065, expression('a'),cex=2)


dev.off()


