library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix); library(emmeans); library(car); library(lubridate); library(lme4);
library(MuMIn);library(car); library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest); library(doBy); library(plotrix) ; library(vioplot)

setwd("F:/CSU/JSG 2022/Physiology/LI-6800/LRC")


df<-read.csv("JSG22LRC_oneatatime.csv")
df<-subset(df, KEEP == "Y")
df$Plot<-as.factor(df$Plot)

# m1<-lm(theta_j~Plot,df)
# anova(m1)
# plot(allEffects(m1))
# summary(m1)
# d1=cld(emmeans(m1, ~ Plot))
# 
# m1<-lm(phi_J~Plot,df)
# anova(m1)
# plot(allEffects(m1))
# summary(m1)
# d1=cld(emmeans(m1, ~ Plot))
# 
# m1<-lm(Rd~Plot,df)
# anova(m1)
# plot(allEffects(m1))
# summary(m1)
# d1=cld(emmeans(m1, ~ Plot))
# 
# m1<-lm(LCP~Plot,df)
# anova(m1)
# plot(allEffects(m1))
# summary(m1)
# d1=cld(emmeans(m1, ~ Plot))
# 
# 
# 
# 
# m1<-lm(A_sat~Plot,df)
# anova(m1)
# plot(allEffects(m1))
# summary(m1)
# d1=cld(emmeans(m1, ~ Plot))
# 
# 
# c<-subset(df, Plot == "22")

tiff(file = "JSG 2022 LResultsC.tiff", height = 12, width = 4, res = 400, units = "in", compression = "zip+p")
par(mfrow = c(3,1), omi = c(0.8, 1, 0.4, 0.1), mar = c(1.5,2.5,0.5,0.5))


m1<-lm(A_sat~Plot,df)
anova(m1)
# plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Plot))


xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,25), xlim=c(0,13))



dum<-subset(d1, Plot == "22")
rect(01, 00, 03, mean(dum$emmean), col = "black", border = "black")
ablineclip(v=2, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")


dum<-subset(d1, Plot == "4")
rect(04, 00, 06, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "3")
rect(06, 00, 08, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=7, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "2")
rect(08, 00, 10, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Plot == "1")
rect(010, 00, 012, mean(dum$emmean), col = "grey69", border = "grey69")
ablineclip(v=11, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
text(2, 12, expression('*'),cex=2)
axis(2, at = seq(0,25,5), las = 2, cex.axis = 1.5, labels = T)

legend("topleft", c(expression(W [edge]),"Beneath" , expression(E [edge]), "Between", "Control" ), col=c("indianred4", "aquamarine3", "cornflowerblue", "grey69", "black"), bty = "n", pch = 15,cex = 1.5)
box()

# legend("bottomleft", c("(a)"), bty = "n", cex = 1.5)
# axis(1, cex.axis = 1.4, labels = F)
mtext(side = 2, expression(italic(A)[sat]~(mu*mol~m^-2~s^-1)), cex = 1.6, padj = -2, outer= F)

################################################################################################################


m1<-lm(phi_J~Plot,df)
anova(m1)
# plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Plot))


xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,0.08), xlim=c(0,13))


dum<-subset(d1, Plot == "22")
rect(01, 00, 03, mean(dum$emmean), col = "black", border = "black")
ablineclip(v=2, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")


dum<-subset(d1, Plot == "4")
rect(04, 00, 06, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "3")
rect(06, 00, 08, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=7, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "2")
rect(08, 00, 10, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Plot == "1")
rect(010, 00, 012, mean(dum$emmean), col = "grey69", border = "grey69")
ablineclip(v=11, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
text(2, 0.042, expression('*'),cex=2)
axis(2, at = seq(0,0.08,0.02), las = 2, cex.axis = 1.5, labels = T)

mtext(side = 2, expression(italic(phi)~CO[2]), cex = 1.5, padj = -4, outer= F)
mtext(side = 2, expression((mol~CO[2]~~mol^-1~quanta)), cex = 1.5, padj = -1.6, outer= F)
legend("bottomleft", c("(b)"), bty = "n", cex = 1.5)


#########################################################################################################################
m1<-lm(LCP~Plot,df)
anova(m1)
# plot(allEffects(m1))
summary(m1)
d1=cld(emmeans(m1, ~ Plot))


xx<-c(-500,500); yy<-c(-500,500)


plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,50), xlim=c(0,13))


dum<-subset(d1, Plot == "22")
rect(01, 00, 03, mean(dum$emmean), col = "black", border = "black")
ablineclip(v=2, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "grey69")


dum<-subset(d1, Plot == "4")
rect(04, 00, 06, mean(dum$emmean), col = "indianred4", border = "indianred4")
ablineclip(v=5, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "3")
rect(06, 00, 08, mean(dum$emmean), col = "aquamarine3", border = "aquamarine3")
ablineclip(v=7, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")

dum<-subset(d1, Plot == "2")
rect(08, 00, 10, mean(dum$emmean), col = "cornflowerblue", border = "cornflowerblue")
ablineclip(v=9, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "gray2")


dum<-subset(d1, Plot == "1")
rect(010, 00, 012, mean(dum$emmean), col = "grey69", border = "grey69")
ablineclip(v=11, y1=as.numeric(mean(dum$emmean) + (dum$SE)), y2=as.numeric(mean(dum$emmean) - (dum$SE)),lwd = 2, col = "black")
text(2, 0.042, expression('*'),cex=2)


axis(2, at = seq(0,50,10), las = 2, cex.axis = 1.5, labels = T)
mtext(side = 2, expression(LCP), cex = 1.8, padj = -2.2, outer= F)
legend("bottomleft", c("(c)"), bty = "n", cex = 1.5)
mtext(side = 1, expression(Plot), cex = 2, padj = 1, outer= T)
dev.off()



