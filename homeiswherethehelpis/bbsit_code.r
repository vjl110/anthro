### Pull up relevant data
setwd("C:/Users/Layne/Dropbox/D1_ALLORES")
rm(list=ls())
load('kin.lst')
info <- as.data.frame(kin.lst["info"])
colnames(info) <- c("focal","male","age","vil","lat","lon","hh_ID","cow_1","cow_6","goat_1","goat_20","ego_ID","mom.dead","dad.dead","mgm.dead","mgf.dead","pgm.dead","pgf.dead","year", "kids")


###
#####  Find and set relevant additional variables
###
labkid.g <- paste(c("kidg"), c(0:15), sep="")
labkid.b <- paste(c("kidb"), c(0:15), sep="")
kidset.g <- as.data.frame(matrix(nrow = nrow(info), ncol=length(labkid.g)))
kidset.b <- as.data.frame(matrix(nrow = nrow(info), ncol=length(labkid.b)))
colnames(kidset.g) <- labkid.g
colnames(kidset.b) <- labkid.b

for(i in 1:nrow(info)){   #  Figuring out the number of infants and older kids
	hld.g <- kin.lst[[6]][i,c(1:19)][!is.na(kin.lst[[6]][i,c(1:19)]) & !is.na(kin.lst[[5]][i,c(1:19)]) & kin.lst[[5]][i,c(1:19)] == 0]
	hld.b <- kin.lst[[6]][i,c(1:19)][!is.na(kin.lst[[6]][i,c(1:19)]) & !is.na(kin.lst[[5]][i,c(1:19)]) & kin.lst[[5]][i,c(1:19)] == 1]
	for(j in 1:length(labkid.g)){
		kidset.g[i,j] <- sum(ifelse(hld.g == j - 1, 1, 0))
		kidset.b[i,j] <- sum(ifelse(hld.b == j - 1, 1, 0))
	}
	if(!is.na(info$hh_ID[i]) & !is.na(kin.lst[[3]][i,c(275)])){
		info$crmom[i] <- ifelse(kin.lst[[3]][i,c(275)] == info$hh_ID[i], 1, 0) # kin.lst[[3]] is household
	}else{
		info$crmom[i] <- NA
	}
}

foc.tot <- c()	# identify folks who are primary relatives of focal interviews to cut out the crap
for(i in 1:nrow(kin.lst[[6]])){
	foc.tot[i] <- ifelse(info$focal[i] == 1 | sum(as.numeric(kin.lst[[8]][i,c(1:57, 275:283)]), na.rm=T) > 0, 1, 0)
}

info <- cbind(info, kidset.g)
info <- cbind(info, kidset.b)
for(i in 1:nrow(info)){
	if(is.na(kin.lst[[2]]$sps_ID[i])){
		info$married[i] <- NA
	}else if(kin.lst[[2]]$sps_ID[i] == "unmarried"){
		info$married[i] <- 0
	}else{
		info$married[i] <- 1
	}
}

####  Truncate to relevant dataset and add key variables

tst <- subset(info, mom.dead == 0 & male == 0 & foc.tot == 1 & kids > 0 & as.numeric(age) <= 45 & year >= 2000 & !is.na(married))

#tst$winf <- ifelse(tst$kidg0 + tst$kidg1 + tst$kidg2 + tst$kidb0 + tst$kidb1 + tst$kidb2 > 0, 1, 0) # binary var... has infant?
tst$ninf <- tst$kidg0 + tst$kidg1 + tst$kidg2 + tst$kidb0 + tst$kidb1 + tst$kidb2
tst$ninf[tst$ninf > 2] <- 2

tst$bbsit <- tst$kidg6*(6/15) + tst$kidg7*(7/15) + tst$kidg8*(8/15) + tst$kidg9*(9/15) + tst$kidg10*(10/15) + tst$kidg11*(11/15) + tst$kidg12*(12/15) + tst$kidg13*(13/15) + tst$kidg14*(14/15) + tst$kidg15*(15/15) # 

tst$boys <- tst$kidb7 + tst$kidb8 + tst$kidb9 + tst$kidb10 + tst$kidb11 + tst$kidb12 + tst$kidb13 + tst$kidb14 + tst$kidb15 # 


###
####  MODELS
##

library(plotrix)
library(lme4)
library(AICcmodavg)
library(car)

bsit.n <- lmer(crmom ~  (1|ego_ID), data=tst, family="binomial")
bsit.1 <- lmer(crmom ~  ninf + (1|ego_ID), data=tst, family="binomial")
	lower <- coef(summary(bsit.1))[,1] + qnorm(.025)*coef(summary(bsit.1))[,2]
	upper <- coef(summary(bsit.1))[,1] + qnorm(.975)*coef(summary(bsit.1))[,2]
	exp(cbind(coef(summary(bsit.1)), lower, upper))
bsit.2 <- lmer(crmom ~  ninf + bbsit + (1|ego_ID), data=tst, family="binomial")
	lower <- coef(summary(bsit.2))[,1] + qnorm(.025)*coef(summary(bsit.2))[,2]
	upper <- coef(summary(bsit.2))[,1] + qnorm(.975)*coef(summary(bsit.2))[,2]
	exp(cbind(coef(summary(bsit.2)), lower, upper))
bsit.3 <- lmer(crmom ~  ninf + bbsit + married + (1|ego_ID), data=tst, family="binomial")
	lower <- coef(summary(bsit.3))[,1] + qnorm(.025)*coef(summary(bsit.3))[,2]
	upper <- coef(summary(bsit.3))[,1] + qnorm(.975)*coef(summary(bsit.3))[,2]
	exp(cbind(coef(summary(bsit.3)), lower, upper))
bsit.4 <- lmer(crmom ~  ninf + bbsit + married + as.numeric(age) + (1|ego_ID), data=tst, family="binomial")
anova(bsit.n, bsit.1)
anova(bsit.1, bsit.2)
anova(bsit.2, bsit.3)
anova(bsit.3, bsit.4)

#### PLOTS

inf.mns <- c()
sit.mns <- c()
mar.mns <- c()
mom.mns <- c()
for(i in 18:45){
	inf.mns[i-17] <- mean(tst$ninf[tst$age == i], na.rm=T)
	sit.mns[i-17] <- mean(tst$bbsit[tst$age == i], na.rm=T)
	mar.mns[i-17] <- mean(tst$married[tst$age == i], na.rm=T)	
	mom.mns[i-17] <- mean(tst$crmom[tst$age == i], na.rm=T)	
}
newdat <- data.frame("ninf" = inf.mns, "bbsit" = sit.mns, "married" = mar.mns)
pred <- predictSE.mer(bsit.3, newdat, se.fit = TRUE, type = "response",
              level = 0, print.matrix = FALSE)
newdat$est <- as.numeric(pred$fit)
newdat$upp <- as.numeric(pred$fit + 1.96*pred$se.fit)
newdat$low <- as.numeric(pred$fit - 1.96*pred$se.fit)

png("babysit.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(18, 45), ylim = c(0, 1), xlab = "Age", ylab = "% Living with mother", main = "", font = 2, font.lab = 2, cex.lab = 1.5, yaxt="n")
axis(2, at=pretty(c(0, 1)), lab=paste0(pretty(c(0, 1)) * 100, " %"), las=TRUE, font = 2)
lws <- loess(newdat$est ~ c(18:45), span=0.9)
lws.l <- loess(newdat$upp ~ c(18:45), span=0.9)
lws.u <- loess(newdat$low ~ c(18:45), span=0.9)
lws.obs <- loess(mom.mns ~ c(18:45), span=0.9)
x1 <- seq(18,45, (45 - 18)/1000)
polygon(c(x1, rev(x1)), c(predict(lws.u, x1), rev(predict(lws.l, x1))),
     col = "light gray", border = NA)
lines(x1, predict(lws, x1), col="dark gray", lwd = 2, lty=3) 
lines(x1, predict(lws.obs, x1), col="black", lwd = 2)  
dev.off()

####	Observed married vs. unmarried
tst$age <- as.numeric(tst$age)

um <- subset(tst, married == 0)
mm <- subset(tst, married == 1)

umom.N <- c()
mmom.N <- c()
umom.mns <- c()
mmom.mns <- c()
for(i in 18:45){
	umom.N[i-17] <- length(as.numeric(um$crmom[um$age >= i - 2 & um$age <= i + 2], na.rm=T))
	mmom.N[i-17] <- length(as.numeric(mm$crmom[mm$age >= i - 2 & mm$age <= i + 2], na.rm=T))
	umom.mns[i-17] <- mean(um$crmom[um$age >= i - 2 & um$age <= i + 2], na.rm=T)	
	mmom.mns[i-17] <- mean(mm$crmom[mm$age >= i - 2 & mm$age <= i + 2], na.rm=T)	
}
png("obs_crmom.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(18, 45), ylim = c(0, 1), xlab = "Age", ylab = "% living with mother", main = "", font = 2, font.lab = 2, cex.lab = 1.5, yaxt="n")
axis(2, at=pretty(c(0, 1)), lab=paste0(pretty(c(0, 1)) * 100, " %"), las=TRUE, font = 2)
points(c(18:45), umom.mns, cex = umom.N/10, pch = 16, col = "gray")
points(c(18:45), mmom.mns, cex = mmom.N/10, pch = 16, col = "black")
dev.off()

#####
mar.null <- lmer(married ~  (1|ego_ID), data=tst, family="binomial")
mar.mod <- lmer(married ~  bbsit*as.numeric(age) + (1|ego_ID), data=tst, family="binomial")
	lower <- coef(summary(mar.mod))[,1] + qnorm(.025)*coef(summary(mar.mod))[,2]
	upper <- coef(summary(mar.mod))[,1] + qnorm(.975)*coef(summary(mar.mod))[,2]
	exp(cbind(coef(summary(mar.mod)), lower, upper))
mar.cnt <- lmer(married ~  bbsit*as.numeric(age) + kids + (1|ego_ID), data=tst, family="binomial")
	lower <- coef(summary(mar.cnt))[,1] + qnorm(.025)*coef(summary(mar.cnt))[,2]
	upper <- coef(summary(mar.cnt))[,1] + qnorm(.975)*coef(summary(mar.cnt))[,2]
	exp(cbind(coef(summary(mar.cnt)), lower, upper))
anova(mar.null, mar.mod)
anova(mar.mod, mar.cnt)


newdat0 <- data.frame("bbsit" = 0, "age" = c(25:35))
newdat1 <- data.frame("bbsit" = 1, "age" = c(25:35))
newdat2 <- data.frame("bbsit" = 2, "age" = c(25:35))
pred0 <- predictSE.mer(mar.mod, newdat0, se.fit = TRUE, type = "response",
              level = 0, print.matrix = FALSE)
pred1 <- predictSE.mer(mar.mod, newdat1, se.fit = TRUE, type = "response",
              level = 0, print.matrix = FALSE)
pred2 <- predictSE.mer(mar.mod, newdat2, se.fit = TRUE, type = "response",
              level = 0, print.matrix = FALSE)
newdat0$est <- as.numeric(pred0$fit)
newdat1$est <- as.numeric(pred1$fit)
newdat2$est <- as.numeric(pred2$fit)
png("mar_bbsit.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(25, 35), ylim = c(0.4, 1), xlab = "Age", ylab = "Pred. Prob. of marriage", main = "", font = 2, font.lab = 2, cex.lab = 1.5, yaxt="n")
axis(2, at=pretty(c(0.4, 1)), lab=paste0(pretty(c(0.4, 1)) * 100, " %"), las=TRUE, font = 2)
lines(c(25:35), newdat0$est, col="light gray", lwd = 3, lty = 1)  
lines(c(25:35), newdat1$est, col="dark gray", lwd = 3, lty = 2)  
lines(c(25:35), newdat2$est, col="black", lwd = 3, lty = 3)  
legend("bottomright", c("0 babysitters", "1 babysitter", "2 babysitters"), col = c("light gray", "dark gray", "black"), lty = c(1, 2, 3), cex= 1.5, box.lwd = 0, bty = "n")   
dev.off()








# married only
bsit.u <- lmer(crmom ~  ninf + bbsit + (1|ego_ID), data=subset(tst, married==0), family="binomial")
bsit.m <- lmer(crmom ~  ninf + bbsit + (1|ego_ID), data=subset(tst, married==1), family="binomial")
um <- subset(tst, married == 0)
mm <- subset(tst, married == 1)

inf.mns <- c()
sit.mns <- c()
umom.mns <- c()
mmom.mns <- c()
for(i in 18:45){
	inf.mns[i-17] <- mean(tst$ninf[tst$age == i], na.rm=T)
	sit.mns[i-17] <- mean(tst$bbsit[tst$age == i], na.rm=T)
	umom.mns[i-17] <- mean(um$crmom[um$age >= i - 2.5 & um$age <= i + 2.5], na.rm=T)	
	mmom.mns[i-17] <- mean(mm$crmom[mm$age >= i - 2.5 & mm$age <= i + 2.5], na.rm=T)	
}
newdat.u <- data.frame("ninf" = inf.mns, "bbsit" = sit.mns, "married" = 0)
newdat.m <- data.frame("ninf" = inf.mns, "bbsit" = sit.mns, "married" = 1)

pred <- predictSE.mer(bsit.3, newdat.u, se.fit = TRUE, type = "response",
              level = 0, print.matrix = FALSE)
newdat.u$est <- as.numeric(pred$fit)
newdat.u$upp <- as.numeric(pred$fit + 1.96*pred$se.fit)
newdat.u$low <- as.numeric(pred$fit - 1.96*pred$se.fit)
#
pred <- predictSE.mer(bsit.3, newdat.m, se.fit = TRUE, type = "response",
              level = 0, print.matrix = FALSE)
newdat.m$est <- as.numeric(pred$fit)
newdat.m$upp <- as.numeric(pred$fit + 1.96*pred$se.fit)
newdat.m$low <- as.numeric(pred$fit - 1.96*pred$se.fit)

png("wo_mar_babysit.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(18, 45), ylim = c(0, 1), xlab = "Age", ylab = "% Living with mother", main = "", font = 2, font.lab = 2, cex.lab = 1.5, yaxt="n")
axis(2, at=pretty(c(0, 0.75)), lab=paste0(pretty(c(0, 0.75)) * 100, " %"), las=TRUE, font = 2)
#
u_lws <- loess(newdat.u$est ~ c(18:45), span=0.9)
u_lws.l <- loess(newdat.u$upp ~ c(18:45), span=0.9)
u_lws.u <- loess(newdat.u$low ~ c(18:45), span=0.9)
#
m_lws <- loess(newdat.m$est ~ c(18:45), span=0.9)
m_lws.l <- loess(newdat.m$upp ~ c(18:45), span=0.9)
m_lws.u <- loess(newdat.m$low ~ c(18:45), span=0.9)
x1 <- seq(18,45, (45 - 18)/1000)
polygon(c(x1, rev(x1)), c(predict(u_lws.u, x1), rev(predict(u_lws.l, x1))),
     col = "slategray1", border = NA)
lines(x1, predict(u_lws, x1), col="slategray", lwd = 2, lty=3) 
#
polygon(c(x1, rev(x1)), c(predict(m_lws.u, x1), rev(predict(m_lws.l, x1))),
     col = "pink", border = NA)
lines(x1, predict(m_lws, x1), col="red", lwd = 2, lty=3) 
dev.off()


