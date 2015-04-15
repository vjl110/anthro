rm(list=ls())
setwd("~/GitHub/anthro")

######
######
######

inf <- read.csv("womenmob/data/tweinfo.csv", strip.white = T)
rep <- read.csv("womenmob/data/repro.csv", strip.white = T)
corsi <- read.csv("womenmob/data/twecorsi.csv", strip.white = T)
psp <- read.csv("womenmob/data/persp_err.csv", strip.white = T)
pnt <- read.csv("womenmob/data/pnt_byrow.csv", strip.white = T)
mr <- read.csv("womenmob/data/MR_bytrial.csv", strip.white = T)
mob <- read.csv("womenmob/data/yrmob.csv", strip.white = T)
srvy <- read.csv("womenmob/data/survey.csv", strip.white = T)
trk <- read.csv("womenmob/data/trackdat.csv", strip.white = T)
ltm <- read.csv("womenmob/data/ltmob.csv", strip.white = T)


	# MENTAL ROTATION DATA
		lowerq = quantile(mr$RT)[2]
		upperq = quantile(mr$RT)[3]
		iqr = upperq - lowerq #Or use IQR(data)
		outlier <- (iqr * 5) + upperq
	mr <- subset(mr, RT <= outlier & Trial != 1 & Degree != 0)
	mr <- mr[c(1, 5:6)]
	mr <- aggregate( . ~ Subject, data = mr, mean)
	colnames(mr)[1] <- "ID"

	# PERSPECTIVE TAKING DATA
	psp <- merge(inf, psp, by="ID")
	for(i in 1:nrow(psp)){
		psp$avg[i] <- ifelse(is.na(mean(as.numeric(abs(psp[i, 9:20])))), NA, mean(as.numeric(abs(psp[i, 9:20]))))
	}
	# POINTING DATA
	hld <- merge(pnt, inf, by="ID", all.x = T)
	# First step is to remove distance component then calculate an average score for individuals.
	hld$Bearing_Difference <- abs(hld$Bearing_Difference)
	hld <- subset(hld, to != "otjinungua" & to != "ruacana" & to != "ongokua")
	hld <- hld[,c(1,10)]
	hld <- aggregate( . ~ ID, data = hld, mean)
	colnames(hld)[2] <- "pnt.err" 
	########
	for(i in 1:nrow(srvy)){
	srvy$ha[i]<-1-mean(c(srvy$HA1[i], srvy$HA2[i], srvy$HA3[i], srvy$HA4[i], srvy$HA5[i]),na.rm=T)
	srvy$sab[i] <- 4 - mean(c(srvy$SAB1b[i], srvy$SAB2[i], srvy$SAB3[i]), na.rm=T)   
	srvy$sax[i] <- 4 - mean(c(srvy$SAX1[i], srvy$SAX2[i], srvy$SAX3[i], (4 - srvy$SAX4[i])), na.rm=T) #removed sax1 because there was no variance   	
	}
	#########
	for(i in 1:nrow(ltm)){
		ltm$ltm[i] <- mean(as.numeric(ltm[i, 2:21]), na.rm=T) - 1
	}
	#########
	mob$avc <- mob$alone/(mob$alone + mob$company)
	#########
	trk <- aggregate( . ~ ID, data = trk, mean)


dat <- merge(inf, corsi[,c(1,5)], by = "ID", all.x=T)
dat <- merge(dat, mr, by = "ID", all.x=T)
dat <- merge(dat, rep[c(1,4:6, 8)], by = "ID", all.x=T)
dat <- merge(dat, psp[c(1, ncol(psp))], by = "ID", all.x=T)
dat <- merge(dat, hld, by = "ID", all.x=T)
dat <- merge(dat, mob[c(1:2, 9)], by = "ID", all.x=T)
dat <- merge(dat, srvy[c(1,31:33)], by = "ID", all.x=T)
dat <- merge(dat, ltm[c(1,22)], by = "ID", all.x=T)
dat <- merge(dat, trk[c(1,6)], by = "ID", all.x=T)

tst <- merge(inf, srvy, by = "ID", all.x=T)
#dat <- merge(dat, srvy[c(1,21:30)], by = "ID", all.x=T)

#dat <- subset(dat, male == 0 & age <= 50)
#dat <- subset(dat, male == 0 & pst.repro != 1)

png("bfeed_tot.png", height = 500, width = 500)
plot(NULL, xlim=c(18, 50), ylim=c(0,8), ylab = "Unique Visits", xlab = "Age", font = 2, font.lab = 2, cex = 1.5, cex.lab = 1.5)
	points(jitter(dat$age[dat$male == 0 & dat$bfeed == 1]), jitter(dat$tot[dat$male == 0 & dat$bfeed == 1]), pch = 16, col = "black")
		lws <- loess(tot ~ age, data = subset(dat, male == 0 & bfeed == 1), span = .9)
		lines(18:50, predict(lws, 18:50), col = "black", lwd = 2)
	points(jitter(dat$age[dat$male == 0 & dat$bfeed == 0]), jitter(dat$tot[dat$male == 0 & dat$bfeed == 0]), pch = 17, col = "dark grey")
		lws <- loess(tot ~ age, data = subset(dat, male == 0 & bfeed == 0), span = .9)
		lines(18:50, predict(lws, 18:50), col = "dark grey", lwd = 2, lty = 2)
	legend("topleft", c("Currently breastfeeding", "Not breastfeeding"), pch = c(16, 17), col = c("black", "dark grey"), bty = "n", cex = 1.5) 
dev.off()


png("menvwom_acctot.png", height = 500, width = 500)
	m1 <- lm(tot ~ Acc*male, subset(dat, Acc >= .5))
		prd <- data.frame(Acc = c(50:100)/100)
		prd$m <- coef(m1)[1] + prd$Acc*coef(m1)[2] + coef(m1)[3] + coef(m1)[4]*prd$Acc
		prd$f <- coef(m1)[1] + prd$Acc*coef(m1)[2]
plot(NULL, xlim=c(0.5, 1), ylim=c(0,20), ylab = "Unique Visits", xlab = "Mental Rotation (% Correct)", font = 2, font.lab = 2, cex = 1.5, cex.lab = 1.5)
points(dat$Acc[dat$male == 1 & dat$Acc >= .5], dat$tot[dat$male == 1 & dat$Acc >= .5], pch = 16, col = "light blue")
	lines(prd$Acc, prd$m, col = "blue", lwd = 2)
points(dat$Acc[dat$male == 0 & dat$Acc >= .5], dat$tot[dat$male == 0 & dat$Acc >= .5], pch = 16, col = "salmon")
	lines(prd$Acc, prd$f, col = "red", lwd = 2)
dev.off()











png("menvwom.png", height = 500, width = 500)
plot(NULL, xlim=c(18, 75), ylim=c(0,1), ylab = "Mental Rotation Accuracy", xlab = "Age", font = 2)
points(dat$age[dat$male == 1], dat$Acc[dat$male == 1], pch = 16, col = "blue")
	lws <- loess(Acc ~ age, data = subset(dat, male == 1), span = .9)
	lines(18:80, predict(lws, 18:80), col = "blue", lwd = 2)
points(dat$age[dat$male == 0], dat$Acc[dat$male == 0], pch = 16, col = "salmon")
	lws <- loess(Acc ~ age, data = subset(dat, male == 0), span = .9)
	lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)
dev.off()

png("menvwom_span.png", height = 500, width = 500)
plot(NULL, xlim=c(18, 75), ylim=c(0,6), ylab = "Span (corsi task)", xlab = "Age", font = 2)
points(dat$age[dat$male == 1], dat$span[dat$male == 1], pch = 16, col = "blue")
	lws <- loess(span ~ age, data = subset(dat, male == 1), span = .9)
	lines(18:80, predict(lws, 18:80), col = "blue", lwd = 2)
points(dat$age[dat$male == 0], dat$span[dat$male == 0], pch = 16, col = "salmon")
	lws <- loess(span ~ age, data = subset(dat, male == 0), span = .9)
	lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)
dev.off()

png("menvwom_pnt.png", height = 500, width = 500)
plot(NULL, xlim=c(18, 75), ylim=c(0,40), ylab = "Degree Error Pointing task", xlab = "Age", font = 2)
points(dat$age[dat$male == 1], dat$pnt.err[dat$male == 1], pch = 16, col = "blue")
	lws <- loess(pnt.err ~ age, data = subset(dat, male == 1), span = .9)
	lines(18:80, predict(lws, 18:80), col = "blue", lwd = 2)
points(dat$age[dat$male == 0], dat$pnt.err[dat$male == 0], pch = 16, col = "salmon")
	lws <- loess(pnt.err ~ age, data = subset(dat, male == 0), span = .9)
	lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)
dev.off()


png("menvwom_span.png", height = 500, width = 500)
plot(NULL, xlim=c(18, 75), ylim=c(0,10), ylab = "Span (corsi task)", xlab = "Age", font = 2)
points(dat$age[dat$male == 1], dat$tot[dat$male == 1], pch = 16, col = "blue")
	lws <- loess(tot ~ age, data = subset(dat, male == 1), span = .9)
	lines(18:80, predict(lws, 18:80), col = "blue", lwd = 2)
points(dat$age[dat$male == 0], dat$tot[dat$male == 0], pch = 16, col = "salmon")
	lws <- loess(tot ~ age, data = subset(dat, male == 0), span = .9)
	lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)
dev.off()



png("menvwom_net.png", height = 500, width = 500)
plot(NULL, xlim=c(18, 80), ylim=c(0,25000), ylab = "Average Daily Travel (meters)", xlab = "Age", main = "Twe Daily Mobility", font = 2, font.lab = 2)
points(dat$age[dat$male == 1], dat$net[dat$male == 1], pch = 1, col = "blue")
	lws <- loess(net ~ age, data = subset(dat, male == 1), span = .9)
	lines(18:80, predict(lws, 18:80), col = "blue", lwd = 2)
points(dat$age[dat$male == 0], dat$net[dat$male == 0], pch = 1, col = "salmon")
	lws <- loess(net ~ age, data = subset(dat, male == 0), span = .9)
	lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)
dev.off()



n1 <- length(na.omit(dat$ltm[dat$male == 0]))
n2 <- length(na.omit(dat$ltm[dat$male == 1]))

vr1 <- var(na.omit(dat$ltm[dat$male == 0]))
vr2 <- var(na.omit(dat$ltm[dat$male == 1]))

mn1 <- mean(na.omit(dat$ltm[dat$male == 0]))
mn2 <- mean(na.omit(dat$ltm[dat$male == 1]))

s <- sqrt(((n1 - 1)*vr1 + (n2 - 1)*vr2)/(n1 + n2))

(mn1 - mn2)/s









