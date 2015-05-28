rm(list=ls())
setwd("~/GitHub/anthro")
library(effsize)
######
######
######

inf <- read.csv("menmob/data/tweinfo.csv", strip.white = T)
mob <- read.csv("menmob/data/menmob_event.csv", strip.white = T)
rep <- read.csv("menmob/data/repro.csv", strip.white = T)
trk <- read.csv("womenmob/data/trackdat.csv", strip.white = T)
ltm <- read.csv("womenmob/data/ltmob.csv", strip.white = T)


##  PREPROCESSING!!!
			tst <- merge(mob, inf, by = "ID", all.x=T)	
	mob <- merge(inf[1], mob[,c(2, 4:5, 12:14)], by = "ID", all.y=T)
		mob$matn <- mob$x_num*mob$bin_mate
		mob$matm <- mob$x_many*mob$bin_mate
			mob$tot <- ifelse(!is.na(mob$bin_mate), 1, 0)
			mob$bin_mate[is.na(mob$bin_mate)] <- 999 
			mob$n_mate[is.na(mob$n_mate)] <- 999 
			mob$funcmate[is.na(mob$funcmate)] <- 999 
			mob$matn[is.na(mob$matn)] <- 999 
			mob$matm[is.na(mob$matm)] <- 999 
	mob <- aggregate(. ~ ID, mob, sum)
		mob$bin_mate[mob$bin_mate >= 999] <- NA
		mob$n_mate[mob$n_mate >= 999] <- NA
		mob$funcmate[mob$funcmate >= 999] <- NA
		mob$matn[mob$matn >= 999] <- NA
		mob$matm[mob$matm >= 999] <- NA

	#########
	for(i in 1:nrow(ltm)){
		ltm$ltm[i] <- mean(as.numeric(ltm[i, 2:21]), na.rm=T) - 1
	}
	#########
	trk <- aggregate( . ~ ID, data = trk, mean)
	trk$rng <- sqrt(trk$rng/1000)
	trk$net <- trk$net/1000
	trk <- subset(trk, ID != 16) # I know that this guy turned off his tracker and left it at home

dat <- merge(inf, mob, by = "ID", all.x=T)
dat <- merge(dat, rep[1:2], by = "ID", all.x=T)
dat <- merge(dat, ltm[c(1, 22)], by = "ID", all.x=T)
dat <- merge(dat, trk[c(1, 4, 6)], by = "ID", all.x=T)
dat$box <- ifelse(dat$male == 1, 1, 0)
dat$box <- ifelse(dat$male == 1 & dat$bin_mate > 0, 2, dat$box)


#### Descriptives

	#  54.5% of locations w/ lovers visited "many times" compared to 20% of other locations.
	#  On net, 23 "many" locations with lovers and 28 "many" locations without.
	#  So... 24% of places men visited had lovers, but 45% of regular haunts had lovers.
	#  13.5% of all men's trips *explicitly* in search of mates, and 27% of all "many" men's trips

#### FIGURES

library(ggplot2)
library(grid)
library(gridExtra)

 # Daily
	# Distribution comparisons men vs. women
png("menmob/hbes_talk/mobrng_sex.png", width=500, height=500, pointsize=12)
	dat$male <- as.factor(dat$male)
	ggplot(dat, aes(x=rng, group=male, fill=male)) + geom_density(alpha = 0.25) +
	ggtitle('Daily range') +
	labs(x = "Range (km^2)", y = "") +
	theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.text.x=element_text(size=20, vjust=0.5), axis.title.x = element_text(size=30, vjust=-0.35), plot.title = element_text(size=30, face="bold", vjust=2))
dev.off()

	# changes across age
png("menmob/hbes_talk/dailyage.png", height = 500, width = 500)
	par(oma = c(1,1,1,1))
	plot(NULL, xlim=c(18, 80), ylim=c(0,135), ylab = "Range (km^2)", xlab = "Age", main = "Daily range size", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)

	abline(h = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130), col = "light grey")
	abline(v = c(20, 30, 40, 50, 60, 70, 80), col = "light grey")

	points(dat$age[dat$male == 1], dat$rng[dat$male == 1], pch = 16, col = "deepskyblue3")
		lws <- loess(rng ~ age, data = subset(dat, male == 1), span = .9)
		lines(18:80, predict(lws, 18:80), col = "deepskyblue3", lwd = 2)
	points(dat$age[dat$male == 0], dat$rng[dat$male == 0], pch = 16, col = "salmon")
		lws <- loess(rng ~ age, data = subset(dat, male == 0), span = .9)
		lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)

	legend("topright", c("Men", "Women"), col = c("deepskyblue3", "salmon"), pch = c(16, 16), lty = c(1, 1), bty = "n")
dev.off()


 # ANNUAL
	# Distribution comparisons men vs. women
png("menmob/hbes_talk/mobtot_sex.png", width=500, height=500, pointsize=12)
	dat$male <- as.factor(dat$male)
	ggplot(dat, aes(x=tot, group=male, fill=male)) + geom_density(alpha = 0.25) +
	ggtitle('Annual travel') +
	labs(x = "Unique visits", y = "") +
	theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.text.x=element_text(size=20, vjust=0.5), axis.title.x = element_text(size=30, vjust=-0.35), plot.title = element_text(size=30, face="bold", vjust=2))
dev.off()

	# changes across age
png("menmob/hbes_talk/annualage.png", height = 500, width = 500)
	par(oma = c(1,1,1,1))
	plot(NULL, xlim=c(18, 80), ylim=c(0,20), ylab = "Unique visits", xlab = "Age", main = "Annual ranging", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)

	abline(h = c(0, 5, 10, 15, 20), col = "light grey")
	abline(v = c(20, 30, 40, 50, 60, 70, 80), col = "light grey")

	points(dat$age[dat$male == 1], dat$tot[dat$male == 1], pch = 16, col = "deepskyblue3")
		lws <- loess(tot ~ age, data = subset(dat, male == 1), span = .9)
		lines(18:80, predict(lws, 18:80), col = "deepskyblue3", lwd = 2)
	points(dat$age[dat$male == 0], dat$tot[dat$male == 0], pch = 16, col = "salmon")
		lws <- loess(tot ~ age, data = subset(dat, male == 0), span = .9)
		lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)

	legend("topright", c("Men", "Women"), col = c("deepskyblue3", "salmon"), pch = c(16, 16), lty = c(1, 1), bty = "n")
dev.off()


 # Lifetime
	# Distribution comparisons men vs. women
png("menmob/hbes_talk/mobltm_sex.png", width=500, height=500, pointsize=12)
	dat$male <- as.factor(dat$male)
	ggplot(dat, aes(x=ltm, group=male, fill=male)) + geom_density(alpha = 0.25) +
	ggtitle('Lifetime range') +
	labs(x = "Lifetime range (Ordinal from 0 to 3)", y = "") +
	theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.text.x=element_text(size=20, vjust=0.5), axis.title.x = element_text(size=30, vjust=-0.35), plot.title = element_text(size=30, face="bold", vjust=2))
dev.off()

	# changes across age
png("menmob/hbes_talk/ltage.png", height = 500, width = 500)
	par(oma = c(1,1,1,1))
	plot(NULL, xlim=c(18, 80), ylim=c(0,3), ylab = "Lifetime range (Ordinal from 0 to 3)", xlab = "Age", main = "Lifetime range", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)

	abline(h = c(0, .5, 1, 1.5, 2, 2.5, 3), col = "light grey")
	abline(v = c(20, 30, 40, 50, 60, 70, 80), col = "light grey")

	points(dat$age[dat$male == 1], dat$ltm[dat$male == 1], pch = 16, col = "deepskyblue3")
		lws <- loess(ltm ~ age, data = subset(dat, male == 1), span = .9)
		lines(18:80, predict(lws, 18:80), col = "deepskyblue3", lwd = 2)
	points(dat$age[dat$male == 0], dat$ltm[dat$male == 0], pch = 16, col = "salmon")
		lws <- loess(ltm ~ age, data = subset(dat, male == 0), span = .9)
		lines(18:80, predict(lws, 18:80), col = "salmon", lwd = 2)

	legend("bottomright", c("Men", "Women"), col = c("deepskyblue3", "salmon"), pch = c(16, 16), lty = c(1, 1), bty = "n")
dev.off()

####  HYPOTHESIS 2
	# Focus on sex diff in lover travel
	png("menmob/hbes_talk/mobmat_sex.png", width=500, height=500, pointsize=12)
	dat$male <- as.factor(dat$male)
	ggplot(dat, aes(x=bin_mate, group=male, fill=male)) + geom_density(alpha = 0.25) +
	ggtitle('Mating travel') +
	labs(x = "Locations w/ lover", y = "") +
	coord_cartesian(ylim=c(0, 1)) +
	theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), axis.text.x=element_text(size=20, vjust=0.5), axis.title.x = element_text(size=30, vjust=-0.35), plot.title = element_text(size=30, face="bold", vjust=2))
dev.off()
	# Does lover travel explain the diff?
png("menmob/hbes_talk/boxtot.png", width=500, height=500, pointsize=12)
	boxplot(tot~male, data=dat, notch=FALSE, ylim = c(0, 12.5), col=(c("salmon","deepskyblue3")), main="", xlab="", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5, xaxt='n')
	axis(side = 1, at = c(1, 2), labels = c("Women", "Men"))
dev.off()
png("menmob/hbes_talk/boxtot_rmv.png", width=500, height=500, pointsize=12)
	boxplot(tot~box, data=dat, notch=FALSE, ylim = c(0, 12.5), col=(c("salmon","deepskyblue3","deepskyblue3")), main="", xlab="", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5, xaxt='n')
	axis(side = 1, at = c(1, 2, 3), labels = c("Women", "Men (no-lovers)", "Men (lovers)"))
dev.off()


	# dealing with the manys
	mentrvl <- c()
	womtrvl <- c()
	for(i in 5:50){
		mentrvl[i-4] <- sum((dat$matn[dat$male == 1] + dat$matm[dat$male == 1]*i), na.rm= T)/sum((dat$x_num[dat$male == 1] + dat$x_many[dat$male == 1]*i), na.rm=T)
		#womtrvl[i-4] <- mean((dat$x_num[dat$male == 0] + dat$x_many[dat$male == 0]*i) - (dat$matn[dat$male == 0] + dat$matm[dat$male == 0]*i), na.rm=T)
	}


####  HYPOTHESIS 3
	day_m <- lm(num_kids ~ scale(rng) + scale(age), data = subset(dat, male == 1))
		day_f <- lm(num_kids ~ scale(rng) + scale(age), data = subset(dat, male == 0))	
	yer_m <-  lm(num_kids ~ scale(tot) + scale(age), data = subset(dat, male == 1))
		yer_f <-  lm(num_kids ~ scale(tot) + scale(age), data = subset(dat, male == 0))	
	lif_m <-  lm(num_kids ~ scale(ltm) + scale(age), data = subset(dat, male == 1))
		lif_f <-  lm(num_kids ~ scale(ltm) + scale(age), data = subset(dat, male == 0))	

	# Daily
	hld <- subset(dat, male == 1 & !is.na(rng) & !is.na(num_kids) & !is.na(age))
		rngage <- lm(rng ~ age, data = hld)
		rsage <- lm(num_kids ~ age, data = hld)
	png("menmob/hbes_talk/rsrng.png", height = 500, width = 500)
		m1 <- lm(scale(resid(rngage)) ~ scale(resid(rsage)))
		plot(scale(resid(rsage)), scale(resid(rngage)), xlab = "Reproductive Success", ylab = "Daily range", main = paste0("Std B = ", round(as.numeric(coef(m1)[2]), 2), ", ", "p = ", round(summary(m1)$coefficients[2,4], 3)), pch = 16, font = 2, font.lab = 2, cex.main = 1.5, cex.lab = 1.5)
			abline(m1, col = "red", lwd = 2)
	dev.off()
	hld <- subset(dat, male == 0 & !is.na(rng) & !is.na(num_kids) & !is.na(age))
		rngage <- lm(rng ~ age, data = hld)
		rsage <- lm(num_kids ~ age, data = hld)
	png("menmob/hbes_talk/rsrng_f.png", height = 500, width = 500)
		m1 <- lm(scale(resid(rngage)) ~ scale(resid(rsage)))
		plot(scale(resid(rsage)), scale(resid(rngage)), xlab = "Reproductive Success", ylab = "Daily range", main = paste0("Std B = ", round(as.numeric(coef(m1)[2]), 2), ", ", "p = ", round(summary(m1)$coefficients[2,4], 3)), pch = 16, font = 2, font.lab = 2, cex.main = 1.5, cex.lab = 1.5)
			abline(m1, col = "red", lwd = 2)
	dev.off()

	# Annual
	hld <- subset(dat, male == 1 & !is.na(tot) & !is.na(num_kids) & !is.na(age))
		totage <- lm(tot ~ age, data = hld)
		rsage <- lm(num_kids ~ age, data = hld)
	png("menmob/hbes_talk/rstot.png", height = 500, width = 500)
		m1 <- lm(scale(resid(totage)) ~ scale(resid(rsage)))
		plot(scale(resid(rsage)), scale(resid(totage)), xlab = "Reproductive Success", ylab = "Annual travel", main = paste0("Std B = ", round(as.numeric(coef(m1)[2]), 2), ", ", "p = ", round(summary(m1)$coefficients[2,4], 3)), pch = 16, font = 2, font.lab = 2, cex.main = 1.5, cex.lab = 1.5)
			abline(m1, col = "red", lwd = 2)
	dev.off()
	hld <- subset(dat, male == 0 & !is.na(tot) & !is.na(num_kids) & !is.na(age))
		totage <- lm(tot ~ age, data = hld)
		rsage <- lm(num_kids ~ age, data = hld)
	png("menmob/hbes_talk/rstot_f.png", height = 500, width = 500)
		m1 <- lm(scale(resid(totage)) ~ scale(resid(rsage)))
		plot(scale(resid(rsage)), scale(resid(totage)), xlab = "Reproductive Success", ylab = "Annual travel", main = paste0("Std B = ", round(as.numeric(coef(m1)[2]), 2), ", ", "p = ", round(summary(m1)$coefficients[2,4], 3)), pch = 16, font = 2, font.lab = 2, cex.main = 1.5, cex.lab = 1.5)
			abline(m1, col = "red", lwd = 2)
	dev.off()

	# Annual
	hld <- subset(dat, male == 1 & !is.na(ltm) & !is.na(num_kids) & !is.na(age))
		ltmage <- lm(ltm ~ age, data = hld)
		rsage <- lm(num_kids ~ age, data = hld)
	png("menmob/hbes_talk/rsltm.png", height = 500, width = 500)
		m1 <- lm(scale(resid(ltmage)) ~ scale(resid(rsage)))
		plot(scale(resid(rsage)), scale(resid(ltmage)), xlab = "Reproductive Success", ylab = "Lieftime range", main = paste0("Std B = ", round(as.numeric(coef(m1)[2]), 2), ", ", "p = ", round(summary(m1)$coefficients[2,4], 3)), pch = 16, font = 2, font.lab = 2, cex.main = 1.5, cex.lab = 1.5)
			abline(m1, col = "red", lwd = 2)
	dev.off()
	hld <- subset(dat, male == 0 & !is.na(ltm) & !is.na(num_kids) & !is.na(age))
		ltmage <- lm(ltm ~ age, data = hld)
		rsage <- lm(num_kids ~ age, data = hld)
	png("menmob/hbes_talk/rsltm_f.png", height = 500, width = 500)
		m1 <- lm(scale(resid(ltmage)) ~ scale(resid(rsage)))
		plot(scale(resid(rsage)), scale(resid(ltmage)), xlab = "Reproductive Success", ylab = "Lieftime range", main = paste0("Std B = ", round(as.numeric(coef(m1)[2]), 2), ", ", "p = ", round(summary(m1)$coefficients[2,4], 3)), pch = 16, font = 2, font.lab = 2, cex.main = 1.5, cex.lab = 1.5)
			abline(m1, col = "red", lwd = 2)
	dev.off()



















# QQ plots for paper.
png("QQ_sex.png", height = 750, width = 500)
par(mfrow=c(3,2), oma = c(0, 2, 0, 2))
	abmin <- min(c(dat$Acc[dat$male == 1 & dat$Acc >= .5], dat$Acc[dat$male == 0 & dat$Acc >= .5]), na.rm=T)
	abmax <- max(c(dat$Acc[dat$male == 1 & dat$Acc >= .5], dat$Acc[dat$male == 0 & dat$Acc >= .5]), na.rm=T)
qqplot(dat$Acc[dat$male == 1 & dat$Acc >= .5], dat$Acc[dat$male == 0 & dat$Acc >= .5], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 2, cex.main = 2,
		xlab = "", ylab = "Women", main = "Mental rotation (accuracy)")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$pnt.err[dat$male == 1], dat$pnt.err[dat$male == 0]), na.rm=T)
	abmax <- max(c(dat$pnt.err[dat$male == 1], dat$pnt.err[dat$male == 0]), na.rm=T)
qqplot(dat$pnt.err[dat$male == 1], dat$pnt.err[dat$male == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 2, cex.main = 2,
		xlab = "", ylab = "", main = "Pointing error")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$sax[dat$male == 1], dat$sax[dat$male == 0]), na.rm=T)
	abmax <- max(c(dat$sax[dat$male == 1], dat$sax[dat$male == 0]), na.rm=T)
qqplot(dat$sax[dat$male == 1], dat$sax[dat$male == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 2, cex.main = 2,
		xlab = "", ylab = "Women", main = "Spatial anxiety")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$tot[dat$male == 1], dat$tot[dat$male == 0]), na.rm=T)
	abmax <- max(c(dat$tot[dat$male == 1], dat$tot[dat$male == 0]), na.rm=T)
qqplot(dat$tot[dat$male == 1], dat$tot[dat$male == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 2, cex.main = 2,
		xlab = "", ylab = "", main = "Annual mobility")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$avc[dat$male == 1], dat$avc[dat$male == 0]), na.rm=T)
	abmax <- max(c(dat$avc[dat$male == 1], dat$avc[dat$male == 0]), na.rm=T)
qqplot(dat$avc[dat$male == 1], dat$avc[dat$male == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 2, cex.main = 2,
		xlab = "Men", ylab = "Women", main = "% of trips solo")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$net[dat$male == 1], dat$net[dat$male == 0]), na.rm=T)
	abmax <- max(c(dat$net[dat$male == 1], dat$net[dat$male == 0]), na.rm=T)
qqplot(dat$net[dat$male == 1], dat$net[dat$male == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 2, cex.main = 2,
		xlab = "Men", ylab = "", main = "Daily mobility")
	abline(0,1, col = "grey", lwd = 2)
dev.off()

png("QQ_pst.png", height = 500, width = 500)
par(mfrow=c(2,2), omi = c(0, 1, 0, 0))
	abmin <- min(c(dat$sax[dat$male == 0 & dat$pst.repro == 1], dat$sax[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
	abmax <- max(c(dat$sax[dat$male == 0 & dat$pst.repro == 1], dat$sax[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
qqplot(dat$sax[dat$male == 0 & dat$pst.repro == 1], dat$sax[dat$male == 0 & dat$pst.repro == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 1.5, cex.main = 1.5,
		xlab = "", ylab = "Reproductive-aged", main = "Spatial anxiety")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$tot[dat$male == 0 & dat$pst.repro == 1], dat$tot[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
	abmax <- max(c(dat$tot[dat$male == 0 & dat$pst.repro == 1], dat$tot[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
qqplot(dat$tot[dat$male == 0 & dat$pst.repro == 1], dat$tot[dat$male == 0 & dat$pst.repro == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 1.5, cex.main = 1.5,
		xlab = "", ylab = "", main = "Annual mobility")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$avc[dat$male == 0 & dat$pst.repro == 1], dat$avc[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
	abmax <- max(c(dat$avc[dat$male == 0 & dat$pst.repro == 1], dat$avc[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
qqplot(dat$avc[dat$male == 0 & dat$pst.repro == 1], dat$avc[dat$male == 0 & dat$pst.repro == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 1.5, cex.main = 1.5,
		xlab = "Postmenopausal", ylab = "Reproductive-aged", main = "% of trips solo")
	abline(0,1, col = "grey", lwd = 2)

	abmin <- min(c(dat$net[dat$male == 0 & dat$pst.repro == 1], dat$net[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
	abmax <- max(c(dat$net[dat$male == 0 & dat$pst.repro == 1], dat$net[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
qqplot(dat$net[dat$male == 0 & dat$pst.repro == 1], dat$net[dat$male == 0 & dat$pst.repro == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16, cex.lab = 1.5, cex.main = 1.5,
		xlab = "Postmenopausal", ylab = "", main = "Daily mobility")
	abline(0,1, col = "grey", lwd = 2)
dev.off()



png("lactmob.png", height = 500, width = 500)
plot(NULL, xlim=c(18, 70), ylim=c(0,8), ylab = "Annual mobility", xlab = "Age", font = 2, font.lab = 2, cex = 1.25, cex.lab = 1.25, cex.main = 1.25)
	points(jitter(dat$age[dat$male == 0 & dat$bfeed == 1 & dat$ID != 106]), jitter(dat$tot[dat$male == 0 & dat$bfeed == 1 & dat$ID != 106]), pch = 16, col = "black")
		lws <- loess(tot ~ age, data = subset(dat, male == 0 & bfeed == 1 & ID != 106), span = 1)
			plx <- predict(lws, 18:70, se=T)
			lines(18:70, plx$fit, col = "black", lwd = 2)
			lines(18:70, plx$fit - plx$se, col = "black", lwd = 1, lty = 2)
			lines(18:70, plx$fit + plx$se, col = "black", lwd = 1, lty = 2)
	points(jitter(dat$age[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0]), jitter(dat$tot[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0]), pch = 17, col = "dark grey")
		lws <- loess(tot ~ age, data = subset(dat, male == 0 & bfeed == 0 & preg == 0), span = 1)
			plx <- predict(lws, 18:70, se=T)
			lines(18:70, plx$fit, col = "dark grey", lwd = 2, lty = 1)
			lines(18:70, plx$fit - plx$se, col = "dark grey", lwd = 1, lty = 2)
			lines(18:70, plx$fit + plx$se, col = "dark grey", lwd = 1, lty = 2)
	abline(v = 50, lty = 3)
	#legend("topleft", c("Currently breastfeeding", "Not breastfeeding"), pch = c(16, 17), col = c("black", "dark grey"), bty = "n", cex = 1.5) 
dev.off()




















png("QQ_3_sax.png", height = 500, width = 500)
par(mfrow=c(2,2))
	abmin <- min(c(dat$sax[dat$male == 1], dat$sax[dat$male == 0]), na.rm=T)
	abmax <- max(c(dat$sax[dat$male == 1], dat$sax[dat$male == 0]), na.rm=T)
qqplot(dat$sax[dat$male == 1], dat$sax[dat$male == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16,
		xlab = "Men", ylab = "Women", main = "Spatial Anxiety")
	abline(0,1, col = "red", lwd = 2)

	abmin <- min(c(dat$sax[dat$male == 0 & dat$pst.repro == 1], dat$sax[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
	abmax <- max(c(dat$sax[dat$male == 0 & dat$pst.repro == 1], dat$sax[dat$male == 0 & dat$pst.repro  == 0]), na.rm=T)
qqplot(dat$sax[dat$male == 0 & dat$pst.repro == 1], dat$sax[dat$male == 0 & dat$pst.repro == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16,
		xlab = "Postmenopausal", ylab = "Reproductive-aged", main = "Spatial Anxiety")
	abline(0,1, col = "red", lwd = 2)

	abmin <- min(c(dat$sax[dat$male == 0 & dat$bfeed == 1], dat$sax[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0]), na.rm=T)
	abmax <- max(c(dat$sax[dat$male == 0 & dat$bfeed == 1], dat$sax[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0]), na.rm=T)
qqplot(dat$sax[dat$male == 0 & dat$bfeed == 1], dat$sax[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16,
		xlab = "Breastfeeding", ylab = "Not Breastfeeding", main = "Spatial Anxiety")
	abline(0,1, col = "red", lwd = 2)

	abmin <- min(c(dat$sax[dat$male == 0 & dat$preg == 1], dat$sax[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0]), na.rm=T)
	abmax <- max(c(dat$sax[dat$male == 0 & dat$preg == 1], dat$sax[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0]), na.rm=T)
qqplot(dat$sax[dat$male == 0 & dat$preg == 1], dat$sax[dat$male == 0 & dat$bfeed == 0 & dat$preg == 0], xlim = c(abmin, abmax), ylim = c(abmin, abmax), font = 2, font.lab = 2, pch = 16,
		xlab = "Pregnant", ylab = "Not Pregnant", main = "Spatial Anxiety")
	abline(0,1, col = "red", lwd = 2)
dev.off()

















png("menvwom_acctot.png", height = 500, width = 500)
	m1 <- lm(tot ~ Acc*male, subset(dat, Acc >= .5))
		prd <- data.frame(Acc = c(50:100)/100)
		prd$m <- coef(m1)[1] + prd$Acc*coef(m1)[2] + coef(m1)[3] + coef(m1)[4]*prd$Acc
		prd$f <- coef(m1)[1] + prd$Acc*coef(m1)[2]
plot(NULL, xlim=c(0.5, 1), ylim=c(0,20), ylab = "Annual mobility", xlab = "Mental Rotation (% Correct)", font = 2, font.lab = 2, cex = 1.5, cex.lab = 1.5)
points(dat$Acc[dat$male == 1 & dat$Acc >= .5], dat$tot[dat$male == 1 & dat$Acc >= .5], pch = 16, col = "black")
	lines(prd$Acc, prd$m, col = "black", lwd = 2)
points(dat$Acc[dat$male == 0 & dat$Acc >= .5], dat$tot[dat$male == 0 & dat$Acc >= .5], pch = 17, col = "dark grey")
	lines(prd$Acc, prd$f, col = "dark grey", lwd = 2)
	legend("topleft", c("Men", "Women"), pch = c(16, 17), col = c("black", "dark grey"), bty = "n", cex = 1.5)
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









