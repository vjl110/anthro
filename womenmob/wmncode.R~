setwd("~/GitHub/anthro")

######
######
######

inf <- read.csv("womenmob/data/tweinfo.csv", strip.white = T)
rep <- read.csv("womenmob/data/repro.csv", strip.white = T)
mob <- read.csv("womenmob/data/yrmob.csv", strip.white = T)
srvy <- read.csv("womenmob/data/survey.csv", strip.white = T)
cog <- read.csv("womenmob/data/cog.csv", strip.white = T)
raw <- read.csv("womenmob/data/mobraw.csv", strip.white = T)
trk <- read.csv("womenmob/data/trackdat.csv", strip.white = T)
psp <- read.csv("womenmob/data/persp_err.csv", strip.white = T)


	for(i in 1:nrow(srvy)){
	srvy$rsk[i] <- mean(c(srvy$HA1[i], srvy$HA2[i], srvy$HA3[i], srvy$HA4[i], srvy$HA5[i], srvy$HA6[i], srvy$HA7[i], srvy$HA8[i], srvy$HA9[i], srvy$HA10[i]), na.rm=T)   
	srvy$sab[i] <- 4 - mean(c(srvy$SAB1b[i], srvy$SAB2[i], srvy$SAB3[i]), na.rm=T)   
	srvy$sax[i] <- 4 - mean(c(srvy$SAX1[i], srvy$SAX2[i], srvy$SAX3[i], (4 - srvy$SAX4[i])), na.rm=T)   	
}

dat <- merge(inf, rep, by="ID")
dat <- merge(dat, raw, by="ID")
	dat <- subset(dat, male == 0 & age < 45)

dat <- merge(inf, rep, by="ID")
dat <- merge(dat, mob, by="ID")
dat <- merge(dat, cog, by="ID")

TRK <- merge(trk, inf, by= "ID", all.x = TRUE)
TRK <- merge(TRK, rep, by= "ID", all.x = TRUE)

psp <- merge(inf, psp, by="ID")
	for(i in 1:nrow(psp)){
		psp$avg[i] <- mean(as.numeric(abs(psp[i, 9:20])), na.rm = T)
	}
	
	##########  ATTEMPTING TO DEAL W/ INCOMPLETE DAY ISSUE ##########

	TRK <- TRK[c(1, 4:6, 8:9, 13:18)]
	TRK <- aggregate( . ~ ID, data = TRK, max)
###

m1 <- lm(tot ~ bfeed, data = subset(dat, male == 0 & age < 45))
summary(m1)

rep <- subset(dat, male == 0 & age < 45 & !is.na(bfeed))



	# both sexes over age.
png("bfeed_mob.png", width=500, height=500, pointsize=12)

plot(NULL, xlim = c(17, 45), ylim = c(0, 7), xlab = "Age", ylab = "Total locations visited", main = "Annual travel by reproductive status", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
rep <- rep[order(rep$age),]
points(jitter(rep$age[rep$bfeed == 0]), jitter(rep$tot[rep$bfeed == 0]), col = 'grey', cex = 1, pch = 16)
points(jitter(rep$age[rep$bfeed == 1]), jitter(rep$tot[rep$bfeed == 1]), col = 'black', cex = 1, pch = 16)

rep$lws[rep$bfeed == 0] <-  predict(loess(rep$tot[rep$bfeed == 0] ~ rep$age[rep$bfeed == 0], span = 0.95), subset(rep, bfeed == 0))
rep$lws[rep$bfeed == 1] <-  predict(loess(rep$tot[rep$bfeed == 1] ~ rep$age[rep$bfeed == 1], span = 0.95), subset(rep, bfeed == 1))
lines(rep$age[rep$bfeed == 0], rep$lws[rep$bfeed == 0], col = 'grey', lwd = 2)
lines(rep$age[rep$bfeed == 1], rep$lws[rep$bfeed == 1], col = 'black', lwd = 2)

legend('topleft',c("No infant","Breastfeeding"), pch=c(16, 16), col=c("grey", "black"), box.lwd = 0, bty = "n", cex = 1.5) # legend to explain the plot.
dev.off()




	# TRACKING.
png("track_agesex.png", width=500, height=500, pointsize=12)

plot(NULL, xlim = c(17, max(TRK$age, na.rm=T)), ylim = c(0, max(TRK$net, na.rm=T)), xlab = "", ylab = "", main = "", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
TRK <- TRK[order(TRK$age),]
points(jitter(TRK$age[TRK$male == 0]), jitter(TRK$net[TRK$male == 0]), col = 'grey', cex = 1, pch = 16)
points(jitter(TRK$age[TRK$male == 1]), jitter(TRK$net[TRK$male == 1]), col = 'black', cex = 1, pch = 16)

TRK$lws[TRK$male == 0] <-  predict(loess(TRK$net[TRK$male == 0] ~ TRK$age[TRK$male == 0], span = 0.95), subset(TRK, male == 0))
TRK$lws[TRK$male == 1] <-  predict(loess(TRK$net[TRK$male == 1] ~ TRK$age[TRK$male == 1], span = 0.95), subset(TRK, male == 1))
lines(TRK$age[TRK$male == 0], TRK$lws[TRK$male == 0], col = 'grey', lwd = 2)
lines(TRK$age[TRK$male == 1], TRK$lws[TRK$male == 1], col = 'black', lwd = 2)

dev.off()

