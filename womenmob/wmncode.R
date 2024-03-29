setwd("~/GitHub/anthro")

######
######
######

inf <- read.csv("womenmob/data/tweinfo.csv", strip.white = T)
mr <- read.csv("womenmob/data/MR_bytrial.csv", strip.white = T)
rep <- read.csv("womenmob/data/repro.csv", strip.white = T)
corsi <- read.csv("womenmob/data/twecorsi.csv", strip.white = T)
psp <- read.csv("womenmob/data/persp_err.csv", strip.white = T)
pnt <- read.csv("womenmob/data/pnt_byrow.csv", strip.white = T)
	pnt <- pnt[c(1:2, 5, 10:11)]
	pnt$Bearing_Difference <- abs(pnt$Bearing_Difference)

# PREPROCESSING
	#MENTAL ROTATION DATA
	mr <- subset(mr, RT <= 30)
	mr <- mr[c(1, 5:6)]
	mr <- aggregate( . ~ Subject, data = mr, mean)
	colnames(mr)[1] <- "ID"

	#PERSPECTIVE TAKING DATA
	psp <- merge(inf, psp, by="ID")
	for(i in 1:nrow(psp)){
		psp$avg[i] <- ifelse(is.na(mean(as.numeric(abs(psp[i, 9:20])))), NA, mean(as.numeric(abs(psp[i, 9:20]))))
	}

	#POINTING DATA
	hld <- merge(pnt, inf, by="ID", all.x = T)
	# First step is to remove distance component then calculate an average score for individuals.
	hld$Distance_Meters <- scale(hld$Distance_Meters)
		hld$Distance_Meters <- hld$Distance_Meters - min(hld$Distance_Meters, na.rm=T)
	hld$Bearing_Difference <- scale(hld$Bearing_Difference)
		hld$Bearing_Difference <- hld$Bearing_Difference - min(hld$Bearing_Difference, na.rm=T)	
	library(lme4)
	dist <- lmer(Bearing_Difference ~ Distance_Meters
		     + (Distance_Meters | ID), data = hld)
	hld$Bearing_Difference <- hld$Bearing_Difference - fixef(dist)[2]*hld$Distance_Meters
		#	dist <- lm(Bearing_Difference ~ Distance_Meters, hlda = hld)
		#	hld$Bearing_Difference <- hld$Bearing_Difference - coef(dist)[2]*hld$Distance_Meters
	hld <- hld[,c(1,4)]
	hld <- aggregate( . ~ ID, data = hld, mean)
	colnames(hld)[2] <- "pnt.err" 

dat <- merge(inf[1:4], mr, by = "ID", all.x=T)
dat <- merge(dat, cog[,c(1,5)], by = "ID", all.x=T)
dat <- merge(dat, rep[c(1,4)], by = "ID", all.x=T)
dat <- merge(dat, psp[c(1, ncol(psp))], by = "ID", all.x=T)
dat <- merge(dat, hld, by = "ID", all.x=T)

dat <- subset(dat, male == 0 & age <= 50)

m1 <- lm(scale(Acc) ~ bfeed, data = dat)

m1 <- lm(I(scale(Acc) - scale(RT) + scale(span)) ~ bfeed, data = dat)


library("lavaan")

model <- '
	# measurement model
	space =~ Acc + RT + span
	# regression
	bfeed ~ space'
fit <- sem(model, data = dat)
summary(fit, standardized = TRUE)








dat <- subset(dat, male == 0 & age <= 45)








acc_bydeg <- c()
dx <- 1
for(i in c(sort(unique(mr$Degree)))){
	acc_bydeg[dx] <- mean(mr$Acc[mr$Degree == i], na.rm=T)
	dx <- dx + 1
}
rt_bydeg <- c()
dx <- 1
for(i in c(sort(unique(mr$Degree)))){
	rt_bydeg[dx] <- mean(mr$RT[mr$Degree == i], na.rm=T)
	dx <- dx + 1
}


png("acc_deg.png", height = 500, width = 500)
plot(acc_bydeg, xlab = "Degree Rotation", ylab = "% Correct", main = "", xaxt="n", pch = 16, font=2, font.lab = 2)
axis(1, at=1:length(unique(mr$Degree)), labels=sort(unique(mr$Degree)), font = 2)
dev.off()

png("rt_deg.png", height = 500, width = 500)
plot(rt_bydeg, xlab = "Degree Rotation", ylab = "Response time (seconds)", main = "", xaxt="n", pch = 16, font=2, font.lab = 2)
axis(1, at=1:length(unique(mr$Degree)), labels=sort(unique(mr$Degree)), font = 2)
dev.off()







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

dat <- merge(inf, rep, by="ID" )
dat <- merge(dat, raw, by="ID")
	dat <- subset(dat, male == 0 & age < 45)

dat <- merge(inf, rep, by="ID", all.x =T)
dat <- merge(dat, mob, by="ID", all.x =T)
dat <- merge(dat, cog, by="ID", all.x =T)
dat <- merge(dat, psp[c(1,ncol(psp))], by="ID", all.x =T)
	dat <- subset(dat, male == 0)
dat$span <- scale(dat$span)
dat$mr.acc <- scale(dat$mr.acc)
dat$mr.time <- scale(dat$mr.time)
dat$avg <- scale(dat$avg)

for(i in 1:nrow(dat)){
	dat$space[i] <- mean(c(dat$span[i], dat$mr.acc[i], -dat$mr.time[i], -dat$avg[i]), na.rm=T)
}

TRK <- merge(trk, inf, by= "ID", all.x = TRUE)
TRK <- merge(TRK, rep, by= "ID", all.x = TRUE)

psp <- merge(inf, psp, by="ID")
	for(i in 1:nrow(psp)){
		psp$avg[i] <- ifelse(is.na(mean(as.numeric(abs(psp[i, 9:20])))), NA, mean(as.numeric(abs(psp[i, 9:20]))))
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









dat <- merge(inf, rep, by="ID", all.x =T)
dat <- merge(dat, mob, by="ID", all.x =T)
dat <- merge(dat, cog, by="ID", all.x =T)
library("lavaan")


MR <- scale(scale(dat$mr.acc[!is.na(dat$mr.acc) & !is.na(dat$span)]) - scale(dat$mr.time[!is.na(dat$mr.acc) & !is.na(dat$span)]))
CR <- scale(dat$span[!is.na(dat$mr.acc) & !is.na(dat$span)])
