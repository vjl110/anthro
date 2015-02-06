setwd("~/GitHub/anthro")


inf <- read.csv("womenmob/data/tweinfo.csv", strip.white = T)
psp <- read.csv("womenmob/data/persp_err.csv", strip.white = T)
	for(i in 1:nrow(psp)){
		psp$psp_avg[i] <- mean(abs(as.numeric(psp[i, 3:14])), na.rm=T)
	}
	psp <- psp[-c(2:14)]
mob <- read.csv("womenmob/data/yrmob.csv", strip.white = T)
ltmob <- read.csv("womenmob/data/ltmob.csv", strip.white = T)
	for(i in 1:nrow(ltmob)){
		ltmob$lt_avg[i] <- mean(as.numeric(ltmob[i, 2:21]), na.rm=T)
	}
	ltmob <- ltmob[-c(2:21)]
cog <- read.csv("womenmob/data/cog.csv", strip.white = T)
	cog <- cog[-c(2:4, 6)]
srvy <- read.csv("womenmob/data/survey.csv", strip.white = T)
	for(i in 1:nrow(srvy)){
		srvy$ha[i] <- (1 - mean(c(srvy$HA1[i], srvy$HA2[i], srvy$HA3[i], srvy$HA4[i], srvy$HA5[i]
				    , srvy$HA6[i], srvy$HA7[i], srvy$HA8[i], srvy$HA9[i], srvy$HA10[i]
				    ), na.rm=T))   
		srvy$sab[i] <- 4 - mean(c(srvy$SAB1b[i], srvy$SAB2[i], srvy$SAB3[i]), na.rm=T)   
		srvy$sax[i] <- 4 - mean(c(srvy$SAX1[i], srvy$SAX2[i], srvy$SAX3[i], (4 - srvy$SAX4[i])), na.rm=T)   	
	}
	srvy <- srvy[c(1,31:33)]
rep <- read.csv("womenmob/data/repro.csv", strip.white = T)
	rep <- rep[c(1, 4)]
trk <- read.csv("womenmob/data/trackdat.csv", strip.white = T)
	trk <- trk[-c(2:3)]
	trk <- aggregate( . ~ ID, data = trk, mean)	
pnt <- read.csv("womenmob/data/pnt_byrow.csv", strip.white = T)
	pnt <- pnt[c(1:2, 5, 10:11)]
	pnt$Bearing_Difference <- abs(pnt$Bearing_Difference)




###   Perspective by age and sex.
dat <- merge(inf, psp, by="ID")

png("prspage.png", width=500, height=500, pointsize=12)

plot(NULL, xlim = c(17, 75), ylim = c(0, 150), xlab = "Age", ylab = "Error (degrees)", main = "", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$male == 0]), jitter(dat$psp_avg[dat$male == 0]), col = 'salmon', cex = 1, pch = 16)
points(jitter(dat$age[dat$male == 1]), jitter(dat$psp_avg[dat$male == 1]), col = 'blue', cex = 1, pch = 16)

dat$lws[dat$male == 0] <-  predict(loess(dat$psp_avg[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), subset(dat, male == 0))
dat$lws[dat$male == 1] <-  predict(loess(dat$psp_avg[dat$male == 1] ~ dat$age[dat$male == 1], span = 0.95), subset(dat, male == 1))
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'salmon', lwd = 2)
lines(dat$age[dat$male == 1], dat$lws[dat$male == 1], col = 'blue', lwd = 2)

dev.off()

m1 <- lm(psp_avg ~ male, data = dat)
summary(m1)
#####    Results?  Not much interesting patterning by age.  B of "male" is -0.31...	



dat <- merge(dat, mob, by = "ID", all.x = T)

m1 <- lm(tot ~ psp_avg, data = subset(dat, male == 0))

####  Weirdly...  there is a significant negative relationship between perspective accuracy and mobility with women....  This blends with the respective age trends..  I assume is not causal.
dat <- merge(inf, cog, by = "ID", all.x=T)



dat <- merge(dat, cog, by = "ID", all.x = T)

m1 <- lm(psp_avg ~ mr.acc, data = dat) # nope
m1 <- lm(psp_avg ~ mr.acc, data = dat) # yup...  -0.27... but -0.18 and non-sig after controling for sex


###########

dat <- merge(inf, srvy, by = "ID", all.x = T)
dat <- merge(dat, mob, by = "ID")
dat <- merge(dat, ltmob, by = "ID")
dat <- merge(dat, trk, by = "ID")
dat <- merge(dat, cog, by = "ID")
dat <- merge(dat, psp, by = "ID")



### Harm avoidance by age and reproductive status
dat <- merge(inf, rep, by="ID")
dat <- merge(dat, srvy, by = "ID")
dat <- subset(dat, male == 0 & age <= 40)

png("enrepage.png", width=500, height=500, pointsize=12)

plot(NULL, xlim = c(17, 40), ylim = c(1, 5), xlab = "Age", ylab = "", main = "", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$bfeed == 0]), jitter(dat$sax[dat$bfeed == 0]), col = 'grey', cex = 1, pch = 16)
points(jitter(dat$age[dat$bfeed == 1]), jitter(dat$sax[dat$bfeed == 1]), col = 'black', cex = 1, pch = 16)

dat$lws[dat$bfeed == 0] <-  predict(loess(dat$sax[dat$bfeed == 0] ~ dat$age[dat$bfeed == 0], span = 0.95), subset(dat, bfeed == 0))
dat$lws[dat$bfeed == 1] <-  predict(loess(dat$sax[dat$bfeed == 1] ~ dat$age[dat$bfeed == 1], span = 0.95), subset(dat, bfeed == 1))
lines(dat$age[dat$bfeed == 0], dat$lws[dat$bfeed == 0], col = 'grey', lwd = 2)
lines(dat$age[dat$bfeed == 1], dat$lws[dat$bfeed == 1], col = 'black', lwd = 2)

legend('bottomright',c("Female","Male"), pch=c(15, 15), col=c("grey", "black"), box.lwd = 0, bty = "n", cex = 1.5) # legend to explain the plot.
dev.off()

m1 <- lm(psp_avg ~ male, data = dat)
summary(m1)
#####    Results?  SAMPLE TOO SMALL... not very intresting



### cognition and mobility
dat <- merge(inf, cog, by="ID")
dat <- merge(dat, psp, by = "ID")
dat <- merge(dat, mob, by = "ID")

d <- na.omit(data.frame(scale(dat$net), scale(dat$tot), scale(dat$lt_avg)))
cor(d) # get correlations (returns matrix)



### pointing and....		SEX DIFF IS STRONGER IF CONTROL FOR DISTANCE... maybe when random effecting point....
dat <- merge(pnt, inf, by="ID", all.x = T)
#m1 <- lm(scale(Bearing_Difference) ~ scale(male), data = dat)

#library(lme4)
#m1 <- lmer(scale(Bearing_Difference) ~ scale(male)
#	   + (1|PoO) + (1|to), data = dat)

	# First step is to remove distance component then calculate an average score for individuals.
	dat$Distance_Meters <- scale(dat$Distance_Meters)
		dat$Distance_Meters <- dat$Distance_Meters - min(dat$Distance_Meters, na.rm=T)
	dat$Bearing_Difference <- scale(dat$Bearing_Difference)
		dat$Bearing_Difference <- dat$Bearing_Difference - min(dat$Bearing_Difference, na.rm=T)	
	library(lme4)
	dist <- lmer(Bearing_Difference ~ Distance_Meters
		     + (Distance_Meters | ID), data = dat)
	dat$Bearing_Difference <- dat$Bearing_Difference - fixef(dist)[2]*dat$Distance_Meters
		#	dist <- lm(Bearing_Difference ~ Distance_Meters, data = dat)
		#	dat$Bearing_Difference <- dat$Bearing_Difference - coef(dist)[2]*dat$Distance_Meters
	dat <- dat[,c(1,4)]
	dat <- aggregate( . ~ ID, data = dat, mean)	
dat <- merge(inf, dat, by="ID", all.x = T)



dat <- merge(dat, cog, by = "ID", all.x = T)
dat <- merge(dat, psp, by = "ID", all.x = T)

dat <- merge(dat, mob, by = "ID", all.x = T)
dat <- merge(dat, ltmob, by="ID", all.x = T)
dat <- merge(dat, trk, by="ID", all.x = T)


##########


### MOBILITY ALL 3
dat <- merge(inf, mob, by="ID", all.x = T)
dat <- merge(dat, ltmob, by="ID", all.x = T)
dat <- merge(dat, trk, by="ID", all.x = T)

m1 <- lm(scale(tot) ~ scale(male), data = dat)

dat <- merge(dat, srvy, by="ID", all.x = T)


### tracking and....
dat <- merge(inf, trk, by="ID")
dat <- merge(dat, mob, by = "ID")

png("tracking.png", width=500, height=500, pointsize=12)

plot(NULL, xlim = c(17, 80), ylim = c(0, 25000), xlab = "Age", ylab = "Net daily travel (km)", main = "", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$male == 0]), jitter(dat$net[dat$male == 0]), col = 'salmon', cex = 1, pch = 16)
points(jitter(dat$age[dat$male == 1]), jitter(dat$net[dat$male == 1]), col = 'blue', cex = 1, pch = 16)

dat$lws[dat$male == 0] <-  predict(loess(dat$net[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), subset(dat, male == 0))
dat$lws[dat$male == 1] <-  predict(loess(dat$net[dat$male == 1] ~ dat$age[dat$male == 1], span = 0.95), subset(dat, male == 1))
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'salmon', lwd = 2)
lines(dat$age[dat$male == 1], dat$lws[dat$male == 1], col = 'blue', lwd = 2)

#legend('topright',c("Female","Male"), pch=c(15, 15), col=c("grey", "black"), box.lwd = 0, bty = "n", cex = 1.5) # legend to explain the plot.
dev.off()

m1 <- lm(trk ~ male, data = dat)
summary(m1)







###PCA

# Pricipal Components Analysis
# entering raw data and extracting PCs
# from the correlation matrix
HA <- srvy[,21:30]
HA <- HA[,1:5]
HA <- na.omit(HA)

fit <- princomp(dat, cor=TRUE)
summary(fit) # print variance accounted for
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit) 


# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors,
# with varimax rotation
fit <- factanal(HA, 3, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(HA),cex=.7) # add variable names 



# Determine Number of Factors to Extract
library(nFactors)
ev <- eigen(cor(HA)) # get eigenvalues
ap <- parallel(subject=nrow(HA),var=ncol(HA),
  rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 














### Harm avoidance by age and reproductive status
dat <- merge(inf, rep, by="ID", all.x =T)
dat <- merge(dat, mob, by="ID", all.x =T)
dat <- merge(dat, ltmob, by="ID", all.x =T)
dat <- merge(dat, trk, by="ID", all.x =T)
dat <- merge(dat, srvy, by="ID", all.x =T)

png("justwom.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(17, 75), ylim = c(0, 8), xlab = "Age", ylab = "Unique laces visited in past year", main = "Women only", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$male == 0]), jitter(dat$tot[dat$male == 0]), col = 'black', cex = 1, pch = 16)
dat$lws[dat$male == 0] <-  predict(loess(dat$tot[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), dat[dat$male == 0,])
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'black', lwd = 2)
dev.off()

# add in men
png("menwom.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(17, 75), ylim = c(0, 20), xlab = "Age", ylab = "Unique laces visited in past year", main = "Men vs. Women", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$male == 0]), jitter(dat$tot[dat$male == 0]), col = 'salmon', cex = 1, pch = 16)
points(jitter(dat$age[dat$male == 1]), jitter(dat$tot[dat$male == 1]), col = 'blue', cex = 1, pch = 16)

dat$lws[dat$male == 0] <-  predict(loess(dat$tot[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), subset(dat, male == 0))
dat$lws[dat$male == 1] <-  predict(loess(dat$tot[dat$male == 1] ~ dat$age[dat$male == 1], span = 0.95), subset(dat, male == 1))
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'salmon', lwd = 2)
lines(dat$age[dat$male == 1], dat$lws[dat$male == 1], col = 'blue', lwd = 2)
dev.off()

# now the breastfeeding....
png("bfeed_mob.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(17, 45), ylim = c(0, 8), xlab = "Age", ylab = "Unique laces visited in past year", main = "Breast-feeding vs. not", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45]), jitter(dat$tot[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45]), col = 'black', cex = 1, pch = 16)
points(jitter(dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45]), jitter(dat$tot[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45]), col = 'red', cex = 1, pch = 16)

dat$lws[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45] <-  predict(loess(dat$tot[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45] ~ dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], span = 0.95), subset(dat, bfeed == 0 & male == 0 & age <= 45))
dat$lws[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45] <-  predict(loess(dat$tot[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45] ~ dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], span = 0.95), subset(dat, bfeed == 1 & male == 0 & age <= 45))
lines(dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], dat$lws[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], col = 'black', lwd = 2)
lines(dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], dat$lws[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], col = 'red', lwd = 2)

legend('topleft',c("No","Yes"), pch=c(15, 15), col=c("black", "red"), box.lwd = 0, bty = "n", cex = 1.5) # legend to explain the plot.
dev.off()



#####
##########
###		ANXIETY
#########
### Harm avoidance by age and reproductive status

png("justwom.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(17, 85), ylim = c(1, 4), xlab = "Age", ylab = "", main = "Women only", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(dat$age[dat$male == 0], dat$sax[dat$male == 0], col = 'black', cex = 1, pch = 16)
dat$lws[dat$male == 0] <-  predict(loess(dat$sax[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), dat[dat$male == 0,])
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'black', lwd = 2)
dev.off()

# add in men
png("menwom.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(17, 85), ylim = c(0.5, 4), xlab = "Age", ylab = "", main = "Men vs. Women", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$male == 0]), jitter(dat$sax[dat$male == 0]), col = 'salmon', cex = 1, pch = 16)
points(jitter(dat$age[dat$male == 1]), jitter(dat$sax[dat$male == 1]), col = 'blue', cex = 1, pch = 16)

dat$lws[dat$male == 0] <-  predict(loess(dat$sax[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), subset(dat, male == 0))
dat$lws[dat$male == 1] <-  predict(loess(dat$sax[dat$male == 1] ~ dat$age[dat$male == 1], span = 0.95), subset(dat, male == 1))
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'salmon', lwd = 2)
lines(dat$age[dat$male == 1], dat$lws[dat$male == 1], col = 'blue', lwd = 2)
dev.off()

# now the breastfeeding....
png("bfeed.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(17, 45), ylim = c(2, 3.5), xlab = "Age", ylab = "", main = "Breast-feeding vs. not", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], dat$sax[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], col = 'grey', cex = 1, pch = 16)
points(dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], dat$sax[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], col = 'black', cex = 1, pch = 16)

dat$lws[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45] <-  predict(loess(dat$sax[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45] ~ dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], span = 0.85), subset(dat, bfeed == 0 & male == 0 & age <= 45))
dat$lws[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45] <-  predict(loess(dat$sax[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45] ~ dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], span = 0.95), subset(dat, bfeed == 1 & male == 0 & age <= 45))
lines(dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], dat$lws[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], col = 'grey', lwd = 2)
lines(dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], dat$lws[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], col = 'black', lwd = 2)

legend('topright',c("No","Yes"), pch=c(15, 15), col=c("grey", "black"), box.lwd = 0, bty = "n", cex = 1.5) # legend to explain the plot.
dev.off()






# now the breastfeeding....
png("bfeed.png", width=500, height=500, pointsize=12)
plot(NULL, xlim = c(17, 45), ylim = c(0, 8), xlab = "Age", ylab = "", main = "Breast-feeding vs. not", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45]), jitter(dat$alone[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45]), col = 'grey', cex = 1, pch = 16)
points(jitter(dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45]), jitter(dat$alone[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45]), col = 'black', cex = 1, pch = 16)

dat$lws[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45] <-  predict(loess(dat$alone[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45] ~ dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], span = 0.85), subset(dat, bfeed == 0 & male == 0 & age <= 45))
dat$lws[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45] <-  predict(loess(dat$alone[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45] ~ dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], span = 0.95), subset(dat, bfeed == 1 & male == 0 & age <= 45))
lines(dat$age[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], dat$lws[dat$bfeed == 0 & dat$male == 0 & dat$age <= 45], col = 'grey', lwd = 2)
lines(dat$age[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], dat$lws[dat$bfeed == 1 & dat$male == 0 & dat$age <= 45], col = 'black', lwd = 2)

legend('topleft',c("No","Yes"), pch=c(15, 15), col=c("grey", "black"), box.lwd = 0, bty = "n", cex = 1.5) # legend to explain the plot.
dev.off()










plot(NULL, xlim = c(17, 85), ylim = c(0, 1), xlab = "Age", ylab = "", main = "Women only", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(dat$age[dat$male == 0], dat$alone[dat$male == 0], col = 'black', cex = 1, pch = 16)
dat$lws[dat$male == 0] <-  predict(loess(dat$alone[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), dat[dat$male == 0,])
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'black', lwd = 2)

# add in men
plot(NULL, xlim = c(17, 85), ylim = c(0, 1), xlab = "Age", ylab = "", main = "Men vs. Women", font = 2, font.lab = 2, cex.lab = 1.5, cex.main = 1.5)
dat <- dat[order(dat$age),]
points(jitter(dat$age[dat$male == 0]), jitter(dat$ha[dat$male == 0]), col = 'salmon', cex = 1, pch = 16)
points(jitter(dat$age[dat$male == 1]), jitter(dat$ha[dat$male == 1]), col = 'blue', cex = 1, pch = 16)

dat$lws[dat$male == 0] <-  predict(loess(dat$ha[dat$male == 0] ~ dat$age[dat$male == 0], span = 0.95), subset(dat, male == 0))
dat$lws[dat$male == 1] <-  predict(loess(dat$ha[dat$male == 1] ~ dat$age[dat$male == 1], span = 0.95), subset(dat, male == 1))
lines(dat$age[dat$male == 0], dat$lws[dat$male == 0], col = 'salmon', lwd = 2)
lines(dat$age[dat$male == 1], dat$lws[dat$male == 1], col = 'blue', lwd = 2)

