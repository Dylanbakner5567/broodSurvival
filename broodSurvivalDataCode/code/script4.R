
#packages
library(jagsUI)
library(lubridate)
library(scales)
library(dplyr)

#load script environments
load("C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/output/enviroments/script1Enviroment.RData")
load("C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/output/enviroments/script2Enviroment.RData")
load("C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/output/enviroments/script3Enviroment.RData")

#how many brooding females
yCopy <- readRDS('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/data/y.rds')
nrow(yCopy)

#across what years
patchSummary <- readRDS('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/data/patchSummary.rds')
patchSummary$formDate<-ymd(patchSummary$date_only)
patchSummary$formDate<-year(patchSummary$date_only)
min(unique(patchSummary$formDate)); max(unique(patchSummary$formDate))

#how many brood were successful
nrow(yCopy)-sum(unlist(as.vector(yCopy)) == 3, na.rm=T)
sum(unlist(as.vector(yCopy)) == 3, na.rm=T)
nrow(yCopy)-sum(unlist(as.vector(yCopy)) == 3, na.rm=T)+sum(unlist(as.vector(yCopy)) == 3, na.rm=T)
round((nrow(yCopy)-sum(unlist(as.vector(yCopy)) == 3, na.rm=T))/nrow(yCopy), digits = 3)
round((sum(unlist(as.vector(yCopy)) == 3, na.rm=T))/nrow(yCopy), digits = 3)

#female mortality
y <- readRDS('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/data/y.rds')
y2 <- readRDS('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/data/y2.rds')
nrow(y)-sum(unlist(as.vector(y2)) == 3, na.rm=T)
sum(unlist(as.vector(y2)) == 3, na.rm=T)
nrow(y2)-sum(unlist(as.vector(y2)) == 3, na.rm=T)+sum(unlist(as.vector(y2)) == 3, na.rm=T)
round((nrow(y2)-sum(unlist(as.vector(y2)) == 3, na.rm=T))/nrow(y2), digits = 3)
round((sum(unlist(as.vector(y2)) == 3, na.rm=T))/nrow(y2), digits = 3)

#how many laying sites did broods revisit?
totPatch <- patchSummary %>% select(ID_Attempt, Overall_Patches) %>% distinct()
round(mean(totPatch$Overall_Patches), digits = 1) 
round(sd(totPatch$Overall_Patches),digits = 1)
median(totPatch$Overall_Patches)
min(totPatch$Overall_Patches) 
max(totPatch$Overall_Patches)

#how many laying sites did broods revisit daily?
round(mean(patchSummary$Total_Patches_Daily), digits = 1) 
round(sd(patchSummary$Total_Patches_Daily),digits = 1)
median(patchSummary$Total_Patches_Daily)
min(patchSummary$Total_Patches_Daily) 
max(patchSummary$Total_Patches_Daily)

#sd shrikage
round(plogis(m1$mean$sigmaID), digits = 2);round(plogis(m1$q2.5$sigmaID), digits = 2);round(plogis(m1$q97.5$sigmaID), digits = 2)
round(plogis(m1Null$mean$sigmaID), digits = 2);round(plogis(m1Null$q2.5$sigmaID), digits = 2);round(plogis(m1Null$q97.5$sigmaID), digits = 2)

#effect of brood day
load("C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/output/enviroments/script1Enviroment.RData")
summary(m1)
round((m1$mean$beta),digits = 2);round((m1$q2.5$beta),digits = 2);round((m1$q97.5$beta),digits = 2)

#effect of total daily distance
round((m1$mean$beta1),digits = 2);round((m1$q2.5$beta1),digits = 2);round((m1$q97.5$beta1),digits = 2)

#revisitation probability 
day_map <- data.frame(day = jags.data1$day, dayS = jags.data1$dayS) %>% distinct() %>% arrange(day)
alpha_samp <- m1$sims.list$alpha
beta_samp  <- m1$sims.list$beta
dat <- do.call(rbind, lapply(1:nrow(day_map), function(i) {
  p <- plogis(alpha_samp + beta_samp * day_map$dayS[i])
  data.frame(day = day_map$day[i],
             mean = mean(p),
             lwr = quantile(p, 0.025),
             upr = quantile(p, 0.975))}))
dat[1,];dat[28,]

#mean brood survival
stateDays <- apply(jags.data2$y, 2, function(x) {
  names(which.max(table(factor(x, levels = 1:3))))
})
quantile((apply(m2$sims.list$phiA[, 1:2], 1, prod) * apply(m2$sims.list$phiB[, 3:27], 1, prod)), c(0.025,0.5,0.975), digits=3)


#prior to tree roosting survival
revizG <- apply(m2$sims.list$phiA[, 1:9], 1, prod)
newG <- apply(m2$sims.list$phiB[, 1:9], 1, prod)
round(quantile(revizG, c(0.025,0.5,0.975)),digits=3)
round(quantile(newG, c(0.025,0.5,0.975)),digits=3)

#transition period
revizT <- apply(m2$sims.list$phiA[, 10:14], 1, prod)
newT <- apply(m2$sims.list$phiB[, 10:14], 1, prod)
round(quantile(revizT, c(0.025,0.5,0.975)),digits=3)
round(quantile(newT, c(0.025,0.5,0.975)),digits=3)

#prior to tree roosting survival
revizTree <- apply(m2$sims.list$phiA[, 15:27], 1, prod)
newTree <- apply(m2$sims.list$phiB[, 15:27], 1, prod)
round(quantile(revizTree, c(0.025,0.5,0.975)),digits=3)
round(quantile(newTree, c(0.025,0.5,0.975)),digits=3)

#dsp spending 0 days in reviz or 10
#project brood survival for some combinations of interest
dsp0 <- data.frame(prob = apply(m2$sims.list$phiB[, 1:27], 1, prod), day = 0)
dsp10 <- data.frame(prob = apply(m2$sims.list$phiA[, 1:10], 1, prod) * apply(m2$sims.list$phiB[, 11:27], 1, prod), day = 10)
dsp0 <- quantile(dsp0$prob, c(0.025,0.5,0.975))
dsp10 <- quantile(dsp10$prob, c(0.025,0.5,0.975))
round(dsp0,digits=3)
round(dsp10,digits=3)

#mean female survival
stateDays
quantile((apply(m3$sims.list$phiA[, 1:2], 1, prod) * apply(m3$sims.list$phiB[, 3:27], 1, prod)), c(0.025,0.5,0.975), digits=3)

#prior to tree roosting survival
revizG <- apply(m3$sims.list$phiA[, 1:9], 1, prod)
newG <- apply(m3$sims.list$phiB[, 1:9], 1, prod)
round(quantile(revizG, c(0.025,0.5,0.975)),digits=3)
round(quantile(newG, c(0.025,0.5,0.975)),digits=3)

#transition period
revizT <- apply(m3$sims.list$phiA[, 10:14], 1, prod)
newT <- apply(m3$sims.list$phiB[, 10:14], 1, prod)
round(quantile(revizT, c(0.025,0.5,0.975)),digits=3)
round(quantile(newT, c(0.025,0.5,0.975)),digits=3)

#tree roosting survival
revizTree <- apply(m3$sims.list$phiA[, 15:27], 1, prod)
newTree <- apply(m3$sims.list$phiB[, 15:27], 1, prod)
round(quantile(revizTree, c(0.025,0.5,0.975)),digits=3)
round(quantile(newTree, c(0.025,0.5,0.975)),digits=3)

#dsp spending 0 days in reviz or 10
#project brood survival for some combinations of interest
dsp0 <- data.frame(prob = apply(m3$sims.list$phiB[, 1:27], 1, prod), day = 0)
dsp10 <- data.frame(prob = apply(m3$sims.list$phiA[, 1:10], 1, prod) * apply(m3$sims.list$phiB[, 11:27], 1, prod), day = 10)
dsp0 <- quantile(dsp0$prob, c(0.025,0.5,0.975))
dsp10 <- quantile(dsp10$prob, c(0.025,0.5,0.975))
round(dsp0,digits=3)
round(dsp10,digits=3)