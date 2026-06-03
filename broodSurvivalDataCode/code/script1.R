
#packages
library(jagsUI)

#set working directory to load in jags data
setwd('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/data/')

#read in jags data
jags.data1 <- readRDS("jags.data1.rds")

#set working directory to read in jags scripts
setwd('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/code')

#parameters monitored
parameters <- c("alpha", "beta", "beta1", "sigmaID")

#mcmc settings
ni <- 50000
nt <- 10
nb <- 20000
nc <- 4

#initial values
inits <- function(){list()}  

#fixed effects model
m1 <- jags(jags.data1, 
           inits, 
           parameters, 
           "jagsScript1.jags", 
           n.chains = nc, 
           n.thin = nt, 
           n.iter = ni, 
           n.burnin = nb, 
           parallel = TRUE)

#null model
m1Null <- jags(jags.data1, 
               inits, 
               parameters, 
               "jagsScript1Null.jags", 
               n.chains = nc, 
               n.thin = nt, 
               n.iter = ni, 
               n.burnin = nb)

#view results
plogis(summary(m1, digits = 3))
summary(m1Null, digits = 3)

#difference in sd
m1$mean$sigmaID
m1Null$mean$sigmaID

#save r environment 
save.image("C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/output/enviroments/script1Enviroment.RData")



