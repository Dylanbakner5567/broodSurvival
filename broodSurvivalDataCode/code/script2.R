
#packages
library(jagsUI)

#set working directory to load in jags data
setwd('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/data/')

#read in jags data
jags.data2 <- readRDS("jags.data2.rds")

#set working directory to read in jags scripts
setwd('C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/code')

#initial values
inits <- function(){list()}  

#parameters monitored
parameters <- c("phiA", "phiB", "psiAB", "psiBA")

#mcmc settings
ni <- 50000
nt <- 10
nb <- 20000
nc <- 4

#call jags from r
m2 <- jags(jags.data2, inits, parameters, "jagsScript2.txt", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, parallel = TRUE)
print(m2, digits = 3)

#save r environment 
save.image("C:/Users/dylan.bakner/Documents/manuscripts/turkeyBroodSurvival/submissionDocs/output/enviroments/script2Enviroment.RData")

