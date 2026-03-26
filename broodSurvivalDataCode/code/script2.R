
#packages
library(jagsUI)

#set working directory to load in jags data below
setwd('/data/')

#read in jags data
jags.data2 <- readRDS("jags.data2.rds")

#set working directory to read in jags scripts
setwd('/code/')

#function to create initial values for unknown z
ms.init.z <- function(ch, f){
  for (i in 1:dim(ch)[1]){ch[i,1:f[i]] <- NA}
  states <- max(ch, na.rm = TRUE)
  known.states <- 1:(states-1)
  v <- which(ch==states)
  ch[-v] <- NA
  ch[v] <- sample(known.states, length(v), replace = TRUE)
  return(ch)
}

#initial values
inits <- function(){list(mean.phi = runif(1, 0, 1), mean.psi = runif(2, 0, 1), z = ms.init.z(jags.data2$y,jags.data2$f))}  

#parameters monitored
parameters <- c("phiA", "phiB", "psiAB", "psiBA")

#mcmc settings
ni <- 50000
nt <- 10
nb <- 20000
nc <- 4

#call jags from r
m2 <- jags(jags.data2, inits, parameters, "jagsScript2.jags", n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
print(m2, digits = 3)
