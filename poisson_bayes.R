library(COUNT)
library(rjags)

data("badhealth")
head(badhealth)

mod1_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

mod2_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

data_jags = as.list(badhealth)

params1 = c("int", "b_badh", "b_age", "b_intx")
params2 = c("int", "b_badh", "b_age")

mod1 = jags.model(textConnection(mod1_string), data=data_jags, n.chains=3)
update(mod1, 1e3)
mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod1_sim = coda.samples(model=mod1, variable.names=params1, n.iter=5e3)
mod1_csim = as.mcmc(do.call(rbind, mod1_sim))
mod2_sim = coda.samples(model=mod2, variable.names=params2, n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

gelman.diag(mod1_sim)
gelman.diag(mod2_sim)

dic1 = dic.samples(mod1, n.iter=1e3)
dic2 = dic.samples(mod2, n.iter=1e3)

x1 = c(0, 35, 0) # good health
x2 = c(1, 35, 35) # bad health

loglam1 = mod1_csim[,"int"] + mod1_csim[,c(2,1,3)] %*% x1
loglam2 = mod1_csim[,"int"] + mod1_csim[,c(2,1,3)] %*% x2
lam1 = exp(loglam1)
lam2 = exp(loglam2)

n_sim = length(lam1)
y1 = rpois(n=n_sim, lambda=lam1)
y2 = rpois(n=n_sim, lambda=lam2)
mean(y2 > y1)

## question 2
ppois(21, 30)
setwd('/storage/Code/r/simdat')
dat = read.csv(file="callers.csv", header=TRUE)
head(dat)
str(dat)
plot(calls/days_active ~ isgroup2, data = dat, subset = isgroup2==0)
plot(calls/days_active ~ isgroup2, data = dat, subset = isgroup2==1, add=T)

mod3_string <- " model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois(lam[i] * days_active[i])
        log(lam[i]) = b0 + b.age * age[i] + b.group2 * isgroup2[i]
    }
    
    b0 ~ dnorm(0.0, 1/100)
    b.age ~ dnorm(0.0, 1/100)
    b.group2 ~ dnorm(0.0, 1/100)
} "

data3_jags <- as.list(dat)
params3 <- c('b0', 'b.age', 'b.group2')
mod3 <- jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)
mod3_sim <- coda.samples(model=mod3, variable.names=params3, n.iter=1e4)
gelman.diag(mod3_sim)

mod3_csim <- as.mcmc(do.call(rbind, mod3_sim))
head(mod3_csim)
mean(mod3_csim[,2] > 0)

summary(mod3_sim)

#predict 29yo, group2, active 30 days
log_lam <- as.matrix(mod3_csim) %*% c(29, 1, 1)
log_lam <- mod3_csim[,'b0']  + mod3_csim[,'b.age'] * 29 + mod3_csim[,'b.group2']
lam_new <- exp(log_lam)
y_new <- rpois(length(lam_new), lam_new * 30)
hist(y_new)
mean(y_new >= 3)
dat[dat$isgroup2 == 1,]
