data("warpbreaks")
?warpbreaks
head(warpbreaks)

table(warpbreaks$wool, warpbreaks$tension)
boxplot(breaks ~ wool + tension, data=warpbreaks)
boxplot(log(breaks) ~ wool + tension, data=warpbreaks) #variance looks more similar

library(rjags)
X = model.matrix( ~ wool + tension, data=warpbreaks)
head(X)
tail(X)

# model for additive
mod1_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = int + alpha*isWoolB[i] + beta[1]*isTensionM[i] + beta[2]*isTensionH[i]
    }
    
    int ~ dnorm(0.0, 1.0/1.0e6)
    alpha ~ dnorm(0.0, 1.0/1.0e6)
    for (j in 1:2) {
        beta[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(3/2.0, 3*1.0/2.0)
    sig = sqrt(1.0 / prec)
} "

# model for cell means
mod2_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec)
    }
    
    for (j in 1:max(woolGrp)) {
        for (k in 1:max(tensGrp)) {
            mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
        }
    }
    
    prec ~ dgamma(3/2.0, 3*1.0/2.0)
    sig = sqrt(1.0 / prec)
} "

# model using different variance
mod3_string = " model {
    for( i in 1:length(y)) {
        y[i] ~ dnorm(mu[woolGrp[i], tensGrp[i]], prec[woolGrp[i], tensGrp[i]])
    }
    
    for (j in 1:max(woolGrp)) {
        for (k in 1:max(tensGrp)) {
            mu[j,k] ~ dnorm(0.0, 1.0/1.0e6)
            prec[j,k] ~ dgamma(1.0/2.0, 1.0/2.0)
            sig[j,k] = sqrt(1.0 / prec[j,k])
        }
    }
} "

data1_jags = list(y=log(warpbreaks$breaks), isWoolB=X[,"woolB"],
                  isTensionM=X[,"tensionM"], isTensionH=X[,"tensionH"])
params1 = c("int", "alpha", "beta", "sig")

data2_jags = list(y=log(warpbreaks$breaks), woolGrp=as.numeric(warpbreaks$wool),
                  tensGrp=as.numeric(warpbreaks$tension))
params2 = c("mu", "sig")

data3_jags = list(y=log(warpbreaks$breaks), woolGrp=as.numeric(warpbreaks$wool),
                  tensGrp=as.numeric(warpbreaks$tension))
params3 = c("mu", "sig")

# run the model
mod1 = jags.model(textConnection(mod1_string), data=data1_jags, n.chains=3)
update(mod1, 1e3)
mod1_sim = coda.samples(model=mod1, variable.names=params1, n.iter=5e3)

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, n.chains=3)
update(mod2, 1e3)
mod2_sim = coda.samples(model=mod2, variable.names=params2, n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod3_sim))

mod3 = jags.model(textConnection(mod3_string), data=data3_jags, n.chains=3)
update(mod3, 1e3)
mod3_sim = coda.samples(model=mod3, variable.names=params3, n.iter=5e3)

# convergence diagnostics
plot(mod1_sim)
gelman.diag(mod1_sim)

plot(mod2_sim, ask=TRUE)
gelman.diag(mod2_sim)

# check for autocorrelation
autocorr.diag(mod1_sim)
effectiveSize(mod1_sim)

autocorr.diag(mod2_sim)
effectiveSize(mod2_sim)
raftery.diag(mod2_sim)

# summary and performance
summary(mod1_sim)
dic1 = dic.samples(mod1, n.iter=1e3)

summary(mod2_sim)
dic2 = dic.samples(mod2, n.iter=1e3)

dic3 <- dic.samples(mod3, n.iter=1e3)
