library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)  # scatter plots for each pair of variables
summary(lm(education ~ income + young + urban, data = Anscombe))
plot(lm(education ~ income + young + urban, data = Anscombe))

library("rjags")

mod1_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    ## Initial guess of variance based on overall
    ## variance of education variable. Uses low prior
    ## effective sample size. Technically, this is not
    ## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

mod2_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    ## Initial guess of variance based on overall
    ## variance of education variable. Uses low prior
    ## effective sample size. Technically, this is not
    ## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

mod3_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*income[i]*young[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    ## Initial guess of variance based on overall
    ## variance of education variable. Uses low prior
    ## effective sample size. Technically, this is not
    ## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)
params <- c('b0', 'b', 'sig')
inits <- function() {
    inits = list('b0'=rnorm(1,0.0,100.0), "b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
mod <- jags.model(textConnection(mod_string), data = data_jags, n.chains = 3)
update(mod, 1000)
mod_sim <- coda.samples(model = mod, variable.names = params, n.iter = 5000)
mod_csim <- do.call(rbind, mod_sim)
head(mod_csim)

plot(mod_sim)
gelman.diag(mod_sim)
autocorr.diag(mod_sim)


##perform alternative and original DIC comparison
mod1 <- jags.model(textConnection(mod1_string), data = data_jags, n.chains = 3)
update(mod1, 1000)
mod2 <- jags.model(textConnection(mod2_string), data = data_jags, n.chains = 3)
update(mod2, 1000)
mod3 <- jags.model(textConnection(mod3_string), data = data_jags, n.chains = 3)
update(mod3, 1000)

dic.samples(mod1, n.iter = 1e5)
dic.samples(mod2, n.iter = 1e5)
dic.samples(mod3, n.iter = 1e5)

##sample from best fitting, which is model1
mod1_sim <- coda.samples(model = mod1, variable.names = c('b'), n.iter = 1e5)
mod1_csim <- do.call(rbind, mod1_sim)
head(mod1_csim)
mean(mod1_csim[,2] < 1)
