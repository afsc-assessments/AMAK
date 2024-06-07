## Demonstrate adnuts on Hake model
library(adnuts)
library(tidyverse)
library(ggplot2)
library(ggthemes)
packageVersion('adnuts')                # needs to be 1.0.1
library(snowfall)
library(rstan)
library(shinystan)
chains <- parallel::detectCores()/4 # chains to run in parallel
## Reproducible seeds are passed to ADMB
set.seed(352)
seeds <- sample(1:1e4, size=chains)

# Model name
m <- './amak'
# Directory  
d <- 'mcmc'
# Assumes a converged MLE model has already been run....
setwd(d)
system(paste(m, '-nox -iprint 200 -mcmc 10 -binp amak.bar -phase 22 -hbf 1'))
setwd('..')

## Two different ways to gets NUTS working. First is to use the
## Hessian (metric) just like with the RMW. Note the control argument.
iter <- 2000 # maybe too many...depends are number cores...I used 8...
chains=8
fit.mle <- sample_nuts(model=m, path=d, iter=iter, warmup=iter/4, 
                   chains=chains, cores=chains, control=list(metric='mle'))
summary(fit.mle)
plot_uncertainties(fit.mle)

pairs_admb(fit.mle, pars=1:6, order='slow')
print(fit.mle)
plot_sampler_params(fit.mle)
launch_shinyadmb(fit.mle)
posterior_cp <- as.array(fit_cp)

plot_marginals(fit.mle)
library(bayesplot)
plot_sampler_params(fit.mle)                # NUTS adaptation
color_scheme_set("darkgray")
posterior_cp <- as.array(fit.mle)

mcmc_parcoord(posterior_cp, np = fit.mle)
mcmc_pairs(posterior_cp, np = np_cp, pars = c("mu","tau","theta[1]"),
           off_diag_args = list(size = 0.75))
mon <- monitor(fit.mle$samples, warmup=fit.mle$warmup, print=FALSE)

library(corrplot)
x <- fit.mle$mle$cor
dimnames(x) <- list('par'=fit.mle$mle$par.names, 'par2'=fit.mle$mle$par.names)
ind <- sort(unique(which(abs(x)>.0 & x!= 1, arr.ind=TRUE)[,1]))
y <- x[ind, ind]
ind
corrplot(y, method='color', type='upper')


## Alternatively if no Hessian is available (e.g., b/c of
## hierarchical model), then adapt a diagonal one during
## warmup. This is much slower b/c it doesn't know the shape of
## the posterior
iter <- 100
fit.diag <- sample_admb(model=m, path=d, iter=iter, algorithm='NUTS', warmup=iter/4,
                   seeds=seeds, parallel=TRUE, chains=chains, cores=chains)

## Now the samples from fit.diag can be used to estimate the
## covariance and that can be used directly.
fit.updated <- sample_admb(model=m, path=d, iter=iter, algorithm='NUTS', warmup=iter/4,
                   seeds=seeds, parallel=TRUE, chains=chains,
                   cores=chains, control=list(metric=fit$covar.est))
