#!/usr/bin/env Rscript
library(lme4)
library(mcemGLM)

# Read in params from SLURM
j <- as.numeric(Sys.getenv('SLURM_ARRAY_JOB_ID'))
a <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))
wd <- paste0('~/', j, '/')
filename <- paste0(wd, 'out', a, '.RData')

# Load functions
####################
source('sim_data.R')

# Set different seed for each array
####################
set.seed(1000 + a)

# Run
####################

ds <- sim_data(K=10, nk=5, beta=c(log(0.25), log(2)), tau=1, sigma=2)

time.gh <- system.time({
    fit.gh <- glmer(y ~ x + (1 | cluster), data=ds, family=binomial(), nAGQ=1)
})
time.mcem <- system.time({
    fit.mcem <- mcemGLMM(y ~ x, random = ~ 0 + fcluster, data=ds,
                         family='bernoulli', vcDist='normal',
			controlEM = list(verb=T))
})

# OUTPUT
coef.gh <- c(fit.gh@beta, fit.gh@theta^2)
coef.mcem <- as.vector(fit.mcem$mcemEST[nrow(fit.mcem$mcemEST), ])
out <- c(coef.gh, time.gh[1:3], coef.mcem, time.mcem[1:3])
names(out) <- c(paste('gh', c('beta0', 'beta1', 'tau_sq', 'user.self',
                                       'sys.self','elapsed'), sep='_'),
                paste('mcem', c('beta0', 'beta1', 'tau_sq', 'user.self',
                                       'sys.self','elapsed'), sep='_'))
save(out, file=filename)
