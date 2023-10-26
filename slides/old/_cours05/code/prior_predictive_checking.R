################################################################################
# Assume a model with a Normal likelihood function: y ~ Normal(mu, sigma)      #
# A Normal prior on the mean: mu ~ Normal(100, 10)                             #
# And an Exponential prior on the standard deviation: sigma ~ Exponential(0.1) #
################################################################################

# graphical parameters (three rows and one column)
par(mfrow = c(3, 1) )

# number of samples to draw
nobs <- 1e4

# simulating data from a normal distribution without (epistemic) uncertainty
rnorm(n = nobs, mean = 100, sd = 10) |> hist(breaks = "FD")

# drawing samples from the normal prior for mu (i.e., p(mu))
# what we know about mu before seeing the data
mu_prior <- rnorm(n = nobs, mean = 100, sd = 10)

# simulating data from a normal distribution with uncertainty on mu
rnorm(n = nobs, mean = mu_prior, sd = 10) |> hist(breaks = "FD")

# drawing samples from the exponential prior for sigma (i.e., p(sigma))
# what we know about sigma before seeing the data
sigma_prior <- rexp(n = nobs, rate = 0.1)

# simulating data from a normal distribution with uncertainty on both mu and sigma
# what we (the model) assume(s) about y according to our priors for mu and sigma
rnorm(n = nobs, mean = mu_prior, sd = sigma_prior) |> hist(breaks = "FD")
