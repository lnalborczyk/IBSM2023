## ----setup, eval = TRUE, include = FALSE, cache = FALSE---------------------------------------------------------------------
library(countdown)
library(tidyverse)
library(patchwork)
library(knitr)
library(brms)
library(imsb)

# setting up knitr options
knitr::opts_chunk$set(
  cache = TRUE, echo = TRUE,
  warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )

# defining constant colour variables
prior_color <- "steelBlue"
likelihood_color <- "orangered"
posterior_color <- "magenta4"

# setting up ggplot theme
theme_set(theme_bw(base_size = 16, base_family = "Open Sans") )


## ----greek, echo = FALSE, fig.cap = "Figure from <https://masterofmemory.com/mmem-0333-learn-the-greek-alphabet/>."---------
knitr::include_graphics("figures/greek.jpeg")


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------------
## #####################################################################
## # We define a model with:                                           #
## # A Gaussian likelihood function: y ~ Normal(mu, sigma)             #
## # A Gaussian prior for the mean: mu ~ Normal(100, 10)               #
## # An Exponential prior for the dispersion: sigma ~ Exponential(0.1) #
## #####################################################################
## 
## # drawing 10.000 observations from a Gaussian distribution without (epistemic) uncertainty
## rnorm(n = 1e4, mean = 100, sd = 10) |> hist(breaks = "FD")
## 
## # drawing 10.000 observations from the Gaussian prior on mu (i.e., p(mu))
## # this prior represents what we know about mu before seeing the data...
## mu_prior <- rnorm(n = 1e4, mean = 100, sd = 10)
## 
## # drawing 10.000 observations from a Gaussian distribution with prior-related (epistemic) uncertainty
## rnorm(n = 1e4, mean = mu_prior, sd = 10) |> hist(breaks = "FD")
## 
## # drawing 10.000 observations from the Exponential prior on sigma (i.e., p(sigma))
## # this prior represents what we know about sigma before seeing the data...
## sigma_prior <- rexp(n = 1e4, rate = 0.1)
## 
## # drawing 10.000 observations from a Gaussian distribution with prior-related
## # (epistemic) uncertainty on mu AND sigma
## # this is what the model expects about y given our priors about mu and sigma and the observation model
## rnorm(n = 1e4, mean = mu_prior, sd = sigma_prior) |> hist(breaks = "FD")


## ----eval = TRUE, echo = FALSE, out.width = "75%", fig.asp = 0.75-----------------------------------------------------------
################################################################################
# Assume a model with a Normal likelihood function: y ~ Normal(mu, sigma)      #
# A Normal prior on the mean: mu ~ Normal(100, 10)                             #
# And an Exponential prior on the standard deviation: sigma ~ Exponential(0.1) #
################################################################################

# graphical parameters (three rows and one column)
par(mfrow = c(3, 1) )

# number of samples to draw
nsamples <- 1e4

# simulating data from a normal distribution without (epistemic) uncertainty
rnorm(n = nsamples, mean = 100, sd = 10) |>
    hist(breaks = "FD", xlim = c(-50, 250) )

# drawing samples from the normal prior for mu (i.e., p(mu))
# what we know about mu before seeing the data
mu_prior <- rnorm(n = nsamples, mean = 100, sd = 10)

# simulating data from a normal distribution with uncertainty on mu
rnorm(n = nsamples, mean = mu_prior, sd = 10) |>
    hist(breaks = "FD", xlim = c(-50, 250) )

# drawing samples from the exponential prior for sigma (i.e., p(sigma))
# what we know about sigma before seeing the data
sigma_prior <- rexp(n = nsamples, rate = 0.1)

# simulating data from a normal distribution with uncertainty on both mu and sigma
# what we (the model) assume(s) about y according to our priors for mu and sigma
rnorm(n = nsamples, mean = mu_prior, sd = sigma_prior) |>
    hist(breaks = "FD", xlim = c(-50, 250) )


## ----eval = FALSE, echo = FALSE---------------------------------------------------------------------------------------------
## # from https://plotly.com/r/3d-surface-plots/
## 
## z <- c(
##   c(8.83,8.89,8.81,8.87,8.9,8.87),
##   c(8.89,8.94,8.85,8.94,8.96,8.92),
##   c(8.84,8.9,8.82,8.92,8.93,8.91),
##   c(8.79,8.85,8.79,8.9,8.94,8.92),
##   c(8.79,8.88,8.81,8.9,8.95,8.92),
##   c(8.8,8.82,8.78,8.91,8.94,8.92),
##   c(8.75,8.78,8.77,8.91,8.95,8.92),
##   c(8.8,8.8,8.77,8.91,8.95,8.94),
##   c(8.74,8.81,8.76,8.93,8.98,8.99),
##   c(8.89,8.99,8.92,9.1,9.13,9.11),
##   c(8.97,8.97,8.91,9.09,9.11,9.11),
##   c(9.04,9.08,9.05,9.25,9.28,9.27),
##   c(9,9.01,9,9.2,9.23,9.2),
##   c(8.99,8.99,8.98,9.18,9.2,9.19),
##   c(8.93,8.97,8.97,9.18,9.2,9.18)
##   )
## 
## dim(z) <- c(15, 6)
## # z2 <- z + 1
## # z3 <- z - 1
## 
## fig <- plot_ly(showscale = FALSE)
## fig <- fig %>% add_surface(z = ~z)
## # fig <- fig %>% add_surface(z = ~z2, opacity = 0.98)
## # fig <- fig %>% add_surface(z = ~z3, opacity = 0.98)
## 
## # exporting it to an html object
## # orca(fig, file = "figures/plotly.png")
## htmlwidgets::saveWidget(fig, file = "plotly1.html")


## ----eval = TRUE------------------------------------------------------------------------------------------------------------
#| echo: false
#| out.width: "100%"
knitr::include_url(url = "plotly1.html", height = "600px")


## ----metropolis_picture, echo = FALSE, out.width = "20%"--------------------------------------------------------------------
knitr::include_graphics("figures/Nicholas_Metropolis_cropped.png")


## ----pi_gif, echo = FALSE, out.width = "25%"--------------------------------------------------------------------------------
knitr::include_graphics("figures/Pi_30K.gif")


## ----pi1, eval = TRUE, echo = TRUE, out.width = "25%"-----------------------------------------------------------------------
trials <- 1e5 # number of samples
radius <- 1 # radius of the circle
x <- runif(n = trials, min = 0, max = radius) # draws for x
y <- runif(n = trials, min = 0, max = radius) # draws for y
distance <- sqrt(x^2 + y^2) # distance to origin
inside <- distance < radius # is it within the quarter of circle?
pi_estimate <- 4 * sum(inside) / trials # estimated value of pi


## ----pi2, eval = TRUE, echo = FALSE, out.width = "33%", dev = "png"---------------------------------------------------------
data.frame(x, y, inside) %>%
    ggplot(aes(x, y, color = inside) ) +
    ggtitle(paste(round(trials), "Trials,", "Estimate =", pi_estimate) ) +
    guides(color = "none") +
    geom_point(size = 1 / trials)


## ----eval = FALSE, echo = FALSE---------------------------------------------------------------------------------------------
## # from https://plotly.com/r/3d-surface-plots/
## 
## z <- c(
##   c(8.83,8.89,8.81,8.87,8.9,8.87),
##   c(8.89,8.94,8.85,8.94,8.96,8.92),
##   c(8.84,8.9,8.82,8.92,8.93,8.91),
##   c(8.79,8.85,8.79,8.9,8.94,8.92),
##   c(8.79,8.88,8.81,8.9,8.95,8.92),
##   c(8.8,8.82,8.78,8.91,8.94,8.92),
##   c(8.75,8.78,8.77,8.91,8.95,8.92),
##   c(8.8,8.8,8.77,8.91,8.95,8.94),
##   c(8.74,8.81,8.76,8.93,8.98,8.99),
##   c(8.89,8.99,8.92,9.1,9.13,9.11),
##   c(8.97,8.97,8.91,9.09,9.11,9.11),
##   c(9.04,9.08,9.05,9.25,9.28,9.27),
##   c(9,9.01,9,9.2,9.23,9.2),
##   c(8.99,8.99,8.98,9.18,9.2,9.19),
##   c(8.93,8.97,8.97,9.18,9.2,9.18)
##   )
## 
## dim(z) <- c(15, 6)
## z2 <- z * 3 - 15
## 
## fig <- plot_ly(showscale = FALSE)
## fig <- fig %>% add_surface(z = ~z)
## fig <- fig %>% add_surface(z = ~z2, opacity = 0.98)
## 
## # exporting it to an html object
## htmlwidgets::saveWidget(fig, file = "plotly2.html")


## ----eval = TRUE------------------------------------------------------------------------------------------------------------
#| echo: false
#| out.width: "100%"
knitr::include_url(url = "plotly2.html", height = "600px")


## ----distribution_theta1, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-----------------------------------
# knitr::include_graphics("figures/distributionTheta1-7.png")

theta <- c(1, 2, 3, 4, 5, 6, 7)

theta %>%
  data.frame %>%
  ggplot(aes(x = theta, y = theta) ) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = expression(theta), y = expression(paste(p, "(", theta, ")") ) ) +
  scale_x_continuous(breaks = 1:7)


## ----distribution_theta2, echo = FALSE, out.width = "50%"-------------------------------------------------------------------
knitr::include_graphics("figures/DistribCarré1-7.png")


## ----eval = FALSE, echo = TRUE----------------------------------------------------------------------------------------------
## niter <- 100 # number of samples
## theta <- 1:7 # possible values for theta
## ptheta <- theta # probability of theta
## samples <- sample(x = theta, prob = ptheta, size = niter, replace = TRUE) # samples


## ----eval = TRUE, echo = FALSE, fig.width = 25------------------------------------------------------------------------------
set.seed(667)

trajLength <- 100
theta <- 1:7
ptheta <- theta
trajectory <- sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

layout(matrix(1:2, ncol = 2), widths = c(0.75, 0.25) )

plot(
    trajectory,
    main = "Posterior distribution based on 100 draws",
    ylab = bquote(theta), xlim = c(0, trajLength),
    xlab = "Iteration number",
    type = "o", pch = 20, col = posterior_color,
    cex.lab = 2, cex.main = 3, cex.axis = 2
    )

barplot(
    table(trajectory),
    col = posterior_color,
    horiz = TRUE, axes = FALSE, axisnames = FALSE
    )


## ----metro1, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"------------------------------------------------
theta %>%
  data.frame() %>%
  ggplot(aes(x = theta, y = theta) ) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = expression(theta), y = expression(paste(p, "(", theta, ")") ) ) +
  scale_x_continuous(breaks = 1:7) +
  annotate(
    geom = "segment", x = 4, y = 9.5, xend = 4, yend = 7.5,
    arrow = arrow(length = unit(5, "mm") )
    ) +
  annotate(
      geom = "text", x = 4, y = 10,
      label = "Starting position", hjust = "center", size = 5
      )


## ----metro2, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"------------------------------------------------
theta %>%
  data.frame() %>%
  ggplot(aes(x = theta, y = theta) ) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = expression(theta), y = expression(paste(p, "(", theta, ")") ) ) +
  scale_x_continuous(breaks = 1:7) +
  annotate(
    geom = "segment", x = 3, y = 9.5, xend = 3, yend = 7.5,
    arrow = arrow(length = unit(5, "mm") )
    ) +
  annotate(
    geom = "segment", x = 5, y = 9.5, xend = 5, yend = 7.5,
    arrow = arrow(length = unit(5, "mm") )
    ) +
  annotate(
      geom = "text", x = 3, y = 10,
      label = "50%", hjust = "center", size = 5
      ) +
  annotate(
      geom = "text", x = 5, y = 10,
      label = "50%", hjust = "center", size = 5
      )


## ----metro3, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"------------------------------------------------
theta %>%
  data.frame() %>%
  ggplot(aes(x = theta, y = theta) ) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = expression(theta), y = expression(paste(p, "(", theta, ")") ) ) +
  scale_x_continuous(breaks = 1:7) +
  annotate(
    geom = "segment", x = 5, y = 9.5, xend = 5, yend = 7.5,
    arrow = arrow(length = unit(5, "mm") )
    ) +
  annotate(
      geom = "text", x = 5, y = 10, label = "Pr(proposed) / Pr(current) = 5 / 4 > 1",
      hjust = "center", size = 5
      )


## ----metro4, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"------------------------------------------------
theta %>%
  data.frame() %>%
  ggplot(aes(x = theta, y = theta) ) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = expression(theta), y = expression(paste(p, "(", theta, ")") ) ) +
  scale_x_continuous(breaks = 1:7) +
  annotate(
    geom = "segment", x = 5, y = 9.5, xend = 5, yend = 7.5,
    arrow = arrow(length = unit(5, "mm") )
    ) +
  annotate(
      geom = "text", x = 5, y = 10,
      label = "New position", hjust = "center", size = 5
      )


## ----metropolis, eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------
metropolis <- function (niter = 1e2, startval = 4) {
    
    x <- rep(0, niter) # initialising the chain (vector) of length niter
    x[1] <- startval # defining the initial value of the parameter
    
    for (i in 2:niter) { # for each iteration
        
        current <- x[i - 1] # current value of the parameter
        proposal <- current + sample(c(-1, 1), size = 1)
        # we ensure the proposed value is within the [1, 7] interval
        if (proposal < 1) proposal <- 1
        if (proposal > 7) proposal <- 7
        # computing the probability of moving to the proposed position
        prob_move <- min(1, proposal / current)
        # we move (or not) according to this probability
        # x[i] <- ifelse(prob_move > runif(n = 1, min = 0, max = 1), proposal, current)
        x[i] <- sample(c(proposal, current), size = 1, prob = c(prob_move, 1 - prob_move) )
        
    }
    
    # returning the entire chain
    return (x)
    
}


## ----metropolis1, eval = TRUE, echo = FALSE, fig.width = 25, fig.height = 6, fig.align = "center"---------------------------
set.seed(666)

theta <- 1:7
ptheta <- theta
trajLength <- 200
trajectory <- sample(theta, prob = ptheta, size = trajLength, replace = TRUE)

layout(matrix(1:2, ncol = 2), widths = c(0.75, 0.25) )

plot(
    trajectory,
    main = "Monte Carlo methods",
    ylab = bquote(theta), xlim = c(0, trajLength), xlab = "Number of iterations",
    type = "o", pch = 20, col = prior_color,
    cex.lab = 2, cex.main = 3, cex.axis = 2
    )

barplot(
    table(trajectory), col = prior_color,
    horiz = TRUE, axes = FALSE, axisnames = FALSE
    )


## ----metropolis2, eval = TRUE, echo = FALSE, fig.width = 25, fig.height = 6, fig.align = "center"---------------------------
set.seed(666)

trajectory <- metropolis(niter = trajLength, startval = 4)

layout(matrix(1:2, ncol = 2), widths = c(0.75, 0.25) )

plot(
  trajectory,
  main = "Metropolis algorithm",
  ylab = bquote(theta), xlim = c(0, trajLength), xlab = "Number of iterations",
  type = "o", pch = 20, col = prior_color, cex.lab = 2, cex.main = 3, cex.axis = 2
  )

barplot(
  table(trajectory), col = prior_color,
  horiz = TRUE, axes = FALSE, axisnames = FALSE
  )


## ----echo = FALSE, out.width = "75%"----------------------------------------------------------------------------------------
knitr::include_graphics("figures/MetroAlgoAcceptProposal.png")


## ----eval = TRUE, echo = FALSE, fig.width = 8, fig.height = 8---------------------------------------------------------------
# source("code/DBDA2E-utilities.R")

# specifies the data to be used in the likelihood function
myData <- c(rep(0, 6), rep(1, 14) )

# defines the Bernoulli likelihood function p(D|theta)
# the argument theta could be a vector, not just a scalar

likelihood <- function (theta, data) {
  
  z <- sum(data)
  N <- length(data)
  pDataGivenTheta <- theta^z * (1 - theta)^(N - z)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the likelihood for theta > 1 or for theta < 0 is zero:
  
  pDataGivenTheta[theta > 1 | theta < 0] = 0
  
  return (pDataGivenTheta)
  
}

# defines the prior density function

prior_prob <- function (theta) {
  
  pTheta <- dbeta(theta, 1, 1)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the prior for theta > 1 or for theta < 0 is zero:
  
  pTheta[theta > 1 | theta < 0] = 0
  
  return (pTheta)
  
}

# defines the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.

targetRelProb <- function (theta, data) {
  
  targetRelProb <- likelihood(theta, data) * prior_prob(theta)
  
  return (targetRelProb)
  
}

# specifies the length of the trajectory, that is, the number of jumps to try
trajLength <- 50000 # arbitrary large number

# initialises the vector that will store the results:
trajectory <- rep(0, trajLength)

# specifies where to start the trajectory
trajectory[1] <- 0.01 # arbitrary value

# specifies the burn-in period
burnIn <- ceiling(0.0 * trajLength) # arbitrary number, less than trajLength

# initialises accepted, rejected counters, just to monitor performance:
nAccepted <- 0
nRejected <- 0

# now generate the random walk. The 't' index is time or trial in the walk.
# specifies seed to reproduce same random walk:
set.seed(47405)

# specifies standard deviation of proposal distribution
proposalSD <- c(0.02, 0.2, 2.0)[2]

for (t in 1:(trajLength - 1) ) {
  
	currentPosition <- trajectory[t]
	
	# uses the proposal distribution to generate a proposed jump
	
	proposedJump <- rnorm(1, mean = 0, sd = proposalSD)
	
	# computes the probability of accepting the proposed jump
	
	probAccept <- min(
	  1, targetRelProb(currentPosition + proposedJump, myData) / targetRelProb(currentPosition, myData)
	  )
	
	# generates a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump
	
	if (runif(1) < probAccept) {
	  
		# accept the proposed jump
		trajectory[t + 1] <- currentPosition + proposedJump
		
		# increment the accepted counter, just to monitor performance
		if (t > burnIn) {nAccepted = nAccepted + 1}
		
	} else {
	  
		# rejects the proposed jump, stay at current position
		trajectory[t + 1] = currentPosition
		
		# increments the rejected counter, just to monitor performance
		if (t > burnIn) {nRejected = nRejected + 1}
	
	}
	
}

# extracts the post-burnIn portion of the trajectory
acceptedTraj <- trajectory[ (burnIn+1) : length(trajectory) ]

##########################################
# Display the chain
###################################

# layout(matrix(1:3, nrow = 3) )
# par(mar = c(3, 4, 2, 1), mgp = c(2, 0.7, 0) )

# layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE) )
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE) )

# trajectory, a.k.a. trace plot, beginning of chain
idxToPlot <- 1:100

plot(
  trajectory[idxToPlot], idxToPlot, main = "Beginning of Chain",
  xlab = bquote(theta), xlim = c (0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior_color, cex.lab = 1.5
  )

# indicates burn in limit (might not be visible if not in range)
if (burnIn > 0) {
  
  abline(h = burnIn, lty = "dotted")
  text(0.5, burnIn + 1, "Burn In", adj = c(0.5, 1.1) )
  
}

# trajectory, a.k.a. trace plot, end of chain
idxToPlot <- (trajLength - 100):trajLength

plot(
  trajectory[idxToPlot], idxToPlot, main = "End of Chain",
  xlab = bquote(theta), xlim = c(0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior_color, cex.lab = 1.5
  )

# displays proposal SD and acceptance ratio in the plot
text(
  0.0, trajLength, adj = c(0.0, 1.1), cex = 1.5,
  labels = bquote(
    frac(N[acc], N[pro]) == .(signif(nAccepted / length(acceptedTraj), 3) )
    )
  )

paramInfo <- BEST::plotPost(
  paramSampleVec =acceptedTraj, xlim = c(0, 1), xlab = bquote(theta), 
  cex = 2, cex.main = 1.5, col = posterior_color,
  # main = paste0(
  #     "Proposal SD = ", proposalSD,
  #     ", ESS = ", round(coda::effectiveSize(acceptedTraj), 1)
  #     )
  )

# displays proposal SD and acceptance ratio in the plot
text(
  x = 0.2, y = 1, # adj = c(0.0, 1.1),
  cex = 1.5,
  labels = paste0(
      "Proposal SD = ", proposalSD,
      "\nESS = ", round(coda::effectiveSize(acceptedTraj), 1)
      )
  )


## ----eval = TRUE, echo = FALSE, fig.width = 8, fig.height = 8---------------------------------------------------------------
# source("code/DBDA2E-utilities.R")

# specifies the data to be used in the likelihood function
myData <- c(rep(0, 6), rep(1, 14) )

# defines the Bernoulli likelihood function p(D|theta)
# the argument theta could be a vector, not just a scalar

likelihood <- function(theta, data) {
  
  z <- sum(data)
  N <- length(data)
  pDataGivenTheta <- theta^z * (1 - theta)^(N - z)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the likelihood for theta > 1 or for theta < 0 is zero:
  
  pDataGivenTheta[theta > 1 | theta < 0] = 0
  
  return(pDataGivenTheta)
  
}

# defines the prior density function

prior_prob <- function(theta) {
  
  pTheta <- dbeta(theta, 1, 1)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the prior for theta > 1 or for theta < 0 is zero:
  
  pTheta[theta > 1 | theta < 0] = 0
  
  return(pTheta)
  
}

# defines the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.

targetRelProb <- function(theta, data) {
  
  targetRelProb <- likelihood(theta, data) * prior_prob(theta)
  
  return(targetRelProb)
  
}

# specifies the length of the trajectory, that is, the number of jumps to try
trajLength <- 50000 # arbitrary large number

# initialises the vector that will store the results:
trajectory <- rep(0 , trajLength)

# specifies where to start the trajectory
trajectory[1] <- 0.01 # arbitrary value

# specifies the burn-in period
burnIn <- ceiling(0.0 * trajLength) # arbitrary number, less than trajLength

# initialises accepted, rejected counters, just to monitor performance:
nAccepted <- 0
nRejected <- 0

# now generate the random walk. The 't' index is time or trial in the walk.
# specifies seed to reproduce same random walk:
set.seed(47405)

# specifies standard deviation of proposal distribution
proposalSD <- c(0.02, 0.2, 2.0)[1]

for (t in 1:(trajLength - 1) ) {
  
	currentPosition <- trajectory[t]
	
	# uses the proposal distribution to generate a proposed jump
	
	proposedJump <- rnorm(1, mean = 0, sd = proposalSD)
	
	# computes the probability of accepting the proposed jump
	
	probAccept <- min(
	  1,
		targetRelProb(currentPosition + proposedJump, myData) / targetRelProb(currentPosition, myData)
		)
	
	# generates a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump
	
	if (runif(1) < probAccept) {
	  
		# accept the proposed jump
		trajectory[t + 1] <- currentPosition + proposedJump
		
		# increment the accepted counter, just to monitor performance
		if (t > burnIn) {nAccepted = nAccepted + 1}
		
	} else {
	  
		# rejects the proposed jump, stay at current position
		trajectory[t + 1] = currentPosition
		
		# increments the rejected counter, just to monitor performance
		if (t > burnIn) {nRejected = nRejected + 1}
	
	}
	
}

# extracts the post-burnIn portion of the trajectory
acceptedTraj <- trajectory[ (burnIn+1) : length(trajectory) ]

##########################################
# Display the chain
###################################

# layout(matrix(1:3, nrow = 3) )
# par(mar = c(3, 4, 2, 1), mgp = c(2, 0.7, 0) )

# layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE) )
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE) )

# trajectory, a.k.a. trace plot, beginning of chain
idxToPlot <- 1:100

plot(
  trajectory[idxToPlot], idxToPlot, main = "Beginning of Chain",
  xlab = bquote(theta), xlim = c (0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior_color, cex.lab = 1.5
  )

# indicates burn in limit (might not be visible if not in range)
if (burnIn > 0) {
  
  abline(h = burnIn, lty = "dotted")
  text(0.5, burnIn + 1, "Burn In", adj = c(0.5, 1.1) )
  
}

# trajectory, a.k.a. trace plot, end of chain
idxToPlot <- (trajLength - 100):trajLength

plot(
  trajectory[idxToPlot], idxToPlot, main = "End of Chain",
  xlab = bquote(theta), xlim = c(0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior_color, cex.lab = 1.5
  )

# displays proposal SD and acceptance ratio in the plot
text(
  0.0, trajLength, adj = c(0.0, 1.1), cex = 1.5,
  labels = bquote(
    frac(N[acc], N[pro]) == .(signif(nAccepted / length(acceptedTraj), 3) )
    )
  )

# posterior histogram
paramInfo <- BEST::plotPost(
  acceptedTraj, xlim = c(0, 1), xlab = bquote(theta), 
  cex = 2, cex.main = 1.5, col = posterior_color,
  # main = bquote(list(
  #   "Proposal SD" == .(proposalSD),
  #   "ESS" == .(round(coda::effectiveSize(acceptedTraj), 1) )
  #   ) )
  )

# displays proposal SD and acceptance ratio in the plot
text(
  x = 0.2, y = 1, # adj = c(0.0, 1.1),
  cex = 1.5,
  labels = paste0(
      "Proposal SD = ", proposalSD,
      "\nESS = ", round(coda::effectiveSize(acceptedTraj), 1)
      )
  )


## ----eval = TRUE, echo = FALSE, fig.width = 8, fig.height = 8---------------------------------------------------------------
# source("code/DBDA2E-utilities.R")

# specifies the data to be used in the likelihood function
myData <- c(rep(0, 6), rep(1, 14) )

# defines the Bernoulli likelihood function p(D|theta)
# the argument theta could be a vector, not just a scalar

likelihood <- function(theta, data) {
  
  z <- sum(data)
  N <- length(data)
  pDataGivenTheta <- theta^z * (1 - theta)^(N - z)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the likelihood for theta > 1 or for theta < 0 is zero:
  
  pDataGivenTheta[theta > 1 | theta < 0] = 0
  
  return(pDataGivenTheta)
  
}

# defines the prior density function

prior_prob <- function(theta) {
  
  pTheta <- dbeta(theta, 1, 1)
  
  # the theta values passed into this function are generated at random,
  # and therefore might be inadvertently greater than 1 or less than 0.
  # the prior for theta > 1 or for theta < 0 is zero:
  
  pTheta[theta > 1 | theta < 0] = 0
  
  return(pTheta)
  
}

# defines the relative probability of the target distribution, 
# as a function of vector theta. For our application, this
# target distribution is the unnormalized posterior distribution.

targetRelProb <- function(theta, data) {
  
  targetRelProb <- likelihood(theta, data) * prior_prob(theta)
  
  return(targetRelProb)
  
}

# specifies the length of the trajectory, that is, the number of jumps to try
trajLength <- 50000 # arbitrary large number

# initialises the vector that will store the results:
trajectory <- rep(0 , trajLength)

# specifies where to start the trajectory
trajectory[1] <- 0.01 # arbitrary value

# specifies the burn-in period
burnIn <- ceiling(0.0 * trajLength) # arbitrary number, less than trajLength

# initialises accepted, rejected counters, just to monitor performance:
nAccepted <- 0
nRejected <- 0

# now generate the random walk. The 't' index is time or trial in the walk.
# specifies seed to reproduce same random walk:
set.seed(47405)

# specifies standard deviation of proposal distribution
proposalSD <- c(0.02, 0.2, 2.0)[3]

for (t in 1:(trajLength - 1) ) {
  
	currentPosition <- trajectory[t]
	
	# uses the proposal distribution to generate a proposed jump
	
	proposedJump <- rnorm(1, mean = 0, sd = proposalSD)
	
	# computes the probability of accepting the proposed jump
	
	probAccept <- min(
	  1,
		targetRelProb(currentPosition + proposedJump, myData) / targetRelProb(currentPosition, myData)
		)
	
	# generates a random uniform value from the interval [0,1] to
	# decide whether or not to accept the proposed jump
	
	if (runif(1) < probAccept) {
	  
		# accept the proposed jump
		trajectory[t + 1] <- currentPosition + proposedJump
		
		# increment the accepted counter, just to monitor performance
		if (t > burnIn) {nAccepted = nAccepted + 1}
		
	} else {
	  
		# rejects the proposed jump, stay at current position
		trajectory[t + 1] = currentPosition
		
		# increments the rejected counter, just to monitor performance
		if (t > burnIn) {nRejected = nRejected + 1}
	
	}
	
}

# extracts the post-burnIn portion of the trajectory
acceptedTraj <- trajectory[ (burnIn+1) : length(trajectory) ]

##########################################
# Display the chain
###################################

# layout(matrix(1:3, nrow = 3) )
# par(mar = c(3, 4, 2, 1), mgp = c(2, 0.7, 0) )

# layout(matrix(c(1,3,2,3), 2, 2, byrow = TRUE) )
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE) )

# trajectory, a.k.a. trace plot, beginning of chain
idxToPlot <- 1:100

plot(
  trajectory[idxToPlot], idxToPlot, main = "Beginning of Chain",
  xlab = bquote(theta), xlim = c (0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior_color, cex.lab = 1.5
  )

# indicates burn in limit (might not be visible if not in range)
if (burnIn > 0) {
  
  abline(h = burnIn, lty = "dotted")
  text(0.5, burnIn + 1, "Burn In", adj = c(0.5, 1.1) )
  
}

# trajectory, a.k.a. trace plot, end of chain
idxToPlot <- (trajLength - 100):trajLength

plot(
  trajectory[idxToPlot], idxToPlot, main = "End of Chain",
  xlab = bquote(theta), xlim = c(0, 1), ylab = "Step in Chain",
  type = "o", pch = 20, col = posterior_color, cex.lab = 1.5
  )

# displays proposal SD and acceptance ratio in the plot
text(
  0.0, trajLength, adj = c(0.0, 1.1), cex = 1.5,
  labels = bquote(
    frac(N[acc], N[pro]) == .(signif(nAccepted / length(acceptedTraj), 3) )
    )
  )

# posterior histogram
paramInfo <- BEST::plotPost(
  acceptedTraj, xlim = c(0, 1), xlab = bquote(theta), 
  cex = 2, cex.main = 1.5, col = posterior_color,
  # main = bquote(list(
  #   "Proposal SD" == .(proposalSD),
  #   "ESS" == .(round(coda::effectiveSize(acceptedTraj), 1) )
  #   ) )
  )

# displays proposal SD and acceptance ratio in the plot
text(
  x = 0.2, y = 1, # adj = c(0.0, 1.1),
  cex = 1.5,
  labels = paste0(
      "Proposal SD = ", proposalSD,
      "\nESS = ", round(coda::effectiveSize(acceptedTraj), 1)
      )
  )


## ----metropolis-beta-binomial1, eval = TRUE, echo = TRUE--------------------------------------------------------------------
metropolis_beta_binomial <- function (niter = 1e2, startval = 0.5) {
    
    x <- rep(0, niter) # initialising the chain (vector) of length niter
    x[1] <- startval # defining the starting/initial value
    
    for (i in 2:niter) {
        
        current <- x[i - 1] # current value of the parameter
        current_plaus <- dbeta(current, 2, 3) * dbinom(1, 2, current)
        # proposal <- runif(n = 1, min = current - w, max = current + w) # proposed value
        proposal <- rnorm(n = 1, mean = current, sd = 0.1) # proposed value
        # ensuring that the proposed value is within the [0, 1] interval
        if (proposal < 0) proposal <- 0
        if (proposal > 1) proposal <- 1
        proposal_plaus <- dbeta(proposal, 2, 3) * dbinom(1, 2, proposal)
        # computing the probability of moving
        alpha <- min(1, proposal_plaus / current_plaus)
        # moving (or not) according to this probability
        x[i] <- sample(c(current, proposal), size = 1, prob = c(1 - alpha, alpha) )
        
    }
    
    return (x)
    
}


## ----metropolis-beta-binomial2, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5------------------------------------
z1 <- metropolis_beta_binomial(niter = 1e4, startval = 0.5)
z2 <- metropolis_beta_binomial(niter = 1e4, startval = 0.5)

data.frame(z1 = z1, z2 = z2) %>%
  mutate(sample = 1:nrow(.) ) %>%
  pivot_longer(cols = z1:z2) %>%
  ggplot(aes(x = sample, y = value, colour = name) ) +
  geom_line(show.legend = FALSE) +
  labs(x = "Number of iterations", y = expression(theta) ) + ylim(c(0, 1) )


## ----metropolis-beta-binomial3, eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5------------------------------------
data.frame(z1 = z1, z2 = z2) %>%
  pivot_longer(cols = z1:z2) %>%
  rownames_to_column() %>%
  mutate(rowname = as.numeric(rowname) ) %>%
  ggplot(aes(x = value) ) +
  geom_histogram(aes(y = ..density..), color = "white", alpha = 0.8) +
  stat_function(fun = dbeta, args = list(3, 4), color = "magenta4", size = 1) +
  facet_wrap(~name) +
  labs(x = expression(theta), y = "Density")


## ----hmc1, echo = FALSE, out.width = "50%"----------------------------------------------------------------------------------
knitr::include_graphics("figures/HMC alorithme.png")


## ----hmc_erreur, echo = FALSE, out.width = "50%"----------------------------------------------------------------------------
knitr::include_graphics("figures/HMC alorithme ERREUR1.png")


## ----hmc_erreur2, echo = FALSE, out.width = "50%"---------------------------------------------------------------------------
knitr::include_graphics("figures/HMC alorithme ERREUR2.png")


## ----repres1, echo = FALSE, out.width = "50%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité1.png")


## ----repres2, echo = FALSE, out.width = "50%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité2.png")


## ----repres3, echo = FALSE, out.width = "50%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité3.png")


## ----autocorrelation, echo = FALSE, out.width = "40%"-----------------------------------------------------------------------
knitr::include_graphics("figures/Verif_autocorrelation.png")


## ----repres4, echo = FALSE, out.width = "50%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité4.png")


## ----repres5, echo = FALSE, out.width = "50%"-------------------------------------------------------------------------------
knitr::include_graphics("figures/Verif_representativité5.png")


## ----diagnostics1, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------
library(tidyverse)
library(imsb)
library(brms)

d <- open_data(howell)
d2 <- d %>% filter(age >= 18)

priors <- c(
  prior(normal(150, 20), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod1 <- brm(
  formula = height ~ 1 + weight,
  prior = priors,
  family = gaussian(),
  data = d2, 
  chains = 4, # number of chains
  iter = 2000, # total number of iteration (per chain)
  warmup = 1000, # number of warm-up iterations
  thin = 1 # thinning (1 = no thinning)
  )


## ----diagnostics2, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------
# combo can be hist, dens, dens_overlay, trace, trace_highlight...
# cf. https://mc-stan.org/bayesplot/reference/MCMC-overview.html
plot(x = mod1, combo = c("dens_overlay", "trace") )


## ----diagnostics3, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------
library(bayesplot)
post <- posterior_samples(mod1, add_chain = TRUE)
post %>% mcmc_acf(pars = vars(b_Intercept:sigma), lags = 10)


## ----diagnostics4, eval = TRUE, echo = TRUE---------------------------------------------------------------------------------
summary(mod1)


## ----diagnostics5, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------
post %>% # rank plots
  mcmc_rank_overlay(pars = vars(b_Intercept:sigma) ) +
  labs(x = "Rang", y = "Frequency") +
  coord_cartesian(ylim = c(25, NA) )


## ----rugged, eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------
library(tidyverse)
library(imsb)

d <- open_data(rugged) %>% mutate(log_gdp = log(rgdppc_2000) )
df1 <- d[complete.cases(d$rgdppc_2000), ]
str(df1)


## ----mod2b, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------------
priors2 <- c(
  prior(normal(0, 100), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod2 <- brm(
  formula = log_gdp ~ 1 + rugged * cont_africa,
  prior = priors2,
  family = gaussian(),
  data = df1,
  chains = 4, # nombre de MCMCs
  iter = 2000, # nombre total d'itérations (par chaîne)
  warmup = 1000 # nombre d'itérations pour le warm-up
  )


## ----mod2-summary, eval = TRUE, echo = TRUE---------------------------------------------------------------------------------
summary(mod2)


## ----mod2-diagnostics, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------
plot(x = mod2, combo = c("dens_overlay", "trace"), pars = "^b_")


## ----mod2-pairs, eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6----------------------------------------------------
pairs(x = mod2, np = nuts_params(mod2) ) # voir ?nuts_params


## ----echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"--------------------------------------------------------
# plot from https://bookdown.org/content/4857/big-entropy-and-the-generalized-linear-model.html#generalized-linear-models

tibble(x = seq(from = -1, to = 3, by = 0.01) ) %>%
  mutate(probability = 0.35 + x * 0.5) %>%
  ggplot(aes(x = x, y = probability) ) +
  geom_rect(xmin = -1, xmax = 3, ymin = 0,  ymax = 1, fill = "gray90") +
  geom_hline(yintercept = 0:1, linetype = 2) +
  geom_line(aes(linetype = probability > 1), linewidth = 1) +
  geom_segment(x = 1.3, xend = 3, y = 1, yend = 1, linewidth = 2 / 3) +
  scale_y_continuous(breaks = c(0, 0.5, 1) ) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 1.2) ) +
  theme(legend.position = "none", panel.grid = element_blank() ) +
  labs(x = "Predictor value", y = "Probability")


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-------------------------------------------
# plot from https://bookdown.org/content/4857/big-entropy-and-the-generalized-linear-model.html#generalized-linear-models

# make data for the horizontal lines
alpha <- 0
beta  <- 4

lines <-
  tibble(x = seq(from = -1, to = 1, by = 0.25) ) %>% 
  mutate(
    `log-odds` = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    )

# make the primary data 
beta <- 2

d <-
  tibble(x = seq(from = -1, to = 1, length.out = 50) ) %>% 
  mutate(
    `log-odds`  = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    ) 

# make the individual plots
p1 <-
  d %>% 
  ggplot(aes(x = x, y = `log-odds`) ) +
  geom_hline(
    data = lines,
    aes(yintercept = `log-odds`),
    color = "gray"
    ) +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Predictor value", y = "Log-odds")

p2 <-
  d %>% 
  ggplot(aes(x = x, y = probability) ) +
  geom_hline(
    data = lines,
    aes(yintercept = probability),
    color = "gray"
    ) +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Predictor value", y = "Probability")

p1 + p2


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-------------------------------------------
# plot from https://bookdown.org/content/4857/big-entropy-and-the-generalized-linear-model.html#generalized-linear-models

# make data for the horizontal lines
alpha <- 0
beta  <- 4

lines <-
  tibble(x = seq(from = -1, to = 1, by = 0.25) ) %>% 
  mutate(
    `log-odds` = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    )

# make the primary data 
beta <- 2

d <-
  tibble(x = seq(from = -1, to = 1, length.out = 50) ) %>% 
  mutate(
    `log-odds`  = alpha + x * beta,
    probability = exp(alpha + x * beta) / (1 + exp(alpha + x * beta) )
    ) 

# make the individual plots
p1 <-
  d %>% 
  ggplot(aes(x = x, y = `log-odds`) ) +
  geom_hline(
    data = lines,
    aes(yintercept = `log-odds`),
    color = "gray"
    ) +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Predictor value", y = "Log-odds")

p2 <-
  d %>% 
  ggplot(aes(x = x, y = probability) ) +
  geom_hline(
    data = lines,
    aes(yintercept = probability),
    color = "gray"
    ) +
  geom_line(linewidth = 1) +
  coord_cartesian(xlim = c(-1, 1) ) +
  theme(panel.grid = element_blank() ) +
  labs(x = "Predictor value", y = "Probability")

p1 + p2


## ----chimp, echo = FALSE, out.width = "50%"---------------------------------------------------------------------------------
knitr::include_graphics("figures/chimp_exp.jpg")


## ----echo = TRUE------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(imsb)

df1 <- open_data(chimpanzees) 
str(df1)


## ----mod1, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
library(brms)

mod1.1 <- brm(
  # "trials" allows defining the number of trials (i.e., n)
  formula = pulled_left | trials(1) ~ 1,
  family = binomial(),
  prior = prior(normal(0, 10), class = Intercept),
  data = df1,
  # we samples from the prior
  sample_prior = "yes"
  )


## ----ppc-mod1.1, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6, out.width = "60%"--------------------------------
# retrieving sample from the prior predictive distribution
prior_draws(x = mod1.1) %>%
  # applying the inverse link function
  mutate(p = brms::inv_logit_scaled(Intercept) ) %>%
  ggplot(aes(x = p) ) +
  geom_density(fill = "steelblue", adjust = 0.1) +
  labs(x = "Prior probability of pulling the left lever", y = "Probability density")


## ----ppc-mod1.2, eval = TRUE, echo = FALSE, results = "hide", fig.width = 12, fig.height = 6, out.width = "80%"-------------
mod1.2 <- brm(
  formula = pulled_left | trials(1) ~ 1,
  family = binomial,
  prior = prior(normal(0, 1), class = Intercept),
  data = df1,
  sample_prior = "yes"
  )

bind_rows(prior_draws(mod1.1), prior_draws(mod1.2) ) %>% 
  mutate(
    p = inv_logit_scaled(Intercept),
    w = factor(rep(c(10, 1), each = n() / 2), levels = c(10, 1) )
    ) %>%
  ggplot(aes(x = p, fill = w) ) +
  geom_density(alpha = 0.8, adjust = 0.1) +
  scale_fill_manual(expression(italic(omega) ), values = c("steelblue", "blue") ) +
  labs(x = "Prior probability of pulling the left lever", y = "Probability density")


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
fixed_effects <- fixef(mod1.2) # fixed effects (i.e., the intercept)
plogis(fixed_effects) # inverse link function


## ----eval = TRUE, echo = TRUE, results = "hide", fig.width = 9, fig.height = 6, out.width = "50%", dev = "png", dpi = 200----
post <- as_draws_df(x = mod1.2) # retrieving the posterior sample
intercept_samples <- plogis(post$b_Intercept) # posterior samples for the intercept

posterior_plot(samples = intercept_samples, compval = 0.5) + labs(x = "Probability of pulling left")


## ----mod2, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
# recoding predictors
df1 <- df1 %>%
  mutate(
    prosoc_left = ifelse(prosoc_left == 1, 0.5, -0.5),
    condition = ifelse(condition == 1, 0.5, -0.5)
    )

priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 10), class = b)
  )

mod2.1 <- brm(
  formula = pulled_left | trials(1) ~ 1 + prosoc_left * condition,
  family = binomial,
  prior = priors,
  data = df1,
  sample_prior = "yes"
  )


## ----ppc-mod2.1, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6, out.width = "50%"--------------------------------
prior_draws(x = mod2.1) %>% # prior samples
  mutate(
    condition1 = plogis(Intercept - 0.5 * b), # p in condition 1
    condition2 = plogis(Intercept + 0.5 * b) # p in condition 0
    ) %>%
  ggplot(aes(x = condition2 - condition1) ) + # plotting the difference
  geom_density(fill = "steelblue", adjust = 0.1) +
  labs(
    x = "Difference in the probability of pulling the left lever between conditions",
    y = "Probability density"
    )


## ----mod2.2, eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 1), class = b)
  )

mod2.2 <- brm(
  formula = pulled_left | trials(1) ~ 1 + prosoc_left * condition,
  family = binomial,
  prior = priors,
  data = df1,
  sample_prior = "yes"
  )


## ----ppc-mod2.2, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6, out.width = "50%"--------------------------------
prior_draws(mod2.2) %>% # prior samples
  mutate(
    condition1 = plogis(Intercept - 0.5 * b), # p in condition 1
    condition2 = plogis(Intercept + 0.5 * b) # p in condition 0
    ) %>%
  ggplot(aes(x = condition2 - condition1) ) +
  geom_density(fill = "steelblue", adjust = 0.1) +
  labs(
    x = "Difference in the probability of pulling the left lever between conditions",
    y = "Probability density"
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
summary(mod2.2)


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
fixef(mod2.2) # retrieving estimates for "fixed effects"


## ----eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6, out.width = "50%", dev = "png", dpi = 200---------------------
post <- as_draws_df(x = mod2.2) # posterior samples
posterior_plot(samples = exp(post$b_prosoc_left), compval = 1) + labs(x = "Odds ratio")


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
model_predictions <- fitted(mod2.2) %>% # prediction for p (i.e., the probability)
  data.frame() %>% 
  bind_cols(df1) %>%
  mutate(condition = factor(condition), prosoc_left = factor(prosoc_left) )


## ----eval = TRUE, echo = FALSE, fig.width = 9, fig.height = 6, out.width = "50%"--------------------------------------------
model_predictions %>%
  ggplot(aes(
    x = prosoc_left, y = Estimate,
    ymin = Q2.5, ymax = Q97.5, colour = condition
    ) ) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_line(
    aes(group = condition),
    size = 1,
    position = position_dodge(0.2)
    ) +
  geom_pointrange(
    aes(color = condition),
    size = 1, fatten = 2, show.legend = TRUE,
    position = position_dodge(0.2)
    ) +
  ylim(0, 1) +
  scale_x_discrete(labels = c("No", "Yes") ) +
  scale_colour_discrete(labels = c("Alone", "Social") ) +
  labs(
  x = "Was the prosocial option on the left?",
  y = "Probability of pulling the left lever"
  )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
(df2 <- open_data(admission) )


## ----mod3, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
priors <- c(prior(normal(0, 1), class = Intercept) )

mod3 <- brm(
  formula = admit | trials(applications) ~ 1,
  family = binomial(link = "logit"),
  prior = priors,
  data = df2,
  sample_prior = "yes"
  )


## ----mod4, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 1), class = b)
  )

# dummy-coding
df2$male <- ifelse(df2$gender == "Male", 1, 0)

mod4 <- brm(
  formula = admit | trials(applications) ~ 1 + male,
  family = binomial(link = "logit"),
  prior = priors,
  data = df2,
  sample_prior = "yes"
  )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
summary(mod4)


## ----eval = TRUE, echo = TRUE, fig.width = 8, fig.height = 6, out.width = "50%", dev = "png", dpi = 200---------------------
post <- as_draws_df(x = mod4)
p.admit.male <- plogis(post$b_Intercept + post$b_male)
p.admit.female <- plogis(post$b_Intercept)
diff.admit <- p.admit.male - p.admit.female
posterior_plot(samples = diff.admit, compval = 0)


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-------------------------------------------
# pp_check(mod4, type = "intervals", nsamples = 1e2, prob = 0.5, prob_outer = 0.95) +
#   scale_x_continuous(breaks = 1:12, limits = c(1, 12) ) +
#   theme_bw(base_size = 20) +
#   labs(x = "Sexe / Département", y = "Nombre d'admissions")

df2 <- df2 %>% mutate(case = factor(1:12) )

p <- 
  predict(mod4) %>% 
  as_tibble() %>% 
  bind_cols(df2)

d_text <-
  df2 %>%
  group_by(dept) %>%
  summarise(
    case  = mean(as.numeric(case) ),
    admit = mean(admit / applications) + 0.05
    )

ggplot(data = df2, aes(x = case, y = admit / applications) ) +
  geom_pointrange(
    data = p, 
    aes(
      y = Estimate / applications,
      ymin = Q2.5 / applications ,
      ymax = Q97.5 / applications
      ),
    shape = 1, alpha = 0.5
    ) +
  geom_point(color = "steelblue") +
  geom_line(
    aes(group = dept),
    color = "steelblue"
    ) +
  geom_text(
    data = d_text,
    aes(y = admit, label = dept),
    color = "steelblue"
    ) +
  coord_cartesian(ylim = 0:1) +
  scale_x_discrete(
    breaks = 1:12,
    labels = rep(c("male", "female"), 6)
    ) +
  labs(
    x = "",
    y = "Admission probability",
    title = "Posterior predictive check"
    )


## ----eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------------
# model without any predictor
mod5 <- brm(
  admit | trials(applications) ~ 0 + dept,
  family = binomial(link = "logit"),
  prior = prior(normal(0, 1), class = b),
  data = df2
  )

# model with one predictor (sex)
mod6 <- brm(
  admit | trials(applications) ~ 0 + dept + male,
  family = binomial(link = "logit"),
  prior = prior(normal(0, 1), class = b),
  data = df2
  )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
summary(mod6)


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
fixef(mod6)


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 6, out.width = "75%"-------------------------------------------
predict(mod6) %>%
  as_tibble() %>% 
  bind_cols(df2) %>%
  ggplot(aes(x = case, y = admit / applications) ) +
  geom_pointrange(
    aes(
      y = Estimate / applications,
      ymin = Q2.5 / applications,
      ymax = Q97.5 / applications
      ),
    color = "steelblue",
    shape = 1, alpha = 0.5
    ) +
  geom_point(color = "steelblue") +
  geom_line(
    aes(group = dept),
    color = "steelblue"
    ) +
  geom_text(
    data = d_text,
    aes(y = admit, label = dept),
    color = "steelblue"
    ) +
  coord_cartesian(ylim = 0:1) +
  scale_x_discrete(
    breaks = 1:12,
    labels = rep(c("male", "female"), 6)
    ) +
  labs(
    x = "",
    y = "Admission probability",
    title = "Posterior predictive check"
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
df3 <- open_data(absence)
df3 %>% sample_frac %>% head(10)


## ----mod7, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
mod7 <- brm(
    presence | trials(total) ~ 1,
    family = binomial(link = "logit"),
    prior = prior(normal(0, 1), class = Intercept),
    data = df3,
    # using all available parallel cores
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
fixef(mod7) # relative effect (log-odds)
fixef(mod7) %>% plogis # absolute effect (probability of presence)


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
df3 <-
  df3 %>%
  mutate(
    reminder = ifelse(reminder == "no", 0, 1),
    inscription = ifelse(inscription == "panel", 0, 1)
    )

head(df3, n = 10)


## ----mod8, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 1), class = b)
  )

mod8 <- brm(
    presence | trials(total) ~ 1 + reminder,
    family = binomial(link = "logit"),
    prior = priors,
    data = df3,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
exp(fixef(mod8)[2]) # odds ratio with and without the reminder e-mail


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5, dev = "png", dpi = 200---------------------------------------
post <- as_draws_df(x = mod8) # retrieving posterior samples
p.no <- plogis(post$b_Intercept) # probability of presence without reminder e-mail
p.yes <- plogis(post$b_Intercept + post$b_reminder) # probability of presence with reminder e-mail
posterior_plot(samples = p.yes - p.no, compval = 0, usemode = TRUE)


## ----eval = TRUE, echo = TRUE, fig.width = 8, fig.height = 4----------------------------------------------------------------
library(tidybayes)
library(modelr)

df3 %>%
  group_by(total) %>%
  data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
  add_fitted_draws(mod8, newdata = ., n = 100, scale = "linear") %>%
  mutate(estimate = plogis(.value) ) %>%
  group_by(reminder, .draw) %>%
  summarise(estimate = mean(estimate) ) %>%
  ggplot(aes(x = reminder, y = estimate, group = .draw) ) +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_line(aes(y = estimate, group = .draw), size = 0.5, alpha = 0.1) +
  ylim(0, 1) +
  labs(x = "Reminder e-mail", y = "Probability of presence")


## ----mod9, eval = TRUE, echo = TRUE, results = "hide"-----------------------------------------------------------------------
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 1), class = b)
  )

mod9 <- brm(
    presence | trials(total) ~ 1 + inscription,
    family = binomial(link = "logit"),
    prior = priors,
    data = df3,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5, dev = "png", dpi = 200---------------------------------------
post <- as_draws_df(x = mod9)
p.panel <- plogis(post$b_Intercept) # average probability of presence - panel
p.doodle <- plogis(post$b_Intercept + post$b_inscription) # average probability of presence- doodle
posterior_plot(samples = p.panel - p.doodle, compval = 0, usemode = TRUE)


## ----mod10, eval = TRUE, echo = TRUE, results = "hide"----------------------------------------------------------------------
priors <- c(
  prior(normal(0, 1), class = Intercept),
  prior(normal(0, 1), class = b)
  )

mod10 <- brm(
    presence | trials(total) ~ 1 + reminder + inscription,
    family = binomial(link = "logit"),
    prior = priors,
    data = df3,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
summary(mod10)


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
fixef(mod8) %>% exp() # computing the odds ratio
fixef(mod9) %>% exp() # computing the odds ratio
fixef(mod10) %>% exp() # computing the odds ratio


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6---------------------------------------------------------------
as_draws_df(x = mod10) %>%
    ggplot(aes(b_reminder, b_inscription) ) +
    geom_point(size = 3, pch = 21, alpha = 0.8, color = "white", fill = "black") +
    labs(x = "Effect (slope) of reminder email", y = "Effect (slope) of registration method")


## ----eval = TRUE, echo = TRUE-----------------------------------------------------------------------------------------------
open_data(absence) %>%
  group_by(inscription, reminder) %>%
  summarise(n = sum(total) ) %>%
  spread(key = reminder, value = n) %>%
  data.frame()

