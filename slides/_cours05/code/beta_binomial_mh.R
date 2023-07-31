library(tidyverse)

current = 0.5
nbTrial = 4
rbeta(1, current * (nbTrial - 2) + 1, (1 - current) * (nbTrial - 2) + 1)
prior2 <- dbeta(p_grid, mode1 * (nbTrial - 2) + 1, (1 - mode1) * (nbTrial - 2) + 1)

metropolis_hastings <- function (niter = 1e2, startval = 0.5, prior) {
    
    x <- rep(0, niter) # initialises the chain vector
    x[1] <- startval # defines the starting value
    
    for (i in 2:niter) {
        
        current <- x[i - 1] # current value of the parameter
        current_plaus <- dbeta(current, 2, 3) * dbinom(1, 2, current)
        
        if (prior == "uniform") {
            
            proposal <- runif(n = 1, min = 0, max = 1) # proposed value of the parameter
        
        } else if (prior == "normal") {
            
            proposal <- rnorm(n = 1, mean = current, sd = 0.1)
            proposal <- ifelse(proposal < 0, 0, proposal)
            proposal <- ifelse(proposal > 1, 1, proposal)
            
        } else if (prior == "beta") {
        
            certainty <- 8
            proposal <- rbeta(1, current * (certainty - 2) + 1, (1 - current) * (certainty - 2) + 1)
                
        }
        
        proposal_plaus <- dbeta(proposal, 2, 3) * dbinom(1, 2, proposal)
        alpha <- min(1, proposal_plaus / current_plaus) # moving probability ratio
        x[i] <- sample(c(current, proposal), size = 1, prob = c(1 - alpha, alpha) )
        
    }
    
    return (x)
    
}

z1 <- metropolis_hastings(niter = 1e4, startval = 0.5, prior = "beta")
z2 <- metropolis_hastings(niter = 1e4, startval = 0.5, prior = "normal")
z3 <- metropolis_hastings(niter = 1e4, startval = 0.5, prior = "uniform")

data.frame(z1 = z1, z2 = z2, z3 = z3) %>%
    mutate(sample = 1:nrow(.) ) %>%
    pivot_longer(cols = z1:z3) %>%
    ggplot(aes(x = sample, y = value, colour = name) ) +
    # geom_line(show.legend = FALSE) +
    geom_line() +
    geom_point() +
    labs(x = "Nombre d'itérations", y = expression(theta) )

data.frame(z1 = z1, z2 = z2, z3 = z3) %>%
    pivot_longer(cols = z1:z3) %>%
    rownames_to_column() %>%
    mutate(rowname = as.numeric(rowname) ) %>%
    ggplot(aes(x = value) ) +
    geom_histogram(aes(y = ..density..), color = "white") +
    stat_function(fun = dbeta, args = list(3, 4), color = "purple", size = 1) +
    facet_wrap(~name) +
    labs(x = expression(theta) )
