## ----setup, eval = TRUE, include = FALSE, cache = FALSE------------------------------------------------
library(countdown)
library(tidyverse)
library(knitr)

# setting up knitr options
knitr::opts_chunk$set(
  cache = TRUE, echo = TRUE,
  warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )

# setting up ggplot theme
theme_set(theme_bw(base_size = 16, base_family = "Open Sans") )


## ----echo = TRUE---------------------------------------------------------------------------------------
library(tidyverse)
library(imsb)

d <- open_data(howell)
str(d)


## ----echo = TRUE---------------------------------------------------------------------------------------
d2 <- d %>% filter(age >= 18)
head(d2)


## ----echo = TRUE, fig.width = 6, fig.height = 6--------------------------------------------------------
d2 %>%
    ggplot(aes(x = height) ) +
    geom_histogram(aes(y = ..density..), bins = 20, col = "white") +
    stat_function(fun = dnorm, args = list(mean(d2$height), sd(d2$height) ), size = 1)


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 6-------------------------------------------
data.frame(value = rnorm(n = 1e4, mean = 10, sd = 1) ) %>% # 10.000 samples from Normal(10, 1)
    ggplot(aes(x = value) ) +
    geom_histogram(col = "white")


## ----normal-explain1, echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------
f1 <- function(x) {exp(-x)}
f2 <- function(x) {exp(-x^2)}

ggplot(data.frame(x = c(0, 5) ), aes(x = x) ) + 
  geom_path(aes(colour = "steelblue"), stat = "function", fun = f1, lwd = 1) +
  geom_path(aes(colour = "orangered"), stat = "function", fun = f2, lwd = 1) +
  labs(x = "x", y = "f(x)") +
  scale_colour_identity(
    "Function", guide = "legend",
    labels = c("y = exp(-x)", "y = exp(-x^2)"),
    breaks = c("steelblue", "orangered")
    )


## ----normal-explain2, echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------
ggplot(data.frame(x = c(-5, 5) ), aes(x = x) ) + 
  geom_path(aes(colour = "steelblue"), stat = "function", fun = f2, lwd = 1) +
  labs(x = "x", y = "f(x)") +
  scale_colour_identity(
    "Function", guide = "legend",
    labels = c("y = exp(-x^2)", "y = exp(-x^2)"),
    breaks = c("steelblue", "orangered")
    )


## ----normal-explain3, echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------
f <- expression(exp(-x^2) )
f_derivative <- D(f, "x")

f3 <- function(x) {-(exp(-x^2) * (2 * x) )}
f3_min <- optimize(f = f3, interval = c(-10, 10) )$minimum

ggplot(data.frame(x = c(-5, 5) ), aes(x = x) ) + 
  geom_path(aes(colour = "steelblue"), stat = "function", fun = f2, lwd = 1) +
  geom_area(
    data = data.frame(x = seq(-5, 5, 0.01) ) %>% filter(-f3_min < x & x < f3_min),
    aes(y = exp(-x^2) ),
    position = "identity", fill = "steelblue", alpha = 0.3
    ) +
  geom_path(aes(colour = "orangered"), stat = "function", fun = f3, lwd = 1) +
  geom_vline(xintercept = -f3_min, colour = "orangered", lty = 2) +
  geom_vline(xintercept = f3_min, colour = "orangered", lty = 2) +
  labs(x = "x", y = "f(x)") +
  scale_colour_identity(
    "Function", guide = "legend",
    labels = c("y = exp(-x^2)", "derivative"),
    breaks = c("steelblue", "orangered")
    )


## ----normal-explain4, echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------
f <- expression(exp((-0.5) * x^2) )
f_derivative <- D(f, "x")

f4 <- function(x) {exp((-0.5) * x^2)}
f5 <- function(x) {exp((-0.5) * x^2) * ((-0.5) * (2 * x) )}

ggplot(data.frame(x = c(-5, 5) ), aes(x = x) ) + 
  geom_path(aes(colour = "steelblue"), stat = "function", fun = f4, lwd = 1) +
  geom_path(aes(colour = "orangered"), stat = "function", fun = f5, lwd = 1) +
  geom_vline(xintercept = -1, colour = "orangered", lty = 2) +
  geom_vline(xintercept = 1, colour = "orangered", lty = 2) +
  labs(x = "x", y = "f(x)") +
  scale_colour_identity(
    "Function", guide = "legend",
    labels = c("y = exp(-0.5x^2)", "derivative"),
    breaks = c("steelblue", "orangered")
    )


## ----normal-explain5, echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------
f4 <- function(x) {exp((-0.5) * x^2)}

ggplot(data.frame(x = c(-5, 5) ), aes(x = x) ) + 
  geom_path(
    aes(colour = "steelblue"),
    colour = "steelblue",
    stat = "function", fun = f4, lwd = 1
    ) +
  labs(x = "x", y = "f(x)")


## ----normal-explain6, echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------
f4 <- function(x) {exp((-0.5) * (x - 3)^2)}

ggplot(data.frame(x = c(-5, 5) ), aes(x = x) ) + 
  geom_path(
    aes(colour = "steelblue"),
    colour = "steelblue",
    stat = "function", fun = f4, lwd = 1
    ) +
  labs(x = "x", y = "f(x)")


## ----normal-explain7, echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------
ggplot(data.frame(x = c(-5, 5) ), aes(x = x) ) + 
  geom_path(
    aes(colour = "steelblue"),
    colour = "steelblue",
    stat = "function", fun = dnorm, lwd = 1
    ) +
  labs(x = "x", y = "f(x)")


## ----eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 9, fig.height = 6--------------------
data.frame(x = c(100, 250) ) %>%
  ggplot(aes(x = x) ) +
  stat_function(
    fun = dnorm, args = list(mean = 178, sd = 20),
    fill = "steelblue", geom = "area", alpha = 0.8
    ) +
  labs(x = expression(mu), y = "Densité de probabilité")


## ----eval = TRUE, echo = FALSE, fig.width = 9, fig.height = 6------------------------------------------
data.frame(x = c(-10, 60) ) %>%
  ggplot(aes(x = x) ) +
  stat_function(
    fun = dunif, args = list(0, 50),
    fill = "steelblue", geom = "area", alpha = 0.8
    ) +
  labs(x = expression(sigma), y = "Densité de probabilité")


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## library(ks)
## sample_mu <- rnorm(1e4, 178, 20) # prior on mu
## sample_sigma <- runif(1e4, 0, 50) # prior on sigma
## prior <- data.frame(cbind(sample_mu, sample_sigma) ) # multivariate prior
## H.scv <- Hscv(x = prior, verbose = TRUE)
## fhat_prior <- kde(x = prior, H = H.scv, compute.cont = TRUE)
## plot(
##     fhat_prior, display = "persp", col = "steelblue", border = NA,
##     xlab = "\nmu", ylab = "\nsigma", zlab = "\n\np(mu, sigma)",
##     shade = 0.8, phi = 30, ticktype = "detailed",
##     cex.lab = 1.2, family = "Helvetica")


## ----prior-plot, echo = FALSE, out.width = "500px"-----------------------------------------------------
knitr::include_graphics("figures/prior.png")


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 6-------------------------------------------
sample_mu <- rnorm(1000, 178, 20)
sample_sigma <- runif(1000, 0, 50)

data.frame(x = rnorm(1000, sample_mu, sample_sigma) ) %>%
    ggplot(aes(x) ) +
    geom_histogram() +
    labs(x = "Taille (en cm)", y = "Nombre d'échantillons")


## ----eval = TRUE, echo = TRUE--------------------------------------------------------------------------
mu_exemple <- 151.23
sigma_exemple <- 23.42

d2$height[34] # une observation de taille (pour exemple)


## ----eval = TRUE, echo = FALSE, fig.width = 6, fig.height = 6------------------------------------------
ggplot(data.frame(x = c(50, 250) ), aes(x) ) +
    stat_function(
        fun = dnorm, args = list(mu_exemple, sigma_exemple), lwd = 2) +
    geom_segment(
        aes(
            x = d2$height[34],
            xend = d2$height[34],
            y = 0,
            yend = dnorm(d2$height[34], mu_exemple,sigma_exemple) ),
        color = "black", size = 1, linetype = 2) +
    geom_point(
        data = d2,
        aes(x = d2$height[34], y = dnorm(d2$height[34], mu_exemple,sigma_exemple) ),
        size = 4) +
    xlab("Taille (en cm)") +
    ylab("Vraisemblance")


## ----eval = TRUE, echo = TRUE, fig.align = "center"----------------------------------------------------
dnorm(d2$height[34], mu_exemple, sigma_exemple)


## ----eval = TRUE, echo = TRUE--------------------------------------------------------------------------
normal_likelihood <- function (x, mu, sigma) {
  
  bell <- exp( (- 1 / (2 * sigma^2) ) * (mu - x)^2 )
  norm <- sqrt(2 * pi * sigma^2)
  
  return(bell / norm)
  
}


## ----eval = TRUE, echo = TRUE--------------------------------------------------------------------------
normal_likelihood(d2$height[34], mu_exemple, sigma_exemple)


## ----grid, eval = TRUE, echo = TRUE--------------------------------------------------------------------
# on définit une grille de valeurs possibles pour mu et sigma
mu.list <- seq(from = 140, to = 160, length.out = 200)
sigma.list <- seq(from = 4, to = 9, length.out = 200)

# on étend la grille à toutes les combinaisons possibles de mu et sigma
post <- expand.grid(mu = mu.list, sigma = sigma.list)

# calcul de la log-vraisemblance (pour chaque combinaison de mu et sigma)
post$LL <-
  sapply(
    1:nrow(post),
    function(i) sum(dnorm(
      d2$height,
      mean = post$mu[i],
      sd = post$sigma[i],
      log = TRUE) )
    )

# calcul de la probabilité a posteriori (non normalisée)
post$prod <-
  post$LL +
  dnorm(x = post$mu, mean = 178, sd = 20, log = TRUE) +
  dunif(x = post$sigma, min = 0, max = 50, log = TRUE)

# on "annule" le log et on standardise par la valeur maximale (pour éviter les erreurs d'arrondi)
post$prob <- exp(post$prod - max(post$prod) )


## ----samples1, eval = TRUE, echo = TRUE----------------------------------------------------------------
# select random 20 rows of the dataframe 
post %>% slice_sample(n = 20, replace = FALSE)


## ----sampling-posterior, eval = TRUE, echo = TRUE------------------------------------------------------
sample.rows <- sample(x = 1:nrow(post), size = 1e4, replace = TRUE, prob = post$prob)
sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]


## ----plotting-samples, eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 12, fig.height = 8----
library(viridis)

sample.mu <- post$mu[sample.rows]
sample.sigma <- post$sigma[sample.rows]

ggplot(
    data.frame(sample.mu, sample.sigma),
    aes(x = sample.mu, y = sample.sigma)
    ) + 
    stat_density_2d(
        geom = "raster", aes(fill = ..density..),
        contour = FALSE, show.legend = FALSE
      ) +
    geom_vline(xintercept = mean(sample.mu), lty = 2) +
    geom_hline(yintercept = mean(sample.sigma), lty = 2) +
    scale_fill_viridis(na.value = "black") +
    coord_cartesian(
      xlim = c(min(sample.mu), max(sample.mu) ),
      ylim = c(min(sample.sigma), max(sample.sigma) )
      ) +
    scale_x_continuous(expand = c(0, 0) ) +
    scale_y_continuous(expand = c(0, 0) ) +
    labs(x = expression(mu), y = expression(sigma) )


## ----eval = TRUE, echo = TRUE, dev = "png", dpi = 200--------------------------------------------------
posterior_plot(samples = sample.mu, nbins = 30) + labs(x = expression(mu) )


## ----eval = TRUE, echo = TRUE, dev = "png", dpi = 200--------------------------------------------------
posterior_plot(samples = sample.sigma, nbins = 30) + labs(x = expression(sigma) )


## ----stan, eval = FALSE, echo = TRUE-------------------------------------------------------------------
## data {
##   int<lower=0> J; // number of schools
##   real y[J]; // estimated treatment effects
##   real<lower=0> sigma[J]; // s.e. of effect estimates
##   }
## 
## parameters {
##   real mu;
##   real<lower=0> tau;
##   real eta[J];
##   }
## 
## transformed parameters {
##   real theta[J];
##   for (j in 1:J)
##     theta[j] = mu + tau * eta[j];
##   }
## 
## model {
##   target += normal_lpdf(eta | 0, 1);
##   target += normal_lpdf(y | theta, sigma);
##   }


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## model <- brm(y ~ x + (1 | subject) + (1 | item), data = d, family = gaussian() )


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## Reaction ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## mvbind(Reaction, Memory) ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## mvbind(Reaction, Memory) ~ Days + (1 + Days | Subject)
## mvbind(Reaction, Memory) ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## mvbind(Reaction, Memory) ~ 0 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## brm(Reaction ~ 1 + Days + (1 + Days | Subject), family = lognormal() )


## ----fonctions-utiles, eval = FALSE, echo = TRUE-------------------------------------------------------
## # générer le code du modèle en Stan
## make_stancode(formula, ...)
## stancode(fit)
## 
## # définir les priors
## get_prior(formula, ...)
## set_prior(prior, ...)
## 
## # récupérer les prédictions du modèle
## fitted(fit, ...)
## predict(fit, ...)
## conditional_effects(fit, ...)
## 
## # posterior predictive checking
## pp_check(fit, ...)
## 
## # comparaison de modèles
## loo(fit1, fit2, ...)
## bayes_factor(fit1, fit2, ...)
## model_weights(fit1, fit2, ...)
## 
## # test d'hypothèse
## hypothesis(fit, hypothesis, ...)


## ----mod1, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
library(brms)
mod1 <- brm(height ~ 1, data = d2)


## ----summary-mod1, eval = TRUE, echo = TRUE------------------------------------------------------------
posterior_summary(mod1, pars = c("^b_", "sigma"), probs = c(0.025, 0.975) )


## ----get-prior, eval = TRUE, echo = TRUE---------------------------------------------------------------
get_prior(height ~ 1, data = d2)


## ----mod2, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
priors <- c(
  prior(normal(178, 20), class = Intercept),
  prior(exponential(0.01), class = sigma)
  )

mod2 <- brm(
  height ~ 1,
  prior = priors,
  family = gaussian(),
  data = d2
  )


## ----prior-mod2, echo = FALSE, fig.width = 15, fig.height = 5------------------------------------------
library(patchwork)

p1 <- data.frame(x = c(100, 250) ) %>%
  ggplot(aes(x = x) ) +
  stat_function(
    fun = dnorm, args = list(mean = 178, sd = 20),
    fill = "steelblue", geom = "area", alpha = 0.8
    ) +
  labs(x = expression(mu), y = "Densité de probabilité")

p2 <- data.frame(x = c(0, 500) ) %>%
  ggplot(aes(x = x) ) +
  stat_function(
    fun = dexp, args = list(0.01),
    fill = "steelblue", geom = "area", alpha = 0.8
    ) +
  labs(x = expression(sigma), y = "Densité de probabilité")

p1 + p2


## ----summary-mod2, eval = TRUE, echo = TRUE------------------------------------------------------------
summary(mod2)


## ----mod3, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
priors <- c(
  prior(normal(178, 0.1), class = Intercept),
  prior(exponential(0.01), class = sigma)
  )

mod3 <- brm(
  height ~ 1,
  prior = priors,
  family = gaussian(),
  data = d2
  )


## ----prior-mod3, echo = FALSE, fig.width = 15, fig.height = 5------------------------------------------
library(patchwork)

p1 <- data.frame(x = c(177, 179) ) %>%
  ggplot(aes(x = x) ) +
  stat_function(
    fun = dnorm, args = list(mean = 178, sd = 0.1),
    fill = "steelblue", geom = "area", alpha = 0.8
    ) +
  labs(x = expression(mu), y = "Densité de probabilité")

p2 <- data.frame(x = c(0, 500) ) %>%
  ggplot(aes(x = x) ) +
  stat_function(
    fun = dexp, args = list(0.01),
    fill = "steelblue", geom = "area", alpha = 0.8
    ) +
  labs(x = expression(sigma), y = "Densité de probabilité")

p1 + p2


## ----summary-mod3, eval = TRUE, echo = TRUE------------------------------------------------------------
summary(mod3)


## ----get-density-function, eval = TRUE, echo = FALSE---------------------------------------------------
library(viridis)
library(MASS)

# Get density of points in 2 dimensions.
# @param x A numeric vector.
# @param y A numeric vector.
# @param n Create a square n by n grid to compute density.
# @return The density within each square.

get_density <- function(x, y, n = 100) {
    
    dens <- MASS::kde2d(x = x, y = y, n = n)
    ix <- findInterval(x, dens$x)
    iy <- findInterval(y, dens$y)
    ii <- cbind(ix, iy)
    return(dens$z[ii])
    
}


## ----samples-plot, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 6-------
post <- as_draws_df(x = mod2) %>%
    mutate(density = get_density(b_Intercept, sigma, n = 1e2) )

ggplot(post, aes(x = b_Intercept, y = sigma, color = density) ) +
    geom_point(size = 2, alpha = 0.5, show.legend = FALSE) +
    labs(x = expression(mu), y = expression(sigma) ) +
    viridis::scale_color_viridis()


## ----eval = TRUE, echo = TRUE--------------------------------------------------------------------------
# gets the first 6 samples
head(post)


## ----eval = TRUE, echo = TRUE--------------------------------------------------------------------------
# gets the median and the 95% credible interval
t(sapply(post[, 1:2], quantile, probs = c(0.025, 0.5, 0.975) ) )


## ----eval = FALSE, echo = TRUE-------------------------------------------------------------------------
## H.scv <- Hscv(post[, 1:2])
## fhat_post <- kde(x = post[, 1:2], H = H.scv, compute.cont = TRUE)
## 
## plot(fhat_post, display = "persp", col = "purple", border = NA,
##   xlab = "\nmu", ylab = "\nsigma", zlab = "\np(mu, sigma)",
##   shade = 0.8, phi = 30, ticktype = "detailed",
##   cex.lab = 1.2, family = "Helvetica")


## ----posterior-plot, echo = FALSE, out.width = "600px"-------------------------------------------------
knitr::include_graphics("figures/posterior.png")


## ----plot-samples, eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 12, fig.height = 8-----
library(viridis)

sample.mu <- post$b_Intercept
sample.sigma <- post$sigma

data.frame(sample.mu, sample.sigma) %>%
    ggplot(aes(x = sample.mu, y = sample.sigma) ) + 
    stat_density_2d(
        geom = "raster",
        aes(fill = ..density..),
        contour = FALSE, show.legend = FALSE
        ) +
    geom_vline(xintercept = mean(sample.mu), lty = 2) +
    geom_hline(yintercept = mean(sample.sigma), lty = 2) +
    scale_fill_viridis(na.value = "black") +
    coord_cartesian(
        xlim = c(min(sample.mu), max(sample.mu) ),
        ylim = c(min(sample.sigma), max(sample.sigma) )
        ) +
    scale_x_continuous(expand = c(0, 0) ) +
    scale_y_continuous(expand = c(0, 0) ) +
    labs(x = expression(mu), y = expression(sigma) )


## ----height-weight-plot, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 6----
d2 %>%
  ggplot(aes(x = weight, y = height) ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8)


## ----lm-regression, eval = TRUE, echo = TRUE, fig.align = "center"-------------------------------------
linear_model <- lm(height ~ weight, data = d2)
rethinking::precis(linear_model, prob = 0.95)


## ----lm-regression-plot, eval = TRUE, echo = FALSE, fig.align = "center", fig.width = 7.5, fig.height = 5----
d2 %>%
    ggplot(aes(x = weight, y = height) ) +
    geom_point(
      colour = "white", fill = "black",
      pch = 21, size = 3, alpha = 0.8
      ) +
    geom_smooth(method = "lm", se = FALSE, color = "black", lwd = 1)


## ----mod4, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
priors <- c(
  prior(normal(178, 20), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod4 <- brm(
  height ~ 1 + weight,
  prior = priors,
  family = gaussian(),
  data = d2
  )


## ----summary-mod4, eval = TRUE, echo = TRUE------------------------------------------------------------
posterior_summary(mod4)


## ----mod5, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
d2$weight.c <- d2$weight - mean(d2$weight)

mod5 <- brm(
  height ~ 1 + weight.c,
  prior = priors,
  family = gaussian(),
  data = d2
  )


## ----fixef-mod5, eval = TRUE, echo = TRUE--------------------------------------------------------------
fixef(mod5) # retrieves the fixed effects estimates


## ----mod4-predictions, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 6----
d2 %>%
    ggplot(aes(x = weight, y = height) ) +
    geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
    geom_abline(intercept = fixef(mod4)[1], slope = fixef(mod4)[2], lwd = 1)


## ----fitted-mod4, eval = TRUE, echo = TRUE-------------------------------------------------------------
# on crée un vecteur de valeurs possibles pour "weight"
weight.seq <- data.frame(weight = seq(from = 25, to = 70, by = 1) )

# on récupère les prédictions du modèle pour ces valeurs de poids
mu <- data.frame(fitted(mod4, newdata = weight.seq) ) %>% bind_cols(weight.seq)

# on affiche les 10 premières lignes de mu
head(mu, 10)


## ----fitted-mod4-plot, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 5----
d2 %>%
  ggplot(aes(x = weight, y = height) ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  geom_smooth(
    data = mu, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity",
    color = "black", alpha = 0.8, size = 1
    )


## ----predict-mod4, eval = TRUE, echo = TRUE------------------------------------------------------------
# on crée un vecteur de valeurs possibles pour "weight"
weight.seq <- data.frame(weight = seq(from = 25, to = 70, by = 1) )

# on récupère les prédictions du modèle pour ces valeurs de poids
pred_height <- data.frame(predict(mod4, newdata = weight.seq) ) %>% bind_cols(weight.seq)

# on affiche les 10 premières lignes de pred_height
head(pred_height, 10)


## ----predict-mod4-plot, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 4----
d2 %>%
  ggplot(aes(x = weight, y = height) ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  geom_ribbon(
    data = pred_height, aes(x = weight, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.2, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = mu, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.8, size = 1
    )


## ----plot-poly, eval = TRUE, echo = TRUE, fig.align = "center"-----------------------------------------
d %>% # on utilise d au lieu de d2
  ggplot(aes(x = weight, y = height) ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8)


## ----poly-plot-std, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 6, fig.height = 4------
d <- d %>% mutate(weight.s = (weight - mean(weight) ) / sd(weight) )

d %>%
    ggplot(aes(x = weight.s, y = height) ) +
    geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8)

c(mean(d$weight.s), sd(d$weight.s) )


## ----mod6, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
priors <- c(
  prior(normal(156, 100), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod6 <- brm(
  # NB: polynomials should be written with the I() function...
  height ~ 1 + weight.s + I(weight.s^2),
  prior = priors,
  family = gaussian(),
  data = d
  )


## ----summary-mod6, eval = TRUE, echo = TRUE------------------------------------------------------------
summary(mod6)


## ----predict-mod6, eval = TRUE, echo = TRUE------------------------------------------------------------
# on crée un vecteur de valeurs possibles pour "weight"
weight.seq <- data.frame(weight.s = seq(from = -2.5, to = 2.5, length.out = 50) )

# on récupère les prédictions du modèle pour ces valeurs de poids
mu <- data.frame(fitted(mod6, newdata = weight.seq) ) %>% bind_cols(weight.seq)
pred_height <- data.frame(predict(mod6, newdata = weight.seq) ) %>% bind_cols(weight.seq)

# on affiche les 10 premières lignes de pred_height
head(pred_height, 10)


## ----predict-mod6-plot, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 5----
d %>%
  ggplot(aes(x = weight.s, y = height) ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  geom_ribbon(
    data = pred_height, aes(x = weight.s, ymin = Q2.5, ymax = Q97.5),
    alpha = 0.2, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = mu, aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
    stat = "identity", color = "black", alpha = 0.8, size = 1
    )


## ----effsize, eval = TRUE, echo = TRUE-----------------------------------------------------------------
post <- as_draws_df(x = mod4)
beta <- post$b_weight
sigma <- post$sigma
rho <- beta^2 * var(d2$weight) / (sigma^2 + beta^2 * var(d2$weight) )


## ----effsize-plot1, eval = TRUE, echo = TRUE, dev = "png", dpi = 200-----------------------------------
posterior_plot(samples = rho, usemode = TRUE) + labs(x = expression(rho) )


## ----summary-lm-effsize, eval = TRUE, echo = TRUE------------------------------------------------------
summary(lm(height ~ weight, data = d2) )$r.squared


## ----effsize-plot2, eval = TRUE, echo = TRUE, dev = "png", dpi = 200-----------------------------------
bayes_R2(mod4)


## ----effsize-plot3, eval = TRUE, echo = TRUE, dev = "png", dpi = 200-----------------------------------
bayes_R2(mod4, summary = FALSE)[, 1] %>%
    posterior_plot(usemode = TRUE) +
    labs(x = expression(rho) )


## ----mod7, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
# on garde seulement les individus ayant moins de 18 ans
d <- open_data(howell) %>% filter(age < 18)

priors <- c(
  prior(normal(150, 100), class = Intercept),
  prior(normal(0, 10), class = b),
  prior(exponential(0.01), class = sigma)
  )

mod7 <- brm(
  height ~ 1 + weight,
  prior = priors,
  family = gaussian(),
  data = d
  )


## ----summary-mod7, eval = TRUE, echo = TRUE------------------------------------------------------------
summary(mod7, prob = 0.89)


## ----predict-mod7, eval = TRUE, echo = TRUE------------------------------------------------------------
# on crée un vecteur de valeurs possibles pour "weight"
weight.seq <- data.frame(weight = seq(from = 5, to = 45, length.out = 1e2) )

# on récupère les prédictions du modèle pour ces valeurs de poids
mu <- data.frame(
  fitted(mod7, newdata = weight.seq, probs = c(0.055, 0.945) )
  ) %>%
  bind_cols(weight.seq)

pred_height <- data.frame(
  predict(mod7, newdata = weight.seq, probs = c(0.055, 0.945) )
  ) %>%
  bind_cols(weight.seq)

# on affiche les 6 premières lignes de pred_height
head(pred_height)


## ----predict-mod7-plot, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 5----
d %>%
  ggplot(aes(x = weight, y = height) ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  geom_ribbon(
    data = pred_height, aes(x = weight, ymin = Q5.5, ymax = Q94.5),
    alpha = 0.2, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = mu, aes(y = Estimate, ymin = Q5.5, ymax = Q94.5),
    stat = "identity", color = "black", alpha = 0.8, size = 1
    )


## ----mod8, eval = TRUE, echo = TRUE, results = "hide"--------------------------------------------------
# on considère maintenant tous les individus
d <- open_data(howell)

mod8 <- brm(
  # on prédit la taille par le logarithme du poids
  height ~ 1 + log(weight),
  prior = priors,
  family = gaussian(),
  data = d
  )


## ----summary-mod8, eval = TRUE, echo = TRUE------------------------------------------------------------
summary(mod8, prob = 0.89)


## ----predict-mod8, eval = TRUE, echo = TRUE------------------------------------------------------------
# on crée un vecteur de valeurs possibles pour "weight"
weight.seq <- data.frame(weight = seq(from = 5, to = 65, length.out = 1e2) )

# on récupère les prédictions du modèle pour ces valeurs de poids
mu <- data.frame(
  fitted(mod8, newdata = weight.seq, probs = c(0.055, 0.945) )
  ) %>%
  bind_cols(weight.seq)

pred_height <- data.frame(
  predict(mod8, newdata = weight.seq, probs = c(0.055, 0.945) )
  ) %>%
  bind_cols(weight.seq)

# on affiche les 6 premières lignes de pred_height
head(pred_height)


## ----predict-mod8-plot, eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 5----
d %>%
  ggplot(aes(x = weight, y = height) ) +
  geom_point(colour = "white", fill = "black", pch = 21, size = 3, alpha = 0.8) +
  geom_ribbon(
    data = pred_height, aes(x = weight, ymin = Q5.5, ymax = Q94.5),
    alpha = 0.2, inherit.aes = FALSE
    ) +
  geom_smooth(
    data = mu, aes(y = Estimate, ymin = Q5.5, ymax = Q94.5),
    stat = "identity", color = "black", alpha = 0.8, size = 1
    )

