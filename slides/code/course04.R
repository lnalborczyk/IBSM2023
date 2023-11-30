## ----setup, eval = TRUE, include = FALSE, cache = FALSE-------------------------------------------
library(tidyverse)
library(BEST)
library(brms)

# setting up knitr options
knitr::opts_chunk$set(
  cache = TRUE, echo = TRUE,
  warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )

# setting up ggplot theme
theme_set(theme_bw(base_size = 16, base_family = "Open Sans") )


## ----echo = FALSE, out.width = "300px"------------------------------------------------------------
knitr::include_graphics("figures/robot.png")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
library(tidyverse)
library(imsb)

df <- open_data(robot)
head(x = df, n = 15)


## ----eval = TRUE, echo = TRUE, fig.width = 15, fig.height = 5-------------------------------------
df %>%
  ggplot(aes(x = factor(cafe), y = wait, fill = factor(afternoon) ) ) +
  geom_dotplot(
    stackdir = "center", binaxis = "y",
    dotsize = 1, show.legend = FALSE
    ) +
  geom_hline(yintercept = mean(df$wait), linetype = 3) +
  facet_wrap(~afternoon, ncol = 2) +
  labs(x = "Café", y = "Waiting time (min)")


## ----eval = TRUE, echo = TRUE, ig.width = 7.5, fig.height = 5-------------------------------------
ggplot(data = data.frame(x = c(0, 10) ), aes(x = x) ) +
    stat_function(
        fun = dcauchy,
        args = list(location = 0, scale = 2), size = 1.5
        )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
library(brms)

mod1 <- brm(
  formula = wait ~ 1,
  prior = c(
    prior(normal(5, 10), class = Intercept),
    prior(cauchy(0, 2), class = sigma)
    ),
  data = df,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE, warning = FALSE----------------------------------------------------
posterior_summary(x = mod1, probs = c(0.025, 0.975), pars = c("^b_", "sigma") )


## ----eval = TRUE, echo = TRUE, fig.width = 14, fig.height = 7-------------------------------------
plot(x = mod1, combo = c("dens_overlay", "trace") )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
mod2 <- brm(
  formula = wait ~ 0 + factor(cafe),
  prior = c(
    prior(normal(5, 10), class = b),
    prior(cauchy(0, 2), class = sigma)
    ),
  data = df,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE, warning = FALSE----------------------------------------------------
posterior_summary(x = mod2, pars = "^b_")


## ----eval = TRUE, echo = TRUE, out.width = "33%"--------------------------------------------------
y1 <- rnorm(n = 1e4, mean = 5, sd = 1)
y2 <- rnorm(n = 1e4, mean = 0, sd = 1) + 5

data.frame(y1 = y1, y2 = y2) %>%
    pivot_longer(cols = 1:2, names_to = "x", values_to = "y") %>%
    ggplot(aes(x = y, colour = x) ) +
    geom_density(show.legend = FALSE)


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
mod3 <- brm(
  formula = wait ~ 1 + (1 | cafe),
  prior = c(
    prior(normal(5, 10), class = Intercept),
    prior(cauchy(0, 2), class = sigma),
    prior(cauchy(0, 2), class = sd)
    ),
  data = df,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----echo = FALSE, fig.width = 14, fig.height = 8-------------------------------------------------
library(wesanderson) # for plotting
post <- as_draws_df(mod3) # retrieving posterior samples

df %>%
    group_by(cafe) %>%
    summarise(Observed = mean(wait) ) %>%
    mutate(Estimated = coef(mod3)$cafe[, , ] %>% data.frame %>% pull(Estimate) ) %>%
    gather(type, Observed, Observed:Estimated) %>%
    ggplot(aes(x = cafe, y = Observed, fill = type) ) +
    geom_hline(yintercept = mean(post$b_Intercept), linetype = 2) +
    geom_point(pch = 21, size = 5, alpha = 0.8, colour = "white", show.legend = TRUE) +
    scale_color_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_fill_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "Café", breaks = 1:20) +
    ylab("Waiting time (min)") +
    theme(legend.title = element_blank() )


## ----echo = FALSE, fig.align = "center", out.width = "66%"----------------------------------------
knitr::include_graphics("figures/stein1.png")


## ----echo = FALSE, fig.align = "center", out.width = "75%"----------------------------------------
knitr::include_graphics("figures/stein2.png")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
# computing the WAIC for each model and storing it
mod1 <- add_criterion(mod1, "waic")
mod2 <- add_criterion(mod2, "waic")
mod3 <- add_criterion(mod3, "waic")

# comparing these WAICs
w <- loo_compare(mod1, mod2, mod3, criterion = "waic")
print(w, simplify = FALSE)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
posterior_summary(mod1, pars = c("^b", "sigma") )
posterior_summary(mod3, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
df2 <- open_data(robot_unequal) # new dataset

mod4 <- brm(
  formula = wait ~ 1 + (1 | cafe),
  prior = c(
    prior(normal(5, 10), class = Intercept),
    prior(cauchy(0, 2), class = sigma),
    prior(cauchy(0, 2), class = sd)
    ),
  data = df2,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----echo = FALSE, fig.width = 12, fig.height = 6-------------------------------------------------
post <- as_draws_df(mod4)

df2 %>%
    group_by(cafe) %>%
    summarise(Observed = mean(wait) ) %>%
    mutate(Estimated = coef(mod4)$cafe[, , ] %>% data.frame %>% pull(Estimate) ) %>%
    gather(type, Observed, Observed:Estimated) %>%
    ggplot(aes(x = cafe, y = Observed, fill = type) ) +
    geom_hline(yintercept = mean(post$b_Intercept), linetype = 2) +
    geom_point(pch = 21, size = 5, alpha = 0.8, colour = "white", show.legend = TRUE) +
    scale_color_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_fill_manual(values = rev(wes_palette(n = 2, name = "Chevalier1") ) )  +
    scale_x_continuous(name = "Café (from the least to the most visited)", breaks = 1:20) +
    ylab("Waiting time (min)") +
    theme(legend.title = element_blank() )


## ----echo = FALSE, out.width = "800px"------------------------------------------------------------
knitr::include_graphics("figures/bivariate.png")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
sigma_a <- 1
sigma_b <- 0.75
rho <- 0.7
cov_ab <- sigma_a * sigma_b * rho
(Sigma1 <- matrix(c(sigma_a^2, cov_ab, cov_ab, sigma_b^2), ncol = 2) )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
(sigmas <- c(sigma_a, sigma_b) ) # standard deviations
(Rho <- matrix(c(1, rho, rho, 1), nrow = 2) ) # correlation matrix
(Sigma2 <- diag(sigmas) %*% Rho %*% diag(sigmas) )


## ----echo = FALSE, fig.width = 14, fig.height = 7, cache = TRUE-----------------------------------
library(ggdist)

expand.grid(eta = c(0.5, 2, 5, 10), K = c(2, 3, 4, 5) ) %>%
  ggplot(
      aes(
          y = ordered(eta), dist = "lkjcorr_marginal",
          arg1 = K, arg2 = eta, fill = as.factor(eta)
          )
      ) +
  stat_dist_slab(p_limits = c(0, 1), alpha = 0.8) +
  facet_grid(~paste0(K, "x", K) ) +
  labs(x = expression(rho), y = "Probability density (per prior)") +
  scale_fill_manual(
      values = c("steelblue", "orangered", "purple", "darkgreen"),
      labels = c(
        expression(paste(zeta, " = ", "0.5") ),
        expression(paste(zeta, " = ", "2") ),
        expression(paste(zeta, " = ", "10") ),
        expression(paste(zeta, " = ", "50") )
        )
      ) +
    theme(
        legend.title = element_blank(),
        legend.text.align = 0,
        legend.background = element_rect(size = 0.5, colour = "black")
        )


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## Reaction ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## Reaction ~ Days + (1 + Days | Subject)
## Reaction ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## Reaction ~ 1 + Days + (1 | Subject)
## Reaction ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## Reaction ~ Days + (1 + Days || Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## brm(formula = Reaction ~ 1 + Days + (1 + Days | Subject), family = lognormal() )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
mod5 <- brm(
  formula = wait ~ 1 + afternoon + (1 + afternoon | cafe),
  prior = c(
    prior(normal(0, 10), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 2), class = sigma),
    prior(cauchy(0, 2), class = sd)
    ),
  data = df,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6--------------------------------------
post <- as_draws_df(x = mod5) # extracting posterior samples
R <- rethinking::rlkjcorr(n = 16000, K = 2, eta = 2) # samples from prior

data.frame(prior = R[, 1, 2], posterior = post$cor_cafe__Intercept__afternoon) %>%
    gather(type, value, prior:posterior) %>%
    ggplot(aes(x = value, color = type, fill = type) ) +
    geom_histogram(position = "identity", alpha = 0.2) +
    labs(x = expression(rho), y = "Number of samples")


## ----eval = TRUE, echo = FALSE, fig.width = 12, fig.height = 8------------------------------------
a1 <- sapply(1:20, function(i) mean(df$wait[df$cafe == i & df$afternoon == 0]) )
b1 <- sapply(1:20, function(i) mean(df$wait[df$cafe == i & df$afternoon == 1]) ) - a1

no_pooling <-
  data.frame(Intercept = a1, afternoon = b1) %>%
  mutate(model = "no pooling")

partial_pooling <-
  data.frame(coef(mod5)$cafe[, 1, 1:2]) %>%
  mutate(model = "partial pooling")

shrinkage <- bind_rows(no_pooling, partial_pooling)

mu <- c(mean(post$b_Intercept), mean(post$b_afternoon) )
rho <- mean(post$cor_cafe__Intercept__afternoon)
sda <- mean(post$sd_cafe__Intercept)
sdb <- mean(post$sd_cafe__afternoon)
cov_ab <- sda * sdb * rho
sigma <- matrix(c(sda^2, cov_ab, cov_ab, sdb^2), ncol = 2)

##############################################################################
# Helper function to make ellipse, credits to Tristan Mahr                   #
# https://tjmahr.github.io/plotting-partial-pooling-in-mixed-effects-models/ #
##############################################################################

library(ellipse)

make_ellipse <- function(cov_mat, center, level) {
    
    ellipse(cov_mat, centre = center, level = level) %>%
        as.data.frame() %>%
        add_column(level = level)
    
}

levels <- c(.1, .3, .5, .7)

df_ellipse <-
    levels %>%
    purrr::map_df(~ make_ellipse(sigma, mu, level = .x) ) %>% 
    rename(Intercept = x, afternoon = y)

shrinkage %>%
    mutate(id = rep(1:20, 2) ) %>%
    ggplot(aes(x = Intercept, y = afternoon, color = model) ) +
    scale_color_manual(values = wesanderson::wes_palette(n = 2, name = "Chevalier1") ) +
    geom_point(size = 5, show.legend = FALSE) +
    # connecting lines
    geom_path(
        aes(group = id, color = NULL),
        arrow = arrow(length = unit(.015, "npc"), type = "closed"), 
        show.legend = FALSE
        ) +
    # ellipses
    geom_path(
        aes(group = level, color = NULL),
        data = df_ellipse,
        linetype = "dashed", color = "grey40", alpha = 0.8
        ) +
    labs(x = "Intercept", y = "Slope")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
mod5 <- add_criterion(mod5, "waic")
w <- loo_compare(mod1, mod2, mod3, mod5, criterion = "waic")
print(w, simplify = FALSE)
model_weights(mod1, mod2, mod3, mod5, weights = "waic")


## ----eval = TRUE, echo = TRUE, warning = FALSE----------------------------------------------------
posterior_summary(mod1, pars = c("^b", "sigma") )
posterior_summary(mod3, pars = c("^b", "sigma") )
posterior_summary(mod5, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
library(lme4)
data(sleepstudy)
head(sleepstudy, 20)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------
sleepstudy %>%
    ggplot(aes(x = Days, y = Reaction) ) +
    geom_smooth(method = "lm", colour = "black") +
    geom_point() +
    facet_wrap(~Subject, nrow = 2) +
    scale_x_continuous(breaks = c(0, 2, 4, 6, 8) )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
# frequentist (flat-priors) models
fmod0 <- lm(Reaction ~ Days, sleepstudy)
fmod1 <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)
fmod2 <- lmer(Reaction ~ Days + (1 + Days | Subject), sleepstudy)

# comparing fmod1 and fmod2
anova(fmod1, fmod2)


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
mod6 <- brm(
  Reaction ~ 1 + Days,
  prior = c(
    prior(normal(200, 100), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma)
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
posterior_summary(mod6)


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
mod7 <- brm(
  Reaction ~ 1 + Days + (1 | Subject),
  prior = c(
    prior(normal(200, 100), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma),
    prior(cauchy(0, 10), class = sd)
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
posterior_summary(mod7, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------
mod8 <- brm(
  Reaction ~ 1 + Days + (1 + Days | Subject),
  prior = c(
    prior(normal(200, 100), class = Intercept),
    prior(normal(0, 10), class = b),
    prior(cauchy(0, 10), class = sigma),
    prior(cauchy(0, 10), class = sd)
    ),
  data = sleepstudy,
  warmup = 1000, iter = 5000,
  cores = parallel::detectCores()
  )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
posterior_summary(mod8, pars = c("^b", "sigma") )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
# computing and storing the WAIC of each model
mod6 <- add_criterion(mod6, "waic")
mod7 <- add_criterion(mod7, "waic")
mod8 <- add_criterion(mod8, "waic")

# comparing the WAICs of these models
w <- loo_compare(mod6, mod7, mod8, criterion = "waic")
print(w, simplify = FALSE)

# computing the relative weight of each model
model_weights(mod6, mod7, mod8, weights = "waic")


## ----rdk, echo = FALSE----------------------------------------------------------------------------
knitr::include_graphics("figures/rdk.gif")


## ----EAMs, eval = TRUE, echo = FALSE--------------------------------------------------------------
knitr::include_graphics("figures/eam.png")


## ----wiener-figure, echo = FALSE------------------------------------------------------------------
knitr::include_graphics("figures/wiener_figure.png")


## ----lexical-decision-figure, echo = FALSE--------------------------------------------------------
knitr::include_graphics("figures/lexical_decision_task.png")


## ----ddm-data-------------------------------------------------------------------------------------
# loading the "speed_acc" data from the "rtdists" package
data(speed_acc, package = "rtdists")

# reshaping the data
df <- speed_acc %>%
    # removing extreme RTs
    filter(censor == FALSE) %>%
    # removing ppt with id=2 (less observations than others)
    filter(id != 2) %>%
    # focusing on high-frequency words and non-words
    filter(frequency %in% c("high", "nw_high") ) %>%
    # converting the response variable to a numeric 0/1 variable
    mutate(response2 = as.numeric(response == "word") ) %>%
    # keeping only some proportion of the data (for computational ease)
    filter(as.numeric(block) < 9) %>%
    mutate(id = factor(id), block = factor(block) )


## ----ddm-formula, eval = TRUE, echo = TRUE--------------------------------------------------------
# defining the model formula (one "linear model" per parameter)
formula <- brmsformula(
  # drift rate (delta)
  rt | dec(response2) ~ 1 + condition * stim_cat + (1 + condition * stim_cat | id),
  # boundary separation parameter (alpha)
  bs ~ 1 + condition + (1 + condition | id),
  # non-decision time (tau)
  ndt ~ 1 + condition + (1 + condition | id),
  # starting point or bias (beta)
  bias ~ 1 + condition + (1 + condition | id)
  )


## ----ddm-priors, eval = TRUE, echo = TRUE---------------------------------------------------------
# defining the contrasts
contrasts(df$condition) <- c(+0.5, -0.5)
contrasts(df$stim_cat) <- c(+0.5, -0.5)

# defining the priors
priors <- c(
  # priors for the intercepts
  prior(normal(0, 5), class = "Intercept"),
  prior(normal(0, 1), class = "Intercept", dpar = "bs"),
  prior(normal(0, 1), class = "Intercept", dpar = "ndt"),
  prior(normal(0, 1), class = "Intercept", dpar = "bias"),
  # priors for the slopes
  prior(normal(0, 1), class = "b"),
  # priors on the SD of the varying effects
  prior(exponential(1), class = "sd")
  )


## ----ddm-fitting, eval = TRUE, results = "hide"---------------------------------------------------
# specify initial values to help the model start sampling
# (with small variation between chains)
chains <- 8 # number of chains
epsilon <- 0.1 # variability in starting value for the NDT intercept
get_init_value <- function (x) list(Intercept_ndt = rnorm(n = 1, mean = x, sd = epsilon) )
inits_drift <- replicate(chains, get_init_value(-3), simplify = FALSE)

# fitting the model
fit_wiener <- brm(
  formula = formula,
  data = df,
  # specifying the family and link functions for each parameter
  family = wiener(
    link = "identity", link_bs = "log",
    link_ndt = "log", link_bias = "logit"
    ),
  # comment this line to use default priors
  prior = priors,
  # list of initialisation values
  init = inits_drift,
  init_r = 0.05,
  warmup = 1000, iter = 5000,
  chains = chains, cores = chains,
  # control = list(adapt_delta = 0.99, max_treedepth = 15),
  # saves the model (as .rds) or loads it if it already exists
  file = "models/ddm.rds",
  # needed for hypothesis testing
  sample_prior = TRUE
  )


## ----diagnostics, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6------------------------
# combo can be hist, dens, dens_overlay, trace, trace_highlight...
# cf. https://mc-stan.org/bayesplot/reference/MCMC-overview.html
plot(
    x = fit_wiener, combo = c("dens_overlay", "trace"),
    variable = variables(fit_wiener)[1:4],
    ask = FALSE
    )


## ----ppc, eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6--------------------------------
pp_check(object = fit_wiener, ndraws = 10) +
  labs(x = "Reaction time", y = "Density")


## ----qqplot1, eval = TRUE, echo = FALSE-----------------------------------------------------------
# aggregating the data using the qpf() function from
# https://vasishth.github.io/bayescogsci/book/ch-lognormalrace.html#sec-acccoding
qpf <- function (df_grouped, preds = FALSE, quantiles = c(0.1, 0.3, 0.5, 0.7, 0.9) ) {
    
    if (preds == FALSE) {
        
        df_grouped %>%
            summarise(
                rt_q = list(c(quantile(rt[acc == 0], quantiles), quantile(rt[acc == 1], quantiles) ) ),
                p = list(c(rep(mean(acc == 0), length(quantiles) ), rep(mean(acc == 1), length(quantiles) ) ) ),
                q = list(rep(quantiles, 2) ),
                response = list(c(rep("incorrect", length(quantiles) ), rep("correct", length(quantiles) ) ) )
                ) %>%
            # Since the summary contains a list in each column,
            # we unnest it to have the following number of rows:
            # number of quantiles x groups x 2 (incorrect, correct)
            unnest(cols = c(rt_q, p, q, response) )
        
    } else{
        
        df_grouped %>%
            summarise(
                rt_q = list(c(quantile(rt[acc == 0], quantiles), quantile(rt[acc == 1], quantiles) ) ),
                preds_rt_q = list(c(quantile(model_predictions[acc == 0], quantiles), quantile(model_predictions[acc == 1], quantiles) ) ),
                p = list(c(rep(mean(acc == 0), length(quantiles) ), rep(mean(acc == 1), length(quantiles) ) ) ),
                q = list(rep(quantiles, 2) ),
                response = list(c(rep("incorrect", length(quantiles) ), rep("correct", length(quantiles) ) ) )
                ) %>%
            # Since the summary contains a list in each column,
            # we unnest it to have the following number of rows:
            # number of quantiles x groups x 2 (incorrect, correct)
            unnest(cols = c(rt_q, preds_rt_q, p, q, response) )
        
    }
    
}


## ----qqplot1bis, eval = TRUE, echo = TRUE---------------------------------------------------------
# aggregating the data using the qpf() function from
# https://vasishth.github.io/bayescogsci/book/ch-lognormalrace.html#sec-acccoding
df_qpf <- df %>%
    mutate(acc = ifelse(as.character(stim_cat) == as.character(response), 1, 0) ) %>%
    group_by(stim_cat, condition) %>%
    qpf() %>%
    ungroup()

head(df_qpf)


## ----qqplot2, eval = TRUE, echo = FALSE, fig.asp = 0.75-------------------------------------------
df_qpf %>%
    ggplot(aes(x = p, y = rt_q) ) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_point(aes(shape = stim_cat) ) +
    geom_line(aes(group = interaction(q, response) ) ) +
    facet_wrap(~condition, ncol = 1, scales = "free_y") +
    labs(
        x = "Response proportion",
        y = "RT quantiles (s)"
        ) +
    annotate("text", x = 0.4, y = 0.4, label = "incorrect") +
    annotate("text", x = 0.6, y = 0.4, label = "correct")


## ----qqplot3, eval = TRUE, echo = FALSE, fig.asp = 0.75-------------------------------------------
# preds_wiener <- predict(
#     fit_wiener, summary = FALSE,
#     negative_rt = TRUE,
#     ndraws = 200,
#     cores = 8
#     )
# 
# df_with_preds <- as_tibble(cbind(df, as_tibble(t(preds_wiener) ) ) )

df_qpf_preds <- df %>%
    mutate(acc = ifelse(as.character(stim_cat) == as.character(response), 1, 0) ) %>%
    mutate(
        model_predictions = predict(
            object = fit_wiener,
            ndraws = 500,
            negative_rt = FALSE,
            cores = 8
            )[, "Estimate"]
        )

df_qpf_preds %>%
    group_by(stim_cat, condition) %>%
    qpf(preds = TRUE) %>%
    ungroup() %>%
    # head()
    ggplot(aes(x = p, y = rt_q) ) +
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_point(aes(shape = stim_cat, y = preds_rt_q), colour = "purple", alpha = 0.8) +
    geom_line(aes(group = interaction(q, response), y = preds_rt_q), colour = "purple", alpha = 0.8) +
    geom_point(aes(shape = stim_cat) ) +
    geom_line(aes(group = interaction(q, response) ) ) +
    facet_wrap(~condition, ncol = 1, scales = "free_y") +
    labs(
        x = "Response proportion",
        y = "RT quantiles (s)"
        ) +
    annotate("text", x = 0.4, y = 0.4, label = "incorrect") +
    annotate("text", x = 0.6, y = 0.4, label = "correct")


## ----posterior-drift, eval = TRUE, echo = TRUE----------------------------------------------------
library(tidybayes)
library(emmeans)

drift_rate_samples_per_condition <- fit_wiener %>%
    # retrieving drift rate values per condition
    emmeans(~condition * stim_cat) %>%
    # retrieving posterior sample for each cell
    gather_emmeans_draws()


## ----posterior-drift-plot, eval = TRUE, echo = FALSE, fig.asp = 0.5-------------------------------
# plotting it
drift_rate_samples_per_condition %>%
    mutate(.value = if_else(stim_cat == "nonword", (-1) * .value, .value) ) %>% 
    pivot_wider(names_from = condition, values_from = .value) %>%
    mutate(accuracy_speed_diff = accuracy - speed) %>%
    ggplot(aes(x = accuracy_speed_diff) ) +
    geom_vline(xintercept = 0) +
    geom_histogram(color = "white", bins = 60, alpha = 1) +
    facet_wrap(~stim_cat) +
    labs(x = "Difference in drift rate (accuracy - speed)", y = "Number of posterior samples")


## ----postplot-drift-rate, eval = TRUE, echo = TRUE, fig.width = 9, fig.height = 6, dev = "png", dpi = 200----
samps <- drift_rate_samples_per_condition %>%
    mutate(.value = if_else(stim_cat == "nonword", (-1) * .value, .value) ) %>% 
    pivot_wider(names_from = condition, values_from = .value) %>%
    mutate(accuracy_speed_diff = accuracy - speed)

posterior_plot(
    samples = sample(x = samps$accuracy_speed_diff, size = 1e3),
    compval = 0, nbins = 30
    ) + labs(x = "Difference in drift rate (accuracy - speed)")


## ----posterior-bs1, eval = TRUE, echo = TRUE------------------------------------------------------
# retrieving posterior samples
post <- as_draws_df(x = fit_wiener)
# retrieving the posterior samples for the boundary-separation
posterior_intercept_bs <- post$b_bs_Intercept
posterior_slope_bs <- post$b_bs_condition1
# computing the posterior distribution in the speed condition
posterior_bs_speed <- exp(posterior_intercept_bs - 0.5 * posterior_slope_bs)
# computing the posterior distribution in the accuracy condition
posterior_bs_accuracy <- exp(posterior_intercept_bs + 0.5 * posterior_slope_bs)


## ----posterior-bs2, eval = TRUE, echo = FALSE, fig.asp = 1----------------------------------------
# plotting it
par(mfrow = c(2, 2) )
plotPost(
  exp(posterior_intercept_bs), showMode = TRUE,
  xlab = expression(paste(exp(beta[0][paste("[", alpha, "]")] ) ) )
  )
plotPost(
  exp(posterior_slope_bs), showMode = TRUE, compVal = 1,
  xlab = expression(paste(exp(beta["condition"][paste("[", alpha, "]")] ) ) )
  )
plotPost(
  posterior_bs_speed, showMode = TRUE,
  xlab = expression(paste(alpha["speed"]) )
  )
plotPost(
  posterior_bs_accuracy, showMode = TRUE,
  xlab = expression(paste(alpha["accuracy"]) )
  )


## ----posterior-ndt1, eval = TRUE, echo = TRUE-----------------------------------------------------
# retrieves the posterior samples for the non-decision time
posterior_intercept_ndt <- post$b_ndt_Intercept
posterior_slope_ndt <- post$b_ndt_condition1
# computes the posterior distribution in the speed condition
posterior_ndt_speed <- exp(posterior_intercept_ndt - 0.5 * posterior_slope_ndt)
# computes the posterior distribution in the accuracy condition
posterior_ndt_accuracy <- exp(posterior_intercept_ndt + 0.5 * posterior_slope_ndt)


## ----posterior-ndt2, eval = TRUE, echo = FALSE, fig.asp = 1---------------------------------------
# plotting it
par(mfrow = c(2, 2) )
plotPost(
  exp(posterior_intercept_ndt), showMode = TRUE,
  xlab = expression(paste(exp(beta[0][paste("[", alpha, "]")] ) ) )
  )
plotPost(
  exp(posterior_slope_ndt), showMode = TRUE, compVal = 1,
  xlab = expression(paste(exp(beta["condition"][paste("[", alpha, "]")] ) ) )
  )
plotPost(
  posterior_ndt_speed, showMode = TRUE,
  xlab = expression(paste(alpha["speed"]) )
  )
plotPost(
  posterior_ndt_accuracy, showMode = TRUE,
  xlab = expression(paste(alpha["accuracy"]) )
  )


## ----posterior-bias1, eval = TRUE, echo = TRUE----------------------------------------------------
# retrieves the posterior samples for the bias
posterior_intercept_bias <- post$b_bias_Intercept
posterior_slope_bias <- post$b_bias_condition1
# computes the posterior distribution in the speed condition
posterior_bias_speed <- plogis(posterior_intercept_bias - 0.5 * posterior_slope_bias)
# computes the posterior distribution in the accuracy condition
posterior_bias_accuracy <- plogis(posterior_intercept_bias + 0.5 * posterior_slope_bias)


## ----posterior-bias2, eval = TRUE, echo = FALSE, fig.asp = 1--------------------------------------
# plotting it
par(mfrow = c(2, 2) )
plotPost(
  plogis(posterior_intercept_bias), showMode = TRUE, compVal = 0.5,
  xlab = expression(paste(invlogit(beta[0][paste("[", alpha, "]")] ) ) )
  )
plotPost(
  exp(posterior_slope_bias), showMode = TRUE, compVal = 1,
  xlab = expression(paste(exp(beta["condition"][paste("[", alpha, "]")] ) ) )
  )
plotPost(
  posterior_bias_speed, showMode = TRUE, compVal = 0.5,
  xlab = expression(paste(alpha["speed"]) )
  )
plotPost(
  posterior_bias_accuracy, showMode = TRUE, compVal = 0.5,
  xlab = expression(paste(alpha["accuracy"]) )
  )


## ----echo = FALSE, out.width = "66%"--------------------------------------------------------------
knitr::include_graphics("figures/bayes_workflow_1.png")


## ----echo = FALSE, out.width = "50%"--------------------------------------------------------------
knitr::include_graphics("figures/bayes_workflow_2.png")

