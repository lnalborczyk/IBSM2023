## ----setup, eval = TRUE, include = FALSE, cache = FALSE-------------------------------------------------------------------------------
library(tidyverse)
library(patchwork)
library(brms)
library(imsb)

# setting up knitr options
knitr::opts_chunk$set(
  cache = TRUE, echo = TRUE,
  warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )

# setting up ggplot theme
theme_set(theme_bw(base_size = 16, base_family = "Open Sans") )


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## Reaction ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days | Subject)
## c(Reaction, Memory) ~ 1 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ 0 + Days + (1 + Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 | Subject)
## c(Reaction, Memory) ~ Days + (Days | Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## c(Reaction, Memory) ~ Days + (1 + Days || Subject)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## brm(Reaction ~ 1 + Days + (1 + Days | Subject), family = lognormal() )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
library(tidyverse)
library(imsb)

# import des données
data <- open_data(absence_multilevel) %>%
    mutate(reminder = ifelse(test = reminder == 1, yes = 0.5, no = -0.5) )

# on affiche 12 lignes "au hasard" dans ces données
data %>% sample_frac() %>% head(12)


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
prior1 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(lkj(2), class = cor)
    )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
mod1 <- brm(
    formula = presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior1,
    data = data,
    sample_prior = TRUE,
    warmup = 2000, iter = 10000,
    chains = 4, cores = parallel::detectCores(),
    control = list(adapt_delta = 0.95),
    backend = "cmdstanr"
    )


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------------------------------
mod1 %>%
    plot(
        combo = c("dens_overlay", "trace"), pars = c("^b_", "^cor_"), widths = c(1, 1.5),
        theme = theme_bw(base_size = 16, base_family = "Open Sans")
        )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
posterior_summary(x = mod1, pars = c("^b_", "^sd_") )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
a <- fixef(mod1)[1] # on récupère la valeur de l'intercept
exp(a) / (1 + exp(a) ) # on "convertit" l'intercept en probabilité (équivalent à plogis(a))


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
fixef(mod1)[2, c(1, 3, 4)] %>% exp()


## ---- echo = FALSE, eval = TRUE, message = FALSE, warning = FALSE, fig.width = 12, fig.height = 7-------------------------------------
library(tidybayes)
library(modelr)

data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_fitted_samples(mod1, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    group_by(reminder, .iteration) %>%
    summarise(estimate = mean(estimate) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.1) +
    labs(x = "Mail de rappel", y = "Pr(présent)")


## ---- echo = TRUE, eval = TRUE, message = FALSE, warning = FALSE, fig.width = 16, fig.height = 6--------------------------------------
data %>%
    group_by(researcher, total) %>%
    data_grid(reminder = seq_range(reminder, n = 1e2) ) %>%
    add_fitted_samples(mod1, newdata = ., n = 100, scale = "linear") %>%
    mutate(estimate = plogis(estimate) ) %>%
    ggplot(aes(x = reminder, y = estimate, group = .iteration) ) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_line(aes(y = estimate, group = .iteration), size = 0.5, alpha = 0.1) +
    facet_wrap(~researcher, nrow = 2) +
    labs(x = "Mail de rappel", y = "Pr(présent)")


## ---- echo = TRUE---------------------------------------------------------------------------------------------------------------------
(hyp1 <- hypothesis(x = mod1, hypothesis = "reminder = 0") ) # Savage-Dickey Bayes factor
1 / hyp1$hypothesis$Evid.Ratio # BF10 = 1 / BF01 (and BF01 = 1 / BF10)


## ---- echo = TRUE, fig.width = 10, fig.height = 7-------------------------------------------------------------------------------------
plot(hyp1, plot = FALSE, theme = theme_bw(base_size = 20, base_family = "Open Sans") )[[1]] +
  geom_vline(xintercept = 0, linetype = 2) +
  coord_cartesian(xlim = c(-5, 5) )


## ---- echo = TRUE, fig.width = 10, fig.height = 7-------------------------------------------------------------------------------------
library(bayestestR)
bf <- bayesfactor_parameters(posterior = mod1, null = 0)
plot(bf)


## ---- echo = TRUE, fig.width = 14, fig.height = 6-------------------------------------------------------------------------------------
data.frame(prior = hyp1$prior_samples$H1, posterior = hyp1$samples$H1) %>%
    gather(type, value) %>%
    mutate(type = factor(type, levels = c("prior", "posterior") ) ) %>%
    ggplot(aes(x = value) ) +
    geom_histogram(bins = 50, alpha = 0.8, col = "white", fill = "steelblue") +
    geom_vline(xintercept = 0, lty = 2, size = 1) +
    facet_wrap(~type, scales = "free") +
    labs(x = expression(beta[reminder]), y = "Nombre d'échantillons")


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
prior2 <- c(
    prior(normal(0, 10), class = Intercept, coef = ""),
    prior(cauchy(0, 10), class = sd),
    prior(lkj(2), class = cor) )

mod2 <- brm(presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior1,
    data = data,
    # this line is important for bridgesampling
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(), backend = "cmdstanr",
    control = list(adapt_delta = 0.95) )

mod3 <- brm(presence | trials(total) ~ 1 + (1 + reminder | researcher), 
    family = binomial(link = "logit"),
    prior = prior2,
    data = data,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(), backend = "cmdstanr",
    control = list(adapt_delta = 0.95) )


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## bayes_factor(mod2, mod3)


## ----eval = TRUE, echo = FALSE, results = "hide"--------------------------------------------------------------------------------------
bf <- bayes_factor(mod2, mod3)


## ----eval = TRUE, echo = FALSE--------------------------------------------------------------------------------------------------------
bf


## ---- echo = TRUE, eval = TRUE--------------------------------------------------------------------------------------------------------
waic(mod2, mod3, compare = FALSE)


## ---- echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------------------------------------------
data %>%
    ggplot(aes(x = presence / total) ) +
    geom_density(fill = "grey20")


## ---- echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------------------------------------------
pp_check(object = mod2, nsamples = 1e2)


## ---- echo = TRUE, fig.width = 12, fig.height = 8-------------------------------------------------------------------------------------
pp_check(object = mod2, nsamples = 1e3, type = "stat_2d")


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------------------------------------------
## mod2 <- brm(
##     formula = presence | trials(total) ~ 1 + reminder + (1 + reminder | researcher),
##     family = binomial(link = "logit"),
##     prior = prior2,
##     data = data,
##     warmup = 2000, iter = 1e4,
##     cores = parallel::detectCores(), # using all available cores
##     control = list(adapt_delta = 0.95) # adjusting the delta step size
##     )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
d <- open_data(meta)
head(d, 15)


## ----echo = FALSE, out.width = "1200px"-----------------------------------------------------------------------------------------------
knitr::include_graphics("figures/meta_structure.png")


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
prior4 <- c(
    prior(normal(0, 1), coef = intercept),
    prior(cauchy(0, 1), class = sd)
    )

mod4 <- brm(
    formula = yi | se(sqrt(vi) ) ~ 0 + intercept + (1 | study) + (1 | experiment),
    data = d,
    prior = prior4,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99),
    backend = "cmdstanr"
    )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
summary(mod4)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------------------------------
mod4 %>%
  plot(
    pars = c("^b_", "^sd_"),
    combo = c("dens_overlay", "trace"),
    theme = theme_bw(base_size = 16, base_family = "Open Sans")
    )


## ----eval = TRUE, echo = FALSE, out.width = "50%"-------------------------------------------------------------------------------------
# source("code/fplot2.R")
# fplot2(d, mod4, level = 0.95)
knitr::include_graphics("figures/forest.png")


## ----echo = FALSE, out.width = "66%"--------------------------------------------------------------------------------------------------
knitr::include_graphics("figures/bayes_workflow_1.png")


## ----echo = FALSE, out.width = "50%"--------------------------------------------------------------------------------------------------
knitr::include_graphics("figures/bayes_workflow_2.png")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
d <- open_data(popular)
head(d, 10)


## ----echo = FALSE, out.width = "500px"------------------------------------------------------------------------------------------------
knitr::include_graphics("figures/cat.gif")


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6-------------------------------------------------------------------------
d %>%
    ggplot(aes(x = popular) ) +
    geom_histogram() +
    facet_wrap(~sex) +
    scale_x_continuous(breaks = 1:10, limits = c(1, 10) )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6-------------------------------------------------------------------------
d %>%
    ggplot(aes(x = texp, y = popular) ) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "lm", colour = "black") +
    facet_wrap(~sex)


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
d <- d %>%
    mutate(
        # using a sum contrast for gender
        sex = ifelse(sex == "boy", -0.5, 0.5),
        # centering and standardising teacher experience
        texp = scale(texp) %>% as.numeric
        )

prior5 <- c(
    prior(normal(5, 2.5), class = Intercept),
    prior(cauchy(0, 10), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod5 <- brm(
    formula = popular ~ 1 + (1 | school),
    data = d,
    prior = prior5,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
prior6 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma)
    )

mod6 <- brm(
    formula = popular ~ 1 + texp + (1 | school),
    data = d,
    prior = prior6,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
prior7 <- c(
    prior(normal(0, 1), class = Intercept),
    prior(normal(0, 1), class = b),
    prior(cauchy(0, 1), class = sd),
    prior(cauchy(0, 10), class = sigma),
    prior(lkj(2), class = cor)
    )

mod7 <- brm(
    formula = popular ~ 1 + sex + texp + (1 + sex | school),
    data = d,
    prior = prior7,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
mod8 <- brm(
    formula = popular ~ 1 + sex + texp + sex:texp + (1 + sex | school),
    data = d,
    prior = prior7,
    save_all_pars = TRUE,
    warmup = 2000, iter = 1e4,
    cores = parallel::detectCores()
    )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
# calcul du WAIC et ajout du WAIC à chaque modèle
mod5 <- add_criterion(mod5, "waic")
mod6 <- add_criterion(mod6, "waic")
mod7 <- add_criterion(mod7, "waic")
mod8 <- add_criterion(mod8, "waic")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
# comparaison des WAIC de chaque modèle
model_comparison_table <- loo_compare(mod5, mod6, mod7, mod8, criterion = "waic") %>%
  data.frame() %>%
  rownames_to_column(var = "model")

weights <- data.frame(weight = model_weights(mod5, mod6, mod7, mod8, weights = "waic") ) %>%
  round(digits = 3) %>%
  rownames_to_column(var = "model")

left_join(model_comparison_table, weights, by = "model")


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5-------------------------------------------------------------------------
pp_check(object = mod8, nsamples = 1e2)


## ----eval = TRUE, echo = FALSE, fig.width = 14, fig.height = 6------------------------------------------------------------------------
library(patchwork)

p1 <-
  d %>% 
  ggplot(aes(x = popular, fill = ..x..) ) +
  geom_histogram(binwidth = 0.5, size = 0) +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  labs(x = "Popularité", y = "Nombre de réponses") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

p2 <-
  d %>%
  count(popular) %>%
  mutate(pr_k = n / nrow(d), cum_pr_k = cumsum(pr_k) ) %>% 
  ggplot(aes(x = popular, y = cum_pr_k, color = popular, fill = popular) ) +
  geom_line() +
  geom_point(shape = 21, color = "white", size = 2.5, stroke = 1) +
  labs(x = "Popularité", y = "Proportion cumulée") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

p3 <-
  d %>%
  count(popular) %>%
  mutate(cum_pr_k = cumsum(n / nrow(d) ) ) %>% 
  filter(popular < 9) %>% 
  ggplot(aes(
    x = popular, y = log(cum_pr_k / (1 - cum_pr_k) ),
    color = popular, fill = popular
    ) ) +
  geom_line() +
  geom_point(shape = 21, colour = "white", size = 2.5, stroke = 1) +
  labs(x = "Popularité", y = "Log cote cumulée") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  scale_x_continuous(breaks = 1:10, limits = c(1, 10) ) +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )

(p1 | p2 | p3)


## ----eval = TRUE, echo = FALSE, fig.width = 6, fig.height = 6-------------------------------------------------------------------------
d_plot <- d %>%
  count(popular) %>%
  mutate(pr_k = n / nrow(d), cum_pr_k = cumsum(n / nrow(d) ) ) %>%
  mutate(discrete_probability = ifelse(popular == 1, cum_pr_k, cum_pr_k - pr_k) )

text <- tibble(
  text = 2:9,
  popular = seq(from = 2.25, to = 9.25, by = 1),
  cum_pr_k = d_plot$cum_pr_k - 0.065
  )

d_plot %>% 
  ggplot(aes(x = popular, y = cum_pr_k, color = cum_pr_k, fill = cum_pr_k) ) +
  geom_line() +
  geom_point(shape = 21, colour = "white", size = 2.5, stroke = 1) +
  geom_linerange(aes(ymin = 0, ymax = cum_pr_k), alpha = 0.5) +
  geom_linerange(
    aes(
      x = popular + .025,
      ymin = ifelse(popular == 1, 0, discrete_probability),
      ymax = cum_pr_k),
    color = "black"
    ) +
  geom_text(data = text,aes(label = text), size = 4) +
  scale_x_continuous(breaks = 2:9) +
  labs(x = "Popularité", y = "Proportion cumulée") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  theme(
    axis.title.y = element_text(angle = 90),
    legend.position = "none"
    )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
mod9 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    prior = prior6,
    warmup = 2000, iter = 5000,
    chains = 4, cores = parallel::detectCores(),
    file = "models/mod9", backend = "cmdstanr"
    )


## ----eval = TRUE, echo = TRUE, results = "hide"---------------------------------------------------------------------------------------
prior10 <- c(
    brms::prior(normal(0, 10), class = Intercept),
    brms::prior(normal(0, 10), class = b),
    brms::prior(cauchy(0, 10), class = sd)
    )

mod10 <- brm(
    popular ~ 1 + sex + texp + sex:texp + (1 | school),
    data = d,
    family = cumulative(link = "logit"),
    prior = prior10,
    chains = 4, cores = parallel::detectCores(),
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    file = "models/mod10", backend = "cmdstanr"
    )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------------------------------------------
waic(mod9, mod10, compare = FALSE)


## ----eval = TRUE, echo = TRUE, fig.width = 12, fig.height = 6-------------------------------------------------------------------------
pp_check(mod10, nsamples = 1e2, type = "bars", prob = 0.95, freq = FALSE) +
  scale_x_continuous(breaks = 1:9) +
  labs(x = "Popularité", y = "Proportion")

