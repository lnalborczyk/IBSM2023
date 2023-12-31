## ----setup, eval = TRUE, include = FALSE, cache = FALSE-------------------------------------------
library(countdown)
library(tidyverse)
library(MetBrewer)
library(knitr)

# setting up knitr options
knitr::opts_chunk$set(
  cache = TRUE, echo = TRUE,
  warning = FALSE, message = FALSE,
  fig.align = "center", dev = "svg"
  )

# setting up ggplot theme
theme_set(theme_bw(base_size = 16, base_family = "Open Sans") )


## ----jquery, echo=FALSE---------------------------------------------------------------------------
# solution from https://github.com/jhelvy/renderthis/issues/46
htmltools::tagList(rmarkdown::html_dependency_jquery())


## ----frequency, fig.width = 7.5, fig.height = 5, `code-line-numbers` = "|3"-----------------------
library(tidyverse)

sample(x = c(0, 1), size = 500, prob = c(0.5, 0.5), replace = TRUE) %>%
        data.frame() %>%
        mutate(x = seq_along(.), y = cummean(.) ) %>%
        ggplot(aes(x = x, y = y) ) +
        geom_line(lwd = 1) +
        geom_hline(yintercept = 0.5, lty = 3) +
        labs(x = "Trial number", y = "Proportion of heads") +
        ylim(0, 1)


## ----joint-prob-plot------------------------------------------------------------------------------
#| echo: FALSE
#| fig-width: 6
#| fig-height: 6
crossing(X = 1:6, Y = 1:6) %>%
    mutate(Z = ifelse(X == 2 & Y == 3, 1, 0) ) %>%
    ggplot(aes(X, Y, fill = Z) ) +
    geom_tile(colour = "white", show.legend = FALSE) +
    scale_x_continuous(expand = c(0, 0), n.breaks = 6) +
    scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
    theme_bw(base_size = 16, base_family = "Open Sans") +
    labs(x = "Die x", y = "Die y")


## ----marginalisation-plot1------------------------------------------------------------------------
#| echo: FALSE
#| fig-width: 6
#| fig-height: 6
crossing(X = 1:6, Y = 1:6) %>%
    mutate(Z = ifelse(X == 1, 1, 0) ) %>%
    ggplot(aes(X, Y, fill = Z) ) +
    geom_tile(colour = "white", show.legend = FALSE) +
    scale_x_continuous(expand = c(0, 0), n.breaks = 6) +
    scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
    theme_bw(base_size = 16, base_family = "Open Sans") +
    labs(x = "Die x", y = "Die y")


## ----marginalisation-plot2------------------------------------------------------------------------
#| echo: FALSE
#| fig-width: 6
#| fig-height: 6
crossing(X = 1:6, Y = 1:6) %>%
    mutate(Z = ifelse(X + Y == 4, 1, 0) ) %>%
    ggplot(aes(X, Y, fill = Z) ) +
    geom_tile(colour = "white", show.legend = FALSE) +
    scale_x_continuous(expand = c(0, 0), n.breaks = 6) +
    scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
    theme_bw(base_size = 16, base_family = "Open Sans") +
    labs(x = "Die x", y = "Die y")


## ----marginalisation-plot3------------------------------------------------------------------------
#| echo: FALSE
#| fig-width: 6
#| fig-height: 6
crossing(X = 1:6, Y = 1:6) %>%
    mutate(
        Z = case_when(
            X == 2 & X + Y == 4 ~ 1,
            X + Y == 4 ~ 0,
            .default = NA
            )
        ) %>%
    ggplot(aes(X, Y, fill = Z) ) +
    geom_tile(colour = "white", show.legend = FALSE) +
    scale_x_continuous(expand = c(0, 0), n.breaks = 6) +
    scale_y_continuous(expand = c(0, 0), n.breaks = 6) +
    theme_bw(base_size = 16, base_family = "Open Sans") +
    labs(x = "Die x", y = "Die y")


## ----echo = FALSE, fig.align = "center", dev = "svg", fig.width = 6, fig.height = 4, cache = FALSE----
coin <- dbinom(x = 0:10, size = 10, prob = 0.5)
barplot(coin, names.arg = 0:10, border = NA, axes = FALSE, cex.names = 1.5, col = "grey20")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
# PMFs sum to 1
dbinom(x = 0:10, size = 10, prob = 0.5) %>% sum


## ----echo = FALSE, fig.align = "center", dev = "svg", fig.width = 6, fig.height = 5---------------
data.frame(x = c(0, 200) ) %>%
    ggplot(aes(x) ) +
    stat_function(
        fun = dnorm,
        args = list(mean = 100, sd = 15),
        lwd = 1.5
        ) +
    labs(x = "IQ", y = "Probability density")


## ----echo = TRUE----------------------------------------------------------------------------------
# PDFs integrate to 1
integrate(dnorm, -Inf, Inf, mean = 100, sd = 15)


## ----message = FALSE, echo = FALSE, dev = "svg", fig.align = "center", fig.width = 7, fig.height = 5----
cord.x <- c(90, seq(90, 96, 0.01), 96) 
cord.y <- c(0, dnorm(seq(90, 96, 0.01), 100, 15), 0) 

data.frame(x = c(0, 200) ) %>%
    ggplot(aes(x) ) +
    stat_function(
        fun = dnorm,
        args = list(mean = 100, sd = 15),
        lwd = 2,
        color = "black"
        ) +
    geom_polygon(
        data = data.frame(cord.x, cord.y),
        aes(cord.x, cord.y),
        color = "black"
        ) +
    labs(x = "IQ", y = "Probability density")


## ----message = FALSE, echo = FALSE, fig.align = "center", dev = "svg", fig.width = 7, fig.height = 5----
data.frame(x = c(0, 200) ) %>%
    ggplot(aes(x) ) +
    stat_function(
        fun = dnorm,
        args = list(mean = 100, sd = 15),
        lwd = 2
        ) +
    geom_polygon(
        data = data.frame(cord.x, cord.y),
        aes(cord.x, cord.y)
        ) +
    labs(x = "QI", y = "Densité de probabilité")


## ----eval = TRUE, echo = TRUE, fig.align = "center", fig.width = 8, fig.height = 6----------------
integrate(dnorm, 90, 96, mean = 100, sd = 15)


## ----countown, echo = FALSE, cache = FALSE--------------------------------------------------------
countdown(
    minutes = 2,
    warn_when = 30,
    left = 0, right = 0,
    padding = "10px",
    margin = "5%",
    font_size = "5em",
    color_border = "#1c5253",
    color_text = "#1c5253"
    )


## ----echo = FALSE, eval = TRUE, fig.height = 10, fig.width = 10, fig.align = "center"-------------
library(rethinking)
source("./code/forking_data_McElreath.R")

dat <- c(1)
arc <- c(0, pi)

garden(
    arc = arc,
    possibilities = c(0, 0, 0, 1),
    data = dat,
    hedge = 0.05,
    ring_dist = ring_dist,
    alpha.fade = 1
    )


## ----echo = FALSE, eval = TRUE, fig.height = 10, fig.width = 10, fig.align = "center"-------------
dat <- c(1, 0)
arc <- c(0, pi)

garden(
    arc = arc,
    possibilities = c(0, 0, 0, 1),
    data = dat,
    hedge = 0.05,
    ring_dist = ring_dist,
    alpha.fade = 1
    )


## ----echo = FALSE, eval = TRUE, fig.height = 10, fig.width = 10, fig.align = "center"-------------
dat <- c(1, 0, 1)
arc <- c(0, pi)

garden(
    arc = arc,
    possibilities = c(0, 0, 0, 1),
    data = dat,
    hedge = 0.05,
    ring_dist = ring_dist,
    alpha.fade = 1
    )


## ----echo = FALSE, eval = TRUE, fig.height = 10, fig.width = 10, fig.align = "center"-------------
dat <- c(1, 0, 1)
arc <- c(0, pi)

garden(
    arc = arc,
    possibilities = c(0, 0, 0, 1),
    data = dat,
    hedge = 0.05,
    ring_dist = ring_dist,
    alpha.fade = 0.3
    )


## ----echo = FALSE, eval = TRUE, fig.height = 7, fig.width = 7, fig.align = "center"---------------
dat <- c(1, 0, 1)
ac <- c(1.2, 0.9, 0.6)

arc <- c( pi / 2, pi / 2 + (2 / 3) * pi)

garden(
    arc = arc,
    possibilities = c(1, 0, 0, 0),
    data = dat,
    hedge = 0.05,
    adj.cex = ac
    ) 

arc <- c(arc[2], arc[2] + (2 / 3) * pi)

garden(
    arc = arc,
    possibilities = c(1, 1, 0, 0),
    data = dat,
    hedge = 0.05,
    newplot = FALSE,
    adj.cex = ac
    )

arc <- c(arc[2], arc[2] + (2 / 3) * pi)

garden(
    arc = arc,
    possibilities = c(1, 1, 1, 0),
    data = dat,
    hedge = 0.05,
    newplot = FALSE,
    adj.cex = ac
    )

line.polar(c(0, 2), pi / 2, lwd = 1)
line.polar(c(0, 2), pi / 2 + (2 / 3) * pi, lwd = 1)
line.polar(c(0, 2), pi / 2 + 2 * (2 / 3) * pi, lwd = 1)


## ----echo = TRUE, eval = TRUE---------------------------------------------------------------------
ways <- c(0, 3, 8, 9, 0)
ways / sum(ways)


## ----diagram, eval = FALSE, echo = FALSE----------------------------------------------------------
## # check https://cran.r-project.org/web/packages/ggparty/vignettes/ggparty-graphic-partying.html


## ----echo = TRUE----------------------------------------------------------------------------------
prior <- c(0.008, 0.992)


## ----echo = TRUE----------------------------------------------------------------------------------
like <- rbind(c(0.9, 0.1), c(0.07, 0.93) ) %>% data.frame
colnames(like) <- c("Mam+", "Mam-")
rownames(like) <- c("Cancer+", "Cancer-")
like


## ----echo = TRUE----------------------------------------------------------------------------------
(marginal <- sum(like$"Mam+" * prior) )


## ----echo = TRUE----------------------------------------------------------------------------------
(posterior <- (like$"Mam+" * prior ) / marginal )


## ----binomialcoef---------------------------------------------------------------------------------
#| output-location: fragment
# computing the total number of possible configurations in R
choose(n = 3, k = 1)


## ----echo = FALSE, fig.align = "center", dev = "svg", fig.width = 7.5, fig.height = 5, cache = FALSE----
coin <- dbinom(x = 0:10, size = 10, prob = 0.5)

barplot(
  coin, names.arg = 0:10, border = NA, axes = FALSE,
  cex.names = 1.5, col = "grey20", family = "Open Sans"
  )


## ----berndata, out.width = "50%", `code-line-numbers` = "|4"--------------------------------------
library(tidyverse)
set.seed(666) # for reproducibility

sample(x = c(0, 1), size = 500, prob = c(0.4, 0.6), replace = TRUE) %>% # theta = 0.6
        data.frame() %>%
        mutate(x = seq_along(.), y = cummean(.) ) %>%
        ggplot(aes(x = x, y = y) ) +
        geom_line(lwd = 1) +
        geom_hline(yintercept = 0.6, lty = 3) +
        labs(x = "Number of trials", y = "Proportion of heads") +
        ylim(0, 1)


## ----likelihood, echo = TRUE, eval = TRUE, fig.width = 10, fig.height = 5, `code-line-numbers` = "|7"----
# Graphical representation of the likelihood function for y = 1 and n = 2

y <- 1 # number of heads
n <- 2 # number of trials

data.frame(theta = seq(from = 0, to = 1, length.out = 1e3) ) %>%
  mutate(likelihood = dbinom(x = y, size = n, prob = theta) ) %>%
  ggplot(aes(x = theta, y = likelihood) ) +
  geom_area(color = "orangered", fill = "orangered", alpha = 0.5) +
  labs(x = expression(paste(theta, " - Pr(head)") ), y = "Likelihood")


## ----echo = TRUE----------------------------------------------------------------------------------
f <- function(theta) {2 * theta * (1 - theta) }
integrate(f = f, lower = 0, upper = 1)


## ----echo = FALSE, results = "asis"---------------------------------------------------------------
library(kableExtra)
library(knitr)

data.frame(
  theta = as.character(c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0) ),
  x0 = c(1.0, 0.64, 0.36, 0.16, 0.04, 0.00),
  x1 = c(0.0, 0.32, 0.48, 0.48, 0.32, 0.00),
  x2 = c(0.0, 0.04, 0.16, 0.36, 0.64, 1.00)
  ) %>%
  mutate(Total = rowSums(.[2:4]) %>% as.character) %>%
  tibble::add_row(
    theta = "Total", x0 = sum(.$x0), x1 = sum(.$x1), x2 = sum(.$x2),
    Total = ""
    ) %>%
  kable(
    col.names = c("theta", "0", "1", "2", "Total"),
    format = "html",
    align = c("c", "c", "c", "c", "c"),
    # caption = "Vraisemblance versus probabilité pour deux lancers de pièce",
    caption = "Likelihood versus probability for two coin tosses",
    escape = FALSE
    ) %>%
  kable_styling(full_width = TRUE, position = "center") %>%
  add_header_above(c(" " = 1, "Number of Heads (y)" = 3, " " = 1) )


## ----beta1, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------
p <- seq(0, 1, length = 100)

data.frame(
  p = p,
  p1 = dbeta(p, 1, 1),
  p2 = dbeta(p, 2, 2),
  p3 = dbeta(p, 4, 2),
  p4 = dbeta(p, 2, 4)
  ) %>%
  pivot_longer(p1:p4, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_discrete(
    name = "Parameters",
    labels = c(
      "a = 1, b = 1",
      "a = 2, b = 2",
      "a = 4, b = 2",
      "a = 2, b = 4"
      )
    )


## ----beta2, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------
p <- seq(0, 1, length = 100)

data.frame(
  p = p,
  p1 = dbeta(p, 1, 1),
  p2 = dbeta(p, 2, 2),
  p3 = dbeta(p, 4, 2),
  p4 = dbeta(p, 2, 4)
  ) %>%
  pivot_longer(p1:p4, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_discrete(
    name = "Parameters",
    labels = c(
      "a = 1, b = 1",
      "a = 2, b = 2",
      "a = 4, b = 2",
      "a = 2, b = 4"
      )
    )


## ----beta3, echo = FALSE, fig.width = 10, fig.height = 5------------------------------------------
p <- seq(0, 1, length = 100)

data.frame(
  p = p,
  p1 = dbeta(p, 1, 1),
  p2 = dbeta(p, 5, 5),
  p3 = dbeta(p, 50, 50)
  ) %>%
  pivot_longer(p1:p3, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_discrete(
    name = "Parameters",
    labels = c(
      "a = 1, b = 1",
      "a = 5, b = 5",
      "a = 50, b = 50"
      )
    )


## ----beta4, echo = FALSE, fig.width = 12, fig.height = 5------------------------------------------
p <- seq(0, 1, length = 100)

W <- 0.65
K1 <- 25
K2 <- 10

data.frame(
  p = p,
  p1 = dbeta(p, W * (K1 - 2) + 1, (1 - W) * (K1 - 2) + 1),
  p2 = dbeta(p, W * (K2 - 2) + 1, (1 - W) * (K2 - 2) + 1)
  ) %>%
  pivot_longer(p1:p2, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = p, y = prob / sum(prob), color = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.6) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_discrete(
    name = "Parameters",
    labels = c(
      expression(paste(omega, " = 0.65 ", kappa, " = 25", sep = "") ),
      expression(paste(omega, " = 0.65 ", kappa, " = 10", sep = "") )
      )
    )


## ----beta-exemple, echo = FALSE, fig.width = 9, fig.height = 9------------------------------------
library(patchwork)

df <- data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, 1, 1),
    likelihood = dbinom(7, 10, p)
    ) %>%
  mutate(posterior = (prior * likelihood) )

p1 <- df %>%
  ggplot(aes(x = theta, y = prior, colour = NULL) ) +
  geom_area(
    fill = "steelblue",
    position = "identity", alpha = 0.5
    ) +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  ylim(0, 2) +
  ggtitle("Prior distribution Beta(1, 1)")

p2 <- df %>%
  ggplot(aes(x = theta, y = likelihood, colour = NULL) ) +
  geom_area(
    fill = "orangered",
    position = "identity", alpha = 0.5
    ) +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  ggtitle("Likelihood function Bin(7, 10)")

p3 <- df %>%
  ggplot(aes(x = theta, y = posterior, colour = NULL) ) +
  geom_area(
    fill = "purple",
    position = "identity", alpha = 0.5
    ) +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  ggtitle("Posterior distribution Beta(8, 4)")

p1 / p2 / p3


## ----posterior-exemple1, echo = FALSE, fig.width = 12, fig.height = 8-----------------------------
a <- 4
b <- 16
y <- 6
n <- 10

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple2, echo = FALSE, fig.width = 12, fig.height = 8-----------------------------
a <- 4
b <- 16
y <- 12
n <- 20

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple3, echo = FALSE, fig.width = 12, fig.height = 8-----------------------------
a <- 4
b <- 16
y <- 24
n <- 40

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple4, echo = FALSE, fig.width = 14, fig.height = 7-----------------------------
a <- 3
b <- 17
y <- 8
n <- 10

data.frame(theta = seq(0, 1, length = 100) ) %>%
  mutate(
    prior = dbeta(p, a, b),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----posterior-exemple5, echo = FALSE, fig.width = 12, fig.height = 6-----------------------------
p <- seq(0, 1, length = 1e3)

y <- 8
n <- 10

data.frame(theta = seq(0, 1, length = 1e3) ) %>%
  mutate(
    prior = dunif(p, 0.25, 0.75),
    likelihood = dbinom(y, n, p)
    ) %>%
  mutate(prior = prior / sum(prior) ) %>%
  mutate(likelihood = likelihood / sum(likelihood) ) %>%
  mutate(posterior = (prior * likelihood) / sum(prior * likelihood) ) %>%
  pivot_longer(prior:posterior, names_to = "params", values_to = "prob") %>%
  ggplot(aes(x = theta, y = prob, colour = NULL, fill = params) ) +
  geom_area(position = "identity", alpha = 0.5) +
  xlab(expression(theta) ) +
  ylab("Probability density") +
  scale_fill_manual(
    name = "",
    labels = c("Likelihood", "Posterior", "Prior"),
    values = c("orangered", "purple", "steelblue")
    )


## ----discrete, echo = FALSE, out.width = "100%"---------------------------------------------------
knitr::include_graphics("figures/discrete.png")


## ----continuous, echo = FALSE, out.width = "100%"-------------------------------------------------
knitr::include_graphics("figures/continuous.png")


## ----grid1, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------
thetaSize <- 30

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize),
  pTheta = rep(0.03, thetaSize)
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = pTheta), size = 1, colour = "#339900") +
  ylim(0, 0.15) +
  labs(x = expression(theta), y = "Probability density")


## ----grid2, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------
thetaSize <- 30
a <- 3
b <- 7

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  labs(x = expression(theta), y = "Probability density")


## ----grid3, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------
thetaSize <- 30
a <- 3
b <- 7
y <- 12
n <- 20

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  mutate(
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  geom_segment(aes(
    x = theta + 0.01, yend = likelihood, xend = theta + 0.01),
    colour = "orangered", size = 1
    ) +
  labs(x = expression(theta), y = "Probability density")


## ----grid4, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------
thetaSize <- 30
a <- 3
b <- 7
y <- 12
n <- 20

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  mutate(
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
  ) %>%
  mutate(
    posterior = (prior * likelihood) / sum(prior * likelihood)
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  geom_segment(aes(
    x = theta + 0.01, yend = likelihood, xend = theta + 0.01),
    colour = "orangered", size = 1
    ) +
  geom_segment(aes(
    x = theta - 0.01, yend = posterior, xend = theta - 0.01),
    colour = "purple", size = 1
    ) +
  labs(x = expression(theta), y = "Probability density")


## ----grid5, echo = FALSE, fig.width = 12, fig.height = 6------------------------------------------
thetaSize <- 100
a <- 3
b <- 7
y <- 12
n <- 20

data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, 3, 7) / sum(dbeta(theta, 3, 7) )
  ) %>% 
  mutate(
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
  ) %>%
  mutate(
    posterior = (prior * likelihood) / sum(prior * likelihood)
  ) %>% 
  ggplot(aes(x = theta, y = 0, xend = theta) ) +
  geom_segment(aes(yend = prior), size = 1, colour = "steelblue") +
  geom_segment(aes(
    x = theta + 0.01, yend = likelihood, xend = theta + 0.01),
    colour = "orangered", size = 1
    ) +
  geom_segment(aes(
    x = theta - 0.01, yend = posterior, xend = theta - 0.01),
    colour = "purple", size = 1
    ) +
  labs(x = expression(theta), y = "Probability density")


## ----work-gif, echo = FALSE, out.width = "500px"--------------------------------------------------
knitr::include_graphics("figures/not_gonna_work.gif")


## ----sampling1, eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 3, `code-line-numbers` = "|5"----
#| output-location: fragment
p_grid <- seq(from = 0, to = 1, length.out = 1000) # creates a grid
prior <- rep(1, 1000) # uniform prior
likelihood <- dbinom(x = 12, size = 20, prob = p_grid) # computes likelihood
posterior <- (likelihood * prior) / sum(likelihood * prior) # computes posterior
samples <- sample(x = p_grid, size = 1e3, prob = posterior, replace = TRUE) # sampling
hist(samples, main = "", xlab = expression(theta) ) # histogram


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## a <- b <- 1 # parameters of the Beta prior
## n <- 9 # number of observations
## y <- 6 # number of successes
## p_grid <- seq(from = 0, to = 1, length.out = 1000)
## posterior <- dbeta(p_grid, y + a, n - y + b) # plot(posterior)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## p_grid <- seq(from = 0, to = 1, length.out = 1000)
## prior <- rep(1, 1000) # uniform prior
## likelihood <- dbinom(x = y, size = n, prob = p_grid)
## posterior <- (likelihood * prior) / sum(likelihood * prior) # plot(posterior)


## ----eval = FALSE, echo = TRUE--------------------------------------------------------------------
## samples <- sample(x = p_grid, size = 1e4, prob = posterior, replace = TRUE) # hist(samples)


## ----tendance-centrale1, eval = FALSE, echo = TRUE------------------------------------------------
## mode_posterior <- find_mode(samples) # in blue
## mean_posterior <- mean(samples) # in orange
## median_posterior <- median(samples) # in green


## ----tendance-centrale2, echo = FALSE-------------------------------------------------------------
library(imsb)
set.seed(666)
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, 1000)
likelihood <- dbinom(x = 3, size = 10, prob = p_grid)
posterior <- (likelihood * prior) / sum(likelihood * prior)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = posterior)
mode_posterior <- find_mode(samples)
mean_posterior <- mean(samples)
median_posterior <- median(samples)


## ----tendance-centrale3, echo = FALSE, fig.width = 18, fig.height = 6-----------------------------
data.frame(samples = samples) %>%
  ggplot(aes(x = samples) ) +
  geom_histogram(aes(y = ..density..), fill = "purple", alpha = 0.25, bins = 50) +
  geom_vline(aes(xintercept = mode_posterior), size = 1, color = "steelblue") +
  annotate(
    geom = "text",
    x = mode_posterior - 0.01, # y = max(posterior) / 2,
    y = 1,
    label = "mode", color = "steelblue", angle = 90, size = 5
    ) +
  geom_vline(aes(xintercept = mean_posterior), size = 1, color = "orangered") +
  annotate(
    geom = "text",
    x = mean_posterior + 0.01, # y = max(posterior) / 2,
    y = 1,
    label = "mean", color = "orangered", angle = 90, size = 5
    ) +
  geom_vline(aes(xintercept = median_posterior), size = 1, color = "forestgreen") +
  annotate(
    geom = "text",
    x = median_posterior - 0.01, # y = max(posterior) / 2,
    y = 1,
    label = "median", color = "forestgreen", angle = 90, size = 5
    ) +
  xlab(expression(theta) ) + ylab("Probability density")

# data.frame(theta = p_grid, posterior = posterior) %>% 
#   ggplot(aes(x = theta, y = posterior, colour = NULL) ) +
#   geom_area(position = "identity", fill = "purple", alpha = 0.25) +
#   geom_vline(aes(xintercept = mode_posterior), size = 1, color = "steelblue") +
#   annotate(
#     geom = "text",
#     x = mode_posterior - 0.01, y = max(posterior) / 2,
#     label = "mode", color = "steelblue", angle = 90, size = 5
#     ) +
#   geom_vline(aes(xintercept = mean_posterior), size = 1, color = "orangered") +
#   annotate(
#     geom = "text",
#     x = mean_posterior + 0.01, y = max(posterior) / 2,
#     label = "moyenne", color = "orangered", angle = 90, size = 5
#     ) +
#   geom_vline(aes(xintercept = median_posterior), size = 1, color = "forestgreen") +
#   annotate(
#     geom = "text",
#     x = median_posterior - 0.01, y = max(posterior) / 2,
#     label = "médiane", color = "forestgreen", angle = 90, size = 5
#     ) +
#   xlab(expression(theta) ) + ylab("Densité de probabilité")


## ----superiority-prob, eval = TRUE, echo = TRUE---------------------------------------------------
sum(samples > 0.5) / length(samples) # equivalent to mean(samples > 0.5)


## ----interval-prob, eval = TRUE, echo = TRUE------------------------------------------------------
sum(samples > 0.2 & samples < 0.4) / length(samples)


## ----interval-prob-plot, echo = FALSE, fig.width = 12, fig.height = 4-----------------------------
df <- data.frame(theta = p_grid, posterior = posterior)

ggplot(df, aes(x = theta, y = posterior, colour = NULL) ) +
    geom_area(
    position = "identity", fill = "purple", alpha = 0.5
    ) +
  geom_area(
    data = subset(df, theta > 0.4),
    position = "identity", fill = "purple", alpha = 0.5
    ) +
  geom_area(
    data = subset(df, 0.2 > theta),
    position = "identity", fill = "purple", alpha = 0.5
    ) +
  xlab(expression(theta) ) + ylab("Probability density")


## ----hdi-plot, echo = FALSE, out.width = "35%"----------------------------------------------------
knitr::include_graphics("figures/HDI.png")


## ----eval = TRUE, echo = TRUE, fig.width = 8, fig.height = 6, dev = "png", dpi = 200--------------
library(imsb)

set.seed(666)
p_grid <- seq(from = 0, to = 1, length.out = 1e3)
pTheta <- dbeta(p_grid, 3, 10)
massVec <- pTheta / sum(pTheta)
samples <- sample(x = p_grid, size = 1e4, replace = TRUE, prob = pTheta)

posterior_plot(samples = samples, credmass = 0.89)


## ----eval = TRUE, echo = FALSE, fig.align = "center", out.width = "50%"---------------------------
knitr::include_graphics("figures/hdi_rope.png")


## ----eval = TRUE, echo = FALSE--------------------------------------------------------------------
set.seed(666)
p_grid <- seq(from = 0, to = 1, length.out = 1e3)
pTheta <- dbeta(p_grid, 3, 10)
massVec <- pTheta / sum(pTheta)
samples <- sample(p_grid, size = 1e4, replace = TRUE, prob = pTheta)


## ----eval = TRUE, echo = TRUE, fig.width = 7.5, fig.height = 5, dev = "png", dpi = 200------------
posterior_plot(samples = samples, rope = c(0.49, 0.51) ) +
    labs(x = expression(theta) )


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
samples <- rbinom(n = 1e4, size = 10, prob = 0.6)


## ----eval = TRUE, echo = TRUE, `code-line-numbers` = "|2"-----------------------------------------
posterior <- rbeta(n = 1e4, shape1 = 16, shape2 = 10)
samples <- rbinom(n = 1e4, size = 10, prob = posterior)


## ----ppc, echo = FALSE, fig.width = 12, fig.height = 8--------------------------------------------
thetaSize <- 1e3
a <- 3
b <- 7
y <- 9
n <- 10

df <- data.frame(
  theta = seq(from = 0, to = 1, length.out = thetaSize)
  ) %>%
  mutate(
    prior = dbeta(theta, a, b) / sum(dbeta(theta, a, b) ),
    likelihood = dbinom(y, n, theta) / sum(dbinom(y, n, theta) )
    ) %>%
  mutate(
    unnormalised_posterior = prior * likelihood,
    posterior = unnormalised_posterior / sum(unnormalised_posterior)
    ) %>%
  mutate(
    # prior predictive distribution
    ppc1 = rbinom(thetaSize, n, rbeta(thetaSize, a, b) ),
    # posterior predictive distribution
    ppc2 = rbinom(thetaSize, n, rbeta(thetaSize, y + a, n - y + b) )
    )
  
p1 <- df %>%
  ggplot(aes(x = theta, y = prior) ) +
  geom_area(aes(colour = NULL), stat = "identity", fill = "steelblue", alpha = 0.8) +
  labs(
    x = expression(theta), y = "Probability density",
    title = "Prior distribution",
    subtitle = "rbeta(n = 1e4, shape1 = 3, shape2 = 7)"
    ) +
  theme_bw(base_size = 16, base_family = "Open Sans")

p2 <- df %>%
  ggplot(aes(x = ppc1) ) +
  geom_histogram(aes(colour = NULL), fill = "black") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  labs(
    x = "Number of Heads",
    y = "Number of samples",
    title = "Prior predictive distribution",
    subtitle = "rbinom(n = 1e4, size = 10, prob = prior)"
    ) +
  scale_x_continuous(breaks = seq(0, 10, 1), labels = seq(0, 10, 1) )

p3 <- df %>%
  ggplot(aes(x = theta, y = posterior) ) +
  geom_area(aes(colour = NULL), stat = "identity", fill = "purple", alpha = 0.8) +
  labs(
    x = expression(theta), y = "Probability density",
    title = "Posterior distribution",
    subtitle = "rbeta(n = 1e4, shape1 = 12, shape2 = 8)"
    ) +
  theme_bw(base_size = 16, base_family = "Open Sans")

p4 <- df %>%
  ggplot(aes(x = ppc2) ) +
  geom_histogram(aes(colour = NULL), fill = "black") +
  theme_bw(base_size = 16, base_family = "Open Sans") +
  labs(
    x = "Number of Heads",
    y = "Number of samples",
    title = "Posterior predictive distribution",
    subtitle = "rbinom(n = 1e4, size = 10, prob = posterior)"
    ) +
  scale_x_continuous(breaks = seq(0, 10, 1), labels = seq(0, 10, 1) )

library(patchwork)

(p1 | p2) / (p3 | p4) # stacking plots


## ----model-ppc, echo = FALSE, out.width = "75%"---------------------------------------------------
knitr::include_graphics("figures/ModelPredictions.jpg")


## ----tartine, echo = FALSE, out.width = "25%"-----------------------------------------------------
knitr::include_graphics("figures/tartine.jpg")


## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
# importing the data
data <- open_data(tartine1)

# summary of the data
str(data)


## ----eval = TRUE, echo = TRUE, fig.width = 6, fig.height = 4--------------------------------------
# number of trials
nbTrial <- length(data$trial)

# number of "successes" (i.e., when the toast lands on the butter side)
nbSuccess <- sum(data$side)

# size of the grid
grid_size <- 1e3

# generating the grid
p_grid <- seq(from = 0, to = 1, length.out = grid_size)

# uniform prior
prior <- rep(1, grid_size)

# computing the likelihod
likelihood <- dbinom(x = nbSuccess, size = nbTrial, prob = p_grid)

# computing the posterior
posterior <- likelihood * prior / sum(likelihood * prior)


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6, dev = "png", dpi = 200-------------
samples <- sample(x = p_grid, prob = posterior, size = 1e3, replace = TRUE)
posterior_plot(samples = samples, credmass = 0.95) + labs(x = expression(theta) )


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6, dev = "png", dpi = 200-------------
posterior_plot(
  samples = samples, credmass = 0.95,
  compval = 0.5, rope = c(0.49, 0.51)
  ) + labs(x = expression(theta) )



## ----eval = TRUE, echo = TRUE---------------------------------------------------------------------
data2 <- open_data(tartine2)
str(data2)
nbTrial2 <- length(data2$trial) # number of trials
nbSucces2 <- sum(data2$side) # number of "successes"


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 5-------------------------------------
mode1 <- find_mode(samples)
prior2 <- dbeta(p_grid, mode1 * (nbTrial - 2) + 1, (1 - mode1) * (nbTrial - 2) + 1)

data.frame(x = p_grid, y = prior2) %>%
  ggplot(aes(x = x, y = y) ) +
  geom_area(alpha = 0.8, fill = "steelblue") +
  geom_line(size = 0.8) +
  labs(x = expression(theta), y = "Probability density")


## ----eval = TRUE, echo = TRUE, fig.width = 10, fig.height = 6, dev = "png", dpi = 200-------------
likelihood2 <- dbinom(x = nbSucces2, size = nbTrial2, prob = p_grid)
posterior2 <- likelihood2 * prior2 / sum(likelihood2 * prior2)
samples2 <- sample(p_grid, prob = posterior2, size = 1e4, replace = TRUE)

posterior_plot(
  samples = samples2, credmass = 0.95,
  compval = 0.5, rope = c(0.49, 0.51)
  ) + labs(x = expression(theta) )

