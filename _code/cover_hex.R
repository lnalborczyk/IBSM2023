library(hexSticker)
library(tidyverse)

# number of trial
thetaSize <- 100     

# beta first parameter
a <- 4

# beta second parameter
b <- 16

# specify the total number of flips
N <- 40

# number of "Face"
z <- 24

p <- tibble(
    theta = seq(from = 0, to = 1, length.out = thetaSize),
    prior = dbeta(theta, a, b),
    likelihood = (theta^(z) ) * (1 - theta)^(N - z)
    ) %>%
    mutate(prior = prior / sum(prior) ) %>%
    mutate(likelihood = likelihood / sum(likelihood) ) %>%
    mutate(posterior = prior * likelihood / sum(prior * likelihood) ) %>%
    ggplot(aes(x = theta) ) +
    geom_area(
        aes(y = prior, fill = "prior"),
        color = "white",
        show.legend = FALSE, alpha = 0.8
        ) +
    geom_area(
        aes(y = likelihood, fill = "likelihood"),
        color = "white",
        show.legend = FALSE, alpha = 0.8
        ) +
    geom_area(
        aes(y = posterior, fill = "posterior"),
        color = "white",
        show.legend = FALSE, alpha = 0.8
        ) +
    # geom_line(aes(y = prior, color = "prior"), show.legend = FALSE) +
    # geom_line(aes(y = likelihood, color = "likelihood"), show.legend = FALSE) +
    # geom_line(aes(y = posterior, color = "posterior"), show.legend = FALSE) +
    scale_fill_manual(values = c("orangered", "magenta4","steelblue") )

p <- p + theme_void(base_family = "Open Sans") + theme_transparent()

sticker(
    subplot = p,
    package = "IMSB2022",
    p_size = 20,
    p_x = 1, p_y = 1.45,
    # p_color = "#1c5253", # p_family = "Montserrat",
    s_x = 1, s_y = 0.85,
    # h_color = "#1c5253",
    # spotlight = TRUE, l_alpha = 0.2,
    s_width = 1.3, s_height = 0.9,
    h_fill = "#1c5253",
    # white_around_sticker = F,
    filename = "files/hex.png", dpi = 300
    )
