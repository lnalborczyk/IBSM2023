########################################################################################
# KL example adapted from wikipedia
# https://fr.wikipedia.org/wiki/Divergence_de_Kullback-Leibler#cite_note-Kullback-3
######################################################################################

library(tidyverse)

p <- c(1/3, 1/3, 1/3) # distribution cible (uniform with p = 1/3)
q1 <- c(0.36, 0.48, 0.16) # modèle q1
q2 <- c(0.2, 0.6, 0.2) # modèle q2
q3 <- c(0.1, 0.8, 0.1) # modèle q3
q4 <- c(0.3, 0.4, 0.3) # modèle q4

(divergence_q1 <- sum(p * log(p / q1) ) ) # divergence modèle q1
(divergence_q2 <- sum(p * log(p / q2) ) ) # divergence modèle q2
(divergence_q3 <- sum(p * log(p / q3) ) ) # divergence modèle q3
(divergence_q4 <- sum(p * log(p / q4) ) ) # divergence modèle q4

(entropie_croisee_q1 <- sum(p * log(q1) ) ) # entropie croisée modèle q1
(entropie_croisee_q2 <- sum(p * log(q2) ) ) # entropie croisée modèle q2
(entropie_croisee_q3 <- sum(p * log(q3) ) ) # entropie croisée modèle q3
(entropie_croisee_q4 <- sum(p * log(q4) ) ) # entropie croisée modèle q4

(deviance_q1 <- (- 2) * sum(log(q1) ) ) # déviance modèle q1
(deviance_q2 <- (- 2) * sum(log(q2) ) ) # déviance modèle q2
(deviance_q3 <- (- 2) * sum(log(q3) ) ) # déviance modèle q3
(deviance_q4 <- (- 2) * sum(log(q4) ) ) # déviance modèle q4

# vecteur de divergences
divergences <- c(divergence_q1, divergence_q2, divergence_q3, divergence_q4)

# vecteur de log-score
log_scores <- c(sum(log(q1) ), sum(log(q2) ), sum(log(q3) ), sum(log(q4) ) )

# relation entre les deux
data.frame(divergences = divergences, neglogscore = -log_scores) %>%
    ggplot(aes(x = divergences, y = neglogscore) ) +
    geom_line() +
    geom_point() +
    labs(x = "KL Divergence", y = "Negative log-score")

# plotting the distributions
data.frame(p, q1, q2, q3, q4) %>%
    mutate(x = c(0, 1, 2) ) %>%
    pivot_longer(cols = 1:5) %>%
    ggplot(aes(x = x, y = value) ) +
    geom_bar(stat = "identity") +
    facet_wrap(~name, ncol = 5) +
    labs(x = expression(theta), y = "")
