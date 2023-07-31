library(tidyverse)
data(HairEyeColor) # data adapted from Snee (1974)

cont <- apply(HairEyeColor, c(1, 2), sum) %>% t 
cont <- round(cont / sum(cont), 2)
cont

cont2 <- cont %>% as.data.frame %>% mutate(marginal_eye = rowSums(cont) )
rownames(cont2) <- row.names(cont)
cont2

cont3 <- rbind(cont2, colSums(cont2) )
rownames(cont3) <- c(row.names(cont2), "marginal_hair")
cont3

# p(y = Blue | c = Blond)
cont3["Blue", "Blond"] / cont3["Blue", "marginal_eye"]

# product rule: p(c = Blond, y = Blue)
cont3["Blue", "marginal_eye"] * cont3["Blue", "Blond"] / cont3["Blue", "marginal_eye"]
