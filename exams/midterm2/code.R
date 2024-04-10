#P1
pacman::p_load(datasets, ggplot2)
data(package = "datasets")

?datasets::ToothGrowth
summary(lm(len ~ ., datasets::ToothGrowth))
X = model.matrix(len ~ ., datasets::ToothGrowth)
p_plus_one = ncol(X)
round(t(X) %*% X, 2)
round(solve(t(X) %*% X), 2)

c(1,0,1) %*% round(solve(t(X) %*% X), 2) %*% c(1,0,1)

n = nrow(datasets::ToothGrowth)

Bsamp = 1000
bs = matrix(NA, nrow = p_plus_one, ncol = Bsamp)
for (b_samp in 1 : Bsamp){
    bs[, b_samp] = coef(lm(len ~ ., datasets::ToothGrowth[sample(1 : n, replace = TRUE), ]))
}

ggplot(data.frame(b = bs[2, ])) + 
  geom_histogram(aes(x = b, y = after_stat(density)), binwidth = 0.1) + 
  geom_vline(xintercept = quantile(bs[2, ], .025)) + 
  geom_vline(xintercept = quantile(bs[2, ], .975)) +
  xlab("OLS estimate for vitamin capsule delivery")


#P3
full_mod = MASS::glm.nb(breaks ~ ., datasets::warpbreaks)
summary(full_mod)
logLik(full_mod)

red_mod = MASS::glm.nb(breaks ~ wool, datasets::warpbreaks)
summary(red_mod)
logLik(red_mod)

red_mod = MASS::glm.nb(breaks ~ tension, datasets::warpbreaks)
summary(red_mod)
logLik(red_mod)