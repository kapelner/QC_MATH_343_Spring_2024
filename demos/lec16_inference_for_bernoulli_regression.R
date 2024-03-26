
#let's look at the Pima diabetes data again
pima = na.omit(MASS::Pima.tr2)
pima$type = ifelse(pima$type == "Yes", 1, 0)
p = ncol(pima) - 1 #-1 for the response
p

#since we're doing bernoulli regression, we need a link function
family_function = binomial(link = "logit")

#and let's fit the generalized linear model on all features
full_mod = glm(type ~ ., pima, family = family_function)
summary(full_mod)
#it displays the Wald tests that are computed via the sqrt of the diagonal 
#entries from the Fisher information matrix and computing the matrix is not super difficult;
#see here: https://web.stanford.edu/class/archive/stats/stats200/stats200.1172/Lecture26.pdf

#notice how we have the log likelihood easily accessible from R
as.numeric(logLik(full_mod))

#we can now compute the omnibus test
#here's the null model: just the intercept
null_mod = glm(type ~ 1, pima, family = family_function)
summary(null_mod)

lik_ratio_test_stat = 2 * (as.numeric(logLik(full_mod)) - as.numeric(logLik(null_mod)))
#pval
1 - pchisq(lik_ratio_test_stat, p)
#big time reject of H_0 which makes sense

#let's verify with R's built-in function anova
anova(full_mod, null_mod) #maybe some numeric error?

#now let's do the subset test. There are some variables that don't seem significant at all
summary(full_mod)
S_subset = c("npreg", "bp", "skin")
red_mod_formula = as.formula(paste("type ~ ", paste(c(".", S_subset), collapse = "-")))
red_mod_formula
red_mod = glm(red_mod_formula, pima, family = family_function)
summary(red_mod)

lik_ratio_test_stat = 2 * (as.numeric(logLik(full_mod)) - as.numeric(logLik(red_mod)))
#pval
1 - pchisq(lik_ratio_test_stat, length(S_subset))
#fail to reject H_0: they don't matter

#let's verify with R's built-in function anova
anova(full_mod, red_mod) 


#But if I yank the ones that seem only marginally significant...
summary(full_mod)
S_subset = c(S_subset, "bmi", "age")
red_mod_formula = as.formula(paste("type ~ ", paste(c(".", S_subset), collapse = "-")))
red_mod = glm(red_mod_formula, pima, family = family_function)
summary(red_mod)

lik_ratio_test_stat = 2 * (as.numeric(logLik(full_mod)) - as.numeric(logLik(red_mod)))
#pval
1 - pchisq(lik_ratio_test_stat, length(S_subset))
#reject H_0: these features when considered together matter a whole lot

#let's verify with R's built-in function anova
anova(full_mod, red_mod) 

