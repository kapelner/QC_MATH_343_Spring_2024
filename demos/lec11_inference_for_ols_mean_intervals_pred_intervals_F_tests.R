pacman::p_load(ggplot2)

set.seed(1)

n = 100

betavec = c(1, 0.2)
sigsq = 0.9

xmin = 1
xmax = 2
X = cbind(1, seq(from = xmin, to = xmax, length.out = n))
p_plus_one = ncol(X)
Xt = t(X)
XtX = Xt %*% X
XtXinv = solve(XtX)
XtXinvXt = XtXinv %*% Xt

#now we introduce randomness into the epsilons
eps = rnorm(n, mean = 0, sd = sqrt(sigsq))
#which means y will be a draw from Y
y = X %*% betavec + eps
#and b will be a draw from B
b = XtXinvXt %*% y
#and e will be a draw from E
e = y - X %*% b
#and SSE will be a draw from ||E||^2
SSE = sum(e^2)
df_err = n - p_plus_one
s_e = sqrt(SSE / df_err) #RMSE

ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y)) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + #accentuate axes
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen") +
  geom_abline(intercept = b[1],       slope = b[2],       col = "red") + 
  xlim(0, 3)

#now let's look at mean response CI's and prediction CI's
x_star = 1.5
#make it into the x_star_vec by appending the 1 and ensuring it's a row vector
x_star_vec = t(c(1, x_star))
#what is the predicted value?
yhat_star = as.numeric(x_star_vec %*% b)
yhat_star
#this is the best guess of both y_i and mu_i := h*(x_i) = expected response 
#mu_i means the average of infinite responses generated from this x_i

#let's make a 95% confidence interval for 
#mu_star = x_star beta_vec
alpha = 0.05
t_one_minus_alpha_over_two_df = qt(1 - alpha / 2, df_err)

yhat_star + c(-1, +1) *
  t_one_minus_alpha_over_two_df * s_e * 
  as.numeric(sqrt(x_star_vec %*% XtXinv %*% t(x_star_vec)))

#let's make a 95% confidence interval for 
#y_star = x_star beta_vec + epsilon_star
yhat_star + c(-1, +1) *
  t_one_minus_alpha_over_two_df * s_e * 
  as.numeric(sqrt(1 + x_star_vec %*% XtXinv %*% t(x_star_vec)))  

#why is the predictive interval much bigger?

#now let's look at how that interval varies
RES = 0.03
x_stars = seq(from = 0, to = 3, by = RES)
X_star_vecs = cbind(1, x_stars)
ci_mu_star_one_min_alphas = matrix(NA, nrow = length(x_stars), ncol = 2)
ci_y_star_one_min_alphas = matrix(NA,  nrow = length(x_stars), ncol = 2)
for (i in 1 : length(x_stars)){
  x_star_vec = X_star_vecs[i, , drop = FALSE]
  yhat_star = as.numeric(x_star_vec %*% b)
  ci_mu_star_one_min_alphas[i, ] = yhat_star + c(-1, +1) *
    t_one_minus_alpha_over_two_df * s_e * 
    as.numeric(sqrt(x_star_vec %*% XtXinv %*% t(x_star_vec)))
  ci_y_star_one_min_alphas[i, ] = yhat_star + c(-1, +1) *
    t_one_minus_alpha_over_two_df * s_e * 
    as.numeric(sqrt(1 + x_star_vec %*% XtXinv %*% t(x_star_vec)))  
}
 
ggplot_obj = ggplot(data.frame(x = X[, 2], y = y)) +
  geom_point(aes(x = x, y = y))


for (i in 1 : length(x_stars)){
  ggplot_obj = ggplot_obj + geom_segment(
    x = X_star_vecs[i, 2], 
    xend = X_star_vecs[i, 2],
    y = ci_y_star_one_min_alphas[i, 1],
    yend = ci_y_star_one_min_alphas[i, 2],
    col = "yellow",
    lwd = 1, 
    inherit.aes = FALSE
  )
}
for (i in 1 : length(x_stars)){
  ggplot_obj = ggplot_obj + geom_segment(
    x = X_star_vecs[i, 2], 
    xend = X_star_vecs[i, 2],
    y = ci_mu_star_one_min_alphas[i, 1],
    yend = ci_mu_star_one_min_alphas[i, 2],
    col = "blue",
    lwd = 1, 
    inherit.aes = FALSE
  )
}
ggplot_obj +
  geom_abline(intercept = betavec[1], slope = betavec[2], col = "limegreen", lwd = 2) +
  geom_abline(intercept = b[1],       slope = b[2],       col = "red", lwd = 2) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + #accentuate axes
  xlim(0, 3) +
  ylim(-2, 4)


# adjusted R squared, omnibus F test, partial F test
rm(list = ls())

X = model.matrix(medv ~ ., MASS::Boston)
n = nrow(X)
p_plus_one = ncol(X)
p_plus_one
df_error = n - p_plus_one
df_error
y = MASS::Boston$medv
b = solve(t(X) %*% X) %*% t(X) %*% y
yhat = X %*% b 
e = y - yhat 
SSE = sum(e^2)
MSE = SSE / df_error
s_e = sqrt(MSE)

full_mod = lm(medv ~ ., MASS::Boston)
summary(full_mod)

#calculate Rsq_adj manually
Rsq_adj = 1 - MSE / var(y)
Rsq_adj

#calculate F test manually
SSR = sum((yhat - mean(y))^2)
p = p_plus_one - 1
MSR = SSR / p
F_omni = MSR / MSE
F_omni
#pval
1 - pf(F_omni, p, df_error)

#calculate partial F test manually
summary_table = coef(summary(lm(medv ~ ., MASS::Boston)))
summary_table[order(summary_table[,4]), ]
#it looks like indus and age don't contribute as they have 
#p-vals of 0.74 and 0.96 respectively
#let's test them both together, i.e. 
#H_0: beta_indus = 0 and beta_age = 0 
#so that the number of features in the set S is:
k = 2

reduced_mod = lm(medv ~ . - indus - age, MASS::Boston)
e_A = reduced_mod$residuals

Fhathat = (sum(e_A^2) - sum(e^2)) / k / MSE
Fhathat
#and the pval
1 - pf(Fhathat, k, df_error)
#fail to reject H_0

#check the authority - use the "anova" function
#to test "nested" models
anova(full_mod, reduced_mod)


#why is the partial F test so important?
rm(list = ls())

#let's create a design matrix where p+1 = 4
n = 20
set.seed(1)

X = cbind(rnorm(n), rnorm(n))
#where the 3rd feature really is the 2nd feature plus tiny disturbances
X = cbind(X, X[, 2] + rnorm(n, 0, 0.01))

ggplot(data.frame(x2 = X[,2], x3 = X[,3])) +
  geom_point(aes(x = x2, y = x3))

#but this matrix is still full rank
#as x2 is not equal to x3, just very, very close
as.numeric(Matrix::rankMatrix(X))

#now imagine h*(x) where all features matter
beta_vec = c(0, 1, 1, 1)

eps = rnorm(n)
y = cbind(1, X) %*% beta_vec + eps

#now let's run a linear model
full_mod = lm(y ~ X)
summary(full_mod)
#conclusion from omnibus F: at least some features matter
#conclusion from t tests: x1 matters and 
#it appears that x2 and x3 both don't matter
#but this conclusion is from the t-test which tests
#individual contribution!

#how do we test "do both x2 and x3 matter together as a single unit"? 
#Yes, we can run a partial F test
red_mod = lm(y ~ X[, 1])
anova(full_mod, red_mod)
#yes they both matter together!



