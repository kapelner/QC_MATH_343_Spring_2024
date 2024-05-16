pacman::p_load(tidyverse, survival)


##################
#Problem 1

theta_1 = 0.4
theta_2 = 3.6
n = 100

x = array(NA, n)
hurdles = rbinom(n, 1, theta_1)
poissons = rpois(n, theta_2)
x = ifelse(hurdles == 1, 0, 1 + poissons)

##################
#Problem 2
rm(list = ls())

pacman::p_load(tseries, data.table)
spy = get.hist.quote("SPY", "2014-05-09", "2024-05-10", quote = "Close")
voo = get.hist.quote("VOO", "2014-05-09", "2024-05-10", quote = "Close")
spy = data.table(spy)
spy[, pct_change := (Close - lag(Close)) / lag(Close) * 100]
spy = na.omit(spy)
voo = data.table(voo)
voo[, pct_change := (Close - lag(Close)) / lag(Close) * 100]
voo = na.omit(voo)
n1 = nrow(spy)
n2 = nrow(voo)

B = 1e5
x = c(spy$pct_change, voo$pct_change)
x_ind = 1 : length(x)

thetahathat_bs = array(NA, B)
for (b in 1 : B){
  x_ind_b = sample(x_ind) #shuffle the indices
  #cut them up into two pieces
  x_ind_b1 = x_ind_b[1 : n1]
  x_ind_b2 = x_ind_b[(n1 + 1) : n2]
  x_b1 = x[x_ind_b1]
  x_b2 = x[x_ind_b2]
  #calculate the test statistic
  thetahathat_bs[b] = mean(x_b1) - mean(x_b2)
}
rm(x, x_ind, b, x_ind_b, x_ind_b1, x_ind_b2, x_b1, x_b2)

#now calculate the actual test statistic
thetahathat = mean(spy$pct_change) - mean(voo$pct_change)

alpha = 0.05
ret_a = quantile(thetahathat_bs, alpha / 2)
ret_b = quantile(thetahathat_bs, 1 - alpha / 2)
ggplot(data.frame(thetahathat_bs = thetahathat_bs)) + 
  geom_histogram(aes(x = thetahathat_bs), bins = B / 100) + 
  # geom_vline(xintercept = ret_a, col = "red") + 
  # geom_vline(xintercept = ret_b, col = "red") +
  # geom_vline(xintercept = thetahathat, col = "blue") +
  xlim(-3,3)

#hence, we reject the null hypothesis
#calc p-value
2 * min(sum(thetahathat_bs > thetahathat), sum(thetahathat_bs < thetahathat)) / B

thetahathat_bs = array(NA, B)
for (b in 1 : B){
  thetahathat_bs[b] = quantile(sample(spy$pct_change, n1, replace = TRUE), .975)
}
ggplot(data.frame(thetahathat_bs = thetahathat_bs)) + 
  geom_histogram(aes(x = thetahathat_bs), bins = B / 1000) 


##################
#Problem 3
rm(list = ls())

D = ggplot2::diamonds %>% 
  select(price, everything())
colnames(D)[6:10] = paste0("x_", 1:5)
summary(lm(D$price ~ . * ., D[, 6:10]))

data.frame(rbind(as.character(1:20), round(qt(0.975, 1:20), 2)))

##################
#Problem 4
rm(list = ls())

#load the lung data set
lung = na.omit(survival::lung)
lung$status = lung$status - 1 #needs to be 0=alive, 1=dead
surv_obj = Surv(lung$time, lung$status)
head(surv_obj, 100)

mod = survreg(surv_obj ~ age + sex + meal.cal, lung)
summary(mod)

mod = coxph(surv_obj ~ age + sex + meal.cal, lung)
summary(mod)

##################
#Problem 5
rm(list = ls())

set.seed(1)
n = 300
sigma = 4
max_age = 80

#structural causal model for the DAG "D"
x2 = runif(n, 0, max_age)
x1 = 5 + 0.2 * x2 + rnorm(n, 0, sigma)
y = 75 - 1.4 * x1 + 0.9 * x2 + rnorm(n, 0, sigma)


cholesterol_example_data = data.table(
  age = x2,
  exercise = x1,
  cholesterol = y
)

#Consider only $x$ and $y$. Here's what it appears as:
ggplot(cholesterol_example_data) +
  aes(x = exercise, y = cholesterol) +
  geom_point() + 
  geom_smooth(method = "lm")


#Consider only $x$ and $y$. Here's what it appears as:
ggplot(cholesterol_example_data) +
  aes(x = age, y = exercise) +
  geom_point() + 
  geom_smooth(method = "lm")



