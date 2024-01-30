source("lec02_visualize_function.R")
pacman::p_load(rstan, ggplot2)
set.seed(1)

### Hurdle Model

n = 50

#https://github.com/kapelner/QC_MATH_341_Fall_2023/blob/main/lectures/lec23.pdf

#############we don't get to see the real DGP!
true_theta_0 = 1
true_theta_1 = 2
t_time = seq(0, 5, length.out = n)
y = array(NA, n)
for (i in 1 : n){
  y[i] = rpois(1, true_theta_0 + true_theta_1 * t_time[i])
}
#############we don't get to see the real DGP!

ggplot(data.frame(t = t_time, y = y)) + 
  geom_point(aes(x = t_time, y = y))

stan_model_data = list(
  n_0 = sum(x == 0),
  n_not_0 = n - n_0,
  sum_x_minus_1_greater_than_0 = sum(x[x > 0] - 1)
)

#build the stan model object and run the sampler
stan_fit = stan(
  seed = get_seed(),
  file = "lec02_stan_spec_hurdle_model.stan",
  model_name = "hurdle_model",
  data = stan_model_data
)

#straight to inference
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_2, true_value = true_theta_2)


#Do we have to thin?
#
#The tradition of thinning when running MCMC stems primarily from the
#use of samplers that require a large number of iterations to achieve 
#the desired effective sample size. Because of the efficiency (effective 
#samples per second) of Hamiltonian Monte Carlo, rarely should this be 
#necessary when using Stan.

#but still looks like a good idea...
stan_ac(stan_fit, separate_chains = TRUE)

stan_fit = stan(
  seed = 1,
  file = "lec02_stan_spec_hurdle_model.stan",
  model_name = "hurdle_model",
  thin = 3,
  data = stan_model_data
)
stan_ac(stan_fit, separate_chains = TRUE)

visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_1, true_value = true_theta_1)
visualize_chain_and_compute_estimates_and_cr(extract(stan_fit)$theta_2, true_value = true_theta_2)

