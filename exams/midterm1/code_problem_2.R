pacman::p_load(ggplot2, rstan)

visualize_chain_and_compute_estimates_and_cr = function(
    samples, 
    plot_mmse = TRUE, 
    plot_mmae = TRUE, 
    true_value = NULL, 
    alpha = NULL, 
    bins = 30,
    colors = c("blue", "orange", "green", "red")){
  ggplot_obj = ggplot(data.frame(samples = samples)) +
    geom_histogram(aes(x = samples), bins = bins)
    
    mmse = mean(samples)
    mmae = median(samples)
    if (plot_mmse){
      ggplot_obj = ggplot_obj + geom_vline(xintercept = mmse, col = colors[1])
    }
  
    if (plot_mmae){
      ggplot_obj = ggplot_obj + geom_vline(xintercept = mmae, col = colors[2])
    }
    
    if (!is.null(true_value)){
      ggplot_obj = ggplot_obj + 
        geom_vline(xintercept = true_value, col = colors[3]) 
    }
    if (!is.null(alpha)){
      ggplot_obj = ggplot_obj + 
        geom_vline(xintercept = quantile(samples, .025), col = colors[4]) + 
        geom_vline(xintercept = quantile(samples, .975), col = colors[4])
    }
    plot(ggplot_obj)
    
    list(
      mmse = mmse,
      mmae = mmae,
      theta = true_value,
      cr_one_minus_alpha_theta = c(
        quantile(samples, alpha / 2), 
        quantile(samples, 1 - alpha / 2)
      )
    )
}


set.seed(1)
n = 300

#############we don't get to see the real DGP!
true_theta = 0.4
true_alpha_1 = 3
true_beta_1 = 6
true_alpha_2 = 9
true_beta_2 = 3
x = array(NA, n)
for (i in 1 : n){
  x[i] =  if (runif(1) < true_theta){
            rbeta(1, true_alpha_1, true_beta_1)
          } else {
            rbeta(1, true_alpha_2, true_beta_2)
          }
}
#############we don't get to see the real DGP!

ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x)) +
  xlab("normalized score")


stan_model_data = list(x = x, n = length(x))

stan_model_code = "
  data {
    int<lower=0> n;
    vector[n] x;
  }
  
  parameters {
    real<lower=0,upper=1> theta;
    real<lower=0,upper=100> alpha_1;
    real<lower=0,upper=100> beta_1;
    real<lower=alpha_1,upper=100> alpha_2;
    real<lower=0,upper=100> beta_2;
  }
  
  model {
    for (i in 1:n) {
      //target += log_sum_exp(theta + beta_lpdf(x[i] | alpha_1, beta_1),
      //                      (1 - theta) + beta_lpdf(x[i] | alpha_2, beta_2));
      target += log(theta * exp(beta_lpdf(x[i] | alpha_1, beta_1)) + (1 - theta) * exp(beta_lpdf(x[i] | alpha_2, beta_2)));
    }
  }
"

stan_fit = stan(
  seed = 1,
  model_code = stan_model_code,
  model_name = "mixture_model",
  data = stan_model_data,
  chains = 1, 
  iter = 4000
)

mcmc_samples = data.frame(
  theta = extract(stan_fit, inc_warmup = TRUE)$theta,
  alpha_1 = extract(stan_fit, inc_warmup = TRUE)$alpha_1,
  beta_1 = extract(stan_fit, inc_warmup = TRUE)$beta_1,
  alpha_2 = extract(stan_fit, inc_warmup = TRUE)$alpha_2,
  beta_2 = extract(stan_fit, inc_warmup = TRUE)$beta_2
)
mcmc_samples$t = 1 : nrow(mcmc_samples)

ggplot(mcmc_samples) + 
  geom_line(aes(x = t, y = beta_1)) + 
  xlim(0, 500)

##assess autocorrelation
par(mfrow = c(5, 1))
ell_max = 20
r_max = 0.3
acf(mcmc_samples$theta, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(mcmc_samples$alpha_1, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(mcmc_samples$beta_1, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(mcmc_samples$alpha_2, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)
acf(mcmc_samples$beta_2, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max)

par(mfrow = c(1, 1))
acf(mcmc_samples$beta_1, 
    xlim = c(0, ell_max), ylim = c(0, r_max), lag.max = ell_max, main= "")

visualize_chain_and_compute_estimates_and_cr(mcmc_samples$beta_1, bins = 200, plot_mmse = FALSE, plot_mmae = FALSE)
visualize_chain_and_compute_estimates_and_cr(mcmc_samples$theta, bins = 200, plot_mmse = FALSE, plot_mmae = FALSE)

visualize_chain_and_compute_estimates_and_cr(mcmc_samples$theta, true_value = true_theta)
visualize_chain_and_compute_estimates_and_cr(mcmc_samples$alpha_1, true_value = true_alpha_1)
visualize_chain_and_compute_estimates_and_cr(mcmc_samples$alpha_2, true_value = true_alpha_2)
visualize_chain_and_compute_estimates_and_cr(mcmc_samples$beta_1, true_value = true_beta_1)
visualize_chain_and_compute_estimates_and_cr(mcmc_samples$beta_2, true_value = true_beta_2)



mcmc_samples$tau = (mcmc_samples$alpha_1 - 1) / (mcmc_samples$alpha_1 + mcmc_samples$beta_1 + 2) - 
  (mcmc_samples$alpha_2 - 1) / (mcmc_samples$alpha_2 + mcmc_samples$beta_2 + 2)
visualize_chain_and_compute_estimates_and_cr(mcmc_samples$tau, bins = 200, plot_mmae = FALSE)
