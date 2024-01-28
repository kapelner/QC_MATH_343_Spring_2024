pacman::p_load(ggplot2)
set.seed(1)
n = 40

#############we don't get to see the real DGP!
true_m = 10       #theta_1
true_lambda_1 = 2 #theta_2
true_lambda_2 = 4 #theta_3

x = rpois(true_m, true_lambda_1)
x = c(x, rpois(n - true_m, true_lambda_2))
#############we don't get to see the real DGP!


ggplot(data.frame(x = x, t = 1 : n)) + 
  geom_point(aes(x = t, y = x))


#chains
num_tot_samples = 1e5
lambda1s = array(NA, num_tot_samples)
lambda2s = array(NA, num_tot_samples)
ms = array(NA, num_tot_samples)
#start positions
lambda1s[1] = 1
lambda2s[1] = 1
ms[1] = 1

#convenience function to calculate log probability
ln_p_m = function(m, lambda1, lambda2){
  if (m == 0){			
    sum(x[1 : n]) * log(lambda2)
  } else if (m == n){
    (lambda2 - lambda1) * m + sum(x[1 : m]) * log(lambda1)
  } else {
    (lambda2 - lambda1) * m + sum(x[1 : m]) * log(lambda1) + sum(x[(m + 1) : n]) * log(lambda2)
  }			
}

for (t in 2 : num_tot_samples){
  m = ms[t - 1]
  lambda1 = rgamma(1, 1 + sum(x[1 : m]), m)
  lambda2 = rgamma(1, 1 + sum(x[(m + 1) : n]), n - m)
  
  #now we need to calculate all the m dist

  ln_m_dist = array(NA, n - 1)
  for (m in 1 : (n - 1)){
    ln_m_dist[m] = ln_p_m(m, lambda1, lambda2)
  }
  ln_m_dist	
  ln_m_dist = ln_m_dist - max(ln_m_dist)
  m_dist = exp(ln_m_dist) / sum(exp(ln_m_dist))
  
  
  lambda1s[t] = lambda1
  lambda2s[t] = lambda2
  ms[t] = sample(1 : (n - 1), 1, prob = m_dist)
}

#collect all iterations
gibbs_chain = data.frame(
  lambda1s = lambda1s, 
  lambda2s = lambda2s, 
  ms = ms,
  t = 1 : num_tot_samples
)

#assess convergence
max_t = 1e4
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = lambda1s)) +
  xlim(1, max_t)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = lambda2s)) +
  xlim(1, max_t)
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = ms)) +
  xlim(1, max_t)
                 
#looks like the gibbs sampler burns in right away... so just to be safe
t_burn_in = 100
gibbs_chain = gibbs_chain[t_burn_in : num_tot_samples, ]


##assess autocorrelation
par(mfrow = c(3, 1))
acf(lambda1s, xlim = c(1, 25), ylim = c(-0.05, 0.3))
acf(lambda2s, xlim = c(1, 25), ylim = c(-0.05, 0.3))
acf(ms, xlim = c(1, 25), ylim = c(-0.05, 0.3))

#looks like it thins around 6
t_thin = 100

#thin the chains
gibbs_chain = gibbs_chain[seq(1, nrow(gibbs_chain), by = t_thin), ]

#inference
ggplot(gibbs_chain) +
  geom_histogram(aes(x = lambda1s)) +
  geom_vline(xintercept = mean(gibbs_chain$lambda1s), col = "blue") + 
  geom_vline(xintercept = true_lambda_1, col = "green") + 
  geom_vline(xintercept = quantile(gibbs_chain$lambda1s, .025), col = "red") + 
  geom_vline(xintercept = quantile(gibbs_chain$lambda1s, .975), col = "red")

#mmse
mean(gibbs_chain$lambda1s)
true_lambda_1
#CR_theta_0_95%
c(quantile(gibbs_chain$lambda1s, .025), quantile(gibbs_chain$lambda1s, .975))

ggplot(gibbs_chain) +
  geom_histogram(aes(x = lambda2s)) +
  geom_vline(xintercept = mean(gibbs_chain$lambda2s), col = "blue") + 
  geom_vline(xintercept = true_lambda_2, col = "green") + 
  geom_vline(xintercept = quantile(gibbs_chain$lambda2s, .025), col = "red") + 
  geom_vline(xintercept = quantile(gibbs_chain$lambda2s, .975), col = "red")

#mmse
mean(gibbs_chain$lambda2s)
true_lambda_2
#CR_theta_0_95%
c(quantile(gibbs_chain$lambda2s, .025), quantile(gibbs_chain$lambda2s, .975))


ggplot(gibbs_chain) +
  geom_histogram(aes(x = ms)) +
  geom_vline(xintercept = mean(gibbs_chain$ms), col = "blue") + 
  geom_vline(xintercept = true_m, col = "green") + 
  geom_vline(xintercept = quantile(gibbs_chain$ms, .025), col = "red") + 
  geom_vline(xintercept = quantile(gibbs_chain$ms, .975), col = "red")

#mmse
mean(gibbs_chain$ms)
true_m
#CR_theta_0_95%
c(quantile(gibbs_chain$ms, .025), quantile(gibbs_chain$ms, .975))
