pacman::p_load(ggplot2)
set.seed(1)
n = 50

#############we don't get to see the real DGP!
true_theta_1 = 0.5678
true_theta_2 = 3.1415

x = array(NA, n)
for (i in 1 : n){
  x[i] =  if (runif(1) <= true_theta_1){
            0
          } else {
            rpois(1, true_theta_2) + 1
          }
}
#############we don't get to see the real DGP!

ggplot(data.frame(x = x)) + 
  geom_histogram(aes(x = x))


##### now do the gibbs sampler
num_tot_samples = 1e4
theta_1s = array(NA, num_tot_samples)
theta_2s = array(NA, num_tot_samples)


###initialize thetas to best guesses
theta_1s[1] = mean(x == 0)
theta_1s[1]
theta_2s[2] = mean(x[x > 0])
theta_2s[2]

##useful data to cache up top
n0 = sum(x == 0)
n_minus_n0 = n - n0
n0
n_minus_n0
sum_x_minus_1_greater_than_0 = sum(x[x > 0] - 1)
###do the samples
for (s in 2 : num_tot_samples){
  theta_1s[s] = rbeta(1, n0 + 1, n_minus_n0)
  theta_2s[s] = rgamma(1, 1 + sum_x_minus_1_greater_than_0, n_minus_n0)
}

#collect all iterations
gibbs_chain = data.frame(
  theta_1 = theta_1s, 
  theta_2 = theta_2s, 
  t = 1 : num_tot_samples
)

#assess convergence
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta_1))
ggplot(gibbs_chain) +
  geom_point(aes(x = t, y = theta_2))

#burn in immediate, let's say to burn in something...
t_burn_in = 100
gibbs_chain = gibbs_chain[t_burn_in : num_tot_samples, ]

par(mfrow = c(2, 1))
acf(gibbs_chain$theta_1)
acf(gibbs_chain$theta_2)

#no thin!

#straight to inference
ggplot(gibbs_chain) +
  geom_histogram(aes(x = theta_1)) +
  geom_vline(xintercept = mean(gibbs_chain$theta_1), col = "blue") + 
  geom_vline(xintercept = true_theta_1, col = "green") + 
  geom_vline(xintercept = quantile(gibbs_chain$theta_1, .025), col = "red") + 
  geom_vline(xintercept = quantile(gibbs_chain$theta_1, .975), col = "red")

#mmse
mean(gibbs_chain$theta_1)
true_theta_1
#CR_theta_1_95%
c(quantile(gibbs_chain$theta_1, .025), quantile(gibbs_chain$theta_1, .975))


ggplot(gibbs_chain) +
  geom_histogram(aes(x = theta_2)) +
  geom_vline(xintercept = mean(gibbs_chain$theta_2), col = "blue") + 
  geom_vline(xintercept = true_theta_2, col = "green") + 
  geom_vline(xintercept = quantile(gibbs_chain$theta_2, .025), col = "red") + 
  geom_vline(xintercept = quantile(gibbs_chain$theta_2, .975), col = "red")

#mmse
mean(gibbs_chain$theta_2)
true_theta_2
#CR_theta_1_95%
c(quantile(gibbs_chain$theta_2, .025), quantile(gibbs_chain$theta_2, .975))
