pacman::p_load(survival, ggplot2)

set.seed(3)
true_theta_1 = 4
true_theta_2 = 0.3
n = 200
t_f = 36
ys = sort(rnbinom(n, true_theta_1, true_theta_2))

ys[ys == 0] = 1 #don't want to deal with zeroes
sort(ys)
t0s = sample(0:(t_f - 1), n, replace = TRUE)
c_vec = ifelse(ys + t0s > t_f, 1, 0)
sum(c_vec)
ys[ys > t_f] = t_f
Surv(ys, 1 - c_vec)


survival_fit_obj = survfit2(Surv(ys, rep(1, n)) ~ 1) 
survival_fit_obj %>% 
  ggsurvfit() +
  labs(x = "# of months subscribed", y = "survival probability estimate") +
  ylim(0, 1)

survival_fit_obj = survfit2(Surv(ys, 1 - c_vec) ~ 1) 
survival_fit_obj %>% 
  ggsurvfit() +
  labs(x = "# of months subscribed", y = "survival probability estimate") +
  ylim(0, 1)
#median
summary(survival_fit_obj)$table[7]





B = 1e5
median_bs = array(NA, B)
idx = 1 : n
for (b in 1 : B){
  idx_b = sample(idx, n, replace = TRUE)
  ys_b = ys[idx_b]
  c_vec_b = c_vec[idx_b]
  survival_fit_obj_b = survfit2(Surv(ys_b, 1 - c_vec_b) ~ 1) 
  median_bs[b] = summary(survival_fit_obj_b)$table[7]  
}

ggplot(data.frame(bootstrapped_medians = median_bs)) +
  geom_histogram(aes(x = bootstrapped_medians))



