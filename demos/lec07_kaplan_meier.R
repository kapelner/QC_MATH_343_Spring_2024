pacman::p_load(ggplot2, survival, ggsurvfit)

set.seed(1)

n = 6

true_k = 3.5
true_lambda = 1.1
true_theta = 1 / true_lambda * gamma(1 / true_k + 1)
y = sort(rweibull(n, shape = true_k, scale = 1 / true_lambda))
 
#let's censor the 2nd and fourth
c_vec = c(0, 1, 0, 1, 1, 0)

#create the table
d_i = c(1, 0, 1, 0, 0, 1)
q_i = c_vec
survival_table = data.frame(t_i = y, d_i = d_i, q_i = q_i)
survival_table

#n_1
n_i = array(NA, n)
n_i[1] = n
n_i[2] = n_i[1] #repeat
n_i[3] = n_i[2] - d_i[1] - q_i[2]
n_i[4] = n_i[3] #repeat
n_i[5] = n_i[4] #repeat
n_i[6] = n_i[5] - d_i[3] - q_i[4] - q_i[5]

survival_table$n_i = n_i
#now we ignore all censorings as they are accounted for in 
survival_table = survival_table[c_vec == 0, ]
survival_table$q_i = NULL
survival_table$p_i = 1 - survival_table$d_i / survival_table$n_i
survival_table$p_surv_i = cumprod(survival_table$p_i)
survival_table

survival_obj = Surv(y, 1 - c_vec) #we flip our vector to be in their format
# survival_fit_obj = survfit(survival_obj ~ 1) 
# plot(survival_fit_obj)
# summary(survival_fit_obj)

survival_fit_obj = survfit2(survival_obj ~ 1) 
survival_fit_obj %>% 
  ggsurvfit() +
  xlab("time") +
  ylim(0, 1) +
  ylab("survival probability estimate") + 
  add_confidence_interval() +
  add_risktable()
