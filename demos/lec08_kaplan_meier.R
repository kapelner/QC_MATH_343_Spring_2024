pacman::p_load(ggplot2, survival, ggsurvfit)

set.seed(1)

n = 40

#############we don't get to see the real DGP!
true_k = 3.5
true_lambda = 1.1
true_theta = 1 / true_lambda * gamma(1 / true_k + 1)
y = rweibull(n, shape = true_k, scale = 1 / true_lambda)
#############we don't get to see the real DGP!


#let's now censor at times that are less than the maximum due to differential
#recruitment. Let's randomly call 30% of them "censored
set.seed(1)
c_vec = rbinom(n, 1, prob = 0.3)

#the empirical survival function won't work anymore as "ignoring censoring 
#erroneously treats patients who are censored as part of the risk set for 
#the entire follow-up period (see https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html)
#we need to use the Kaplan-Meier estimator and that's exactly what the package does
#https://scholar.google.com/scholar?q=Nonparametric+estimation+from+incomplete+observations&hl=en&btnG=Search&as_sdt=1%2C39&as_sdtp=on

survival_obj = Surv(y, 1 - c_vec) #we flip our vector to be in their format
survival_fit_obj = survfit2(survival_obj ~ 1) 
survival_fit_obj %>% 
  ggsurvfit() +
  xlab("time") +
  ylim(0, 1) +
  ylab("survival probability estimate") + 
  add_confidence_interval() +
  add_risktable()
#why does the K-S survival function hit zero? There's no censored observations at
#time max(y)

#can we do inference for any time t?
summary(survival_fit_obj) 
#those are the upper/lower limits of the bands you see in the plot

#how about inference for theta := Med[Y]?
summary(survival_fit_obj)$table[7 : 9]



########### let's build a K-S survival curve from scratch

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
