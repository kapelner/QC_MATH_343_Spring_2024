
# Spurious Correlation

#Take a look at the following real data:

pacman::p_load(tidyverse, magrittr, data.table)

spurious = data.frame(
  yearly_divorce_rate_maine_per_1000 = c(5,4.7,4.6,4.4,4.3,4.1,4.2,4.2,4.2,4.1),
  yearly_US_consumption_margarine_per_capita = c(8.2,7,6.5,5.3,5.2,4,4.6,4.5,4.2,3.7)
)

with(spurious, 
     cor(yearly_divorce_rate_maine_per_1000, yearly_US_consumption_margarine_per_capita))


#And visually,

ggplot(spurious, aes(x = yearly_divorce_rate_maine_per_1000, y = yearly_US_consumption_margarine_per_capita)) +
  geom_point() + geom_smooth()

#How did this happen? I looked at many, many different datasets until I found something impressive! 
#Well, we can imagine doing the same thing. Let's look at a million datasets and find the dataset most correlated with the yearly consumption of margarine per capita:

y = spurious$yearly_US_consumption_margarine_per_capita
n = length(y)

n_sim = 1e6
best_abs_corr = 0
best_random_xs = NULL
for (i in 1 : n_sim){
  x = rnorm(n)
  
  random_abs_corr = abs(cor(x, y))
  if (random_abs_corr > best_abs_corr){
    best_abs_corr = random_abs_corr
    best_random_xs = x
  }
}
spurious$best_random_xs = best_random_xs

best_abs_corr

#And visually,

ggplot(spurious, aes(x = best_random_xs, y = yearly_US_consumption_margarine_per_capita)) +
  geom_point() + geom_smooth() + ggtitle(paste("Spurious Correlation has |r| = ", round(best_abs_corr, 3)))

#So what's the narrative here? If you look through a gajillion random features that have no causal 
#connection with the phenomenon $y$, you will eventually find something that "clicks". It's the same 
#as the "chance capitalization" from 342W taken to an extreme. Here are a whole bunch of them:
#https://www.tylervigen.com/spurious-correlations

#However, these will all vanish if you keep collecting data. Anything that is built upon falsehood will crumble!

#Here's an Example of REAL Correlation without Causation

#When does correlation really not imply causation? We now mean real correlation, not spurious correlation. 
#This correlation will persist as the sample size increases. 

#Consider the phenomenon y = "num car accidents" with observed feature x = "num umbrellas sold" but common cause z = "rain amount". 
#It is clear the umbrella sales has *no causal* relationship with car accidents. But they *are correlated* because they are linked by a common cause. Here's the data example that makes this clear.

#The data generating process as specified by the causal diagram looks as follows:
  
rm(list = ls())
set.seed(1)
n = 300
sigma = 0.3

umbrella_example_data = data.frame(
  z_rainfall = runif(n, 0, 6) #here's the common cause - rainfall
)
umbrella_example_data$x_umbrella_sales = umbrella_example_data$z_rainfall^2 + rnorm(n, sigma) #x is a variable that is driven by z with noise
umbrella_example_data$y_car_accidents = umbrella_example_data$z_rainfall + rnorm(n, sigma) #y is a variable driven by z with noise

#So we only see $x$ and $y$. Here's what it appears as:

pacman::p_load(tidyverse, data.table, magrittr)
ggplot(umbrella_example_data) +
  aes(x = x_umbrella_sales, y = y_car_accidents) +
  geom_point() + 
  geom_smooth(method = "lm")

#and the model looks like:

mod = lm(y_car_accidents ~ x_umbrella_sales, umbrella_example_data)
summary(mod)

# So what's the interpretation of the coefficient for $x$? ...

#What you can't say is that $x$ is a causal contributor to $y$! You may want to say it, but you can't!

#Now let's build a model of $y$ linear in both $x$ and $z$. What happens?

mod = lm(y_car_accidents ~ x_umbrella_sales + z_rainfall, umbrella_example_data)
summary(mod)

#The effect of $x$ is gone!! Why? If you keep $z$ constant, the sole true causal factor in $y$, manipulating $x$ won't matter anymore!

#Why is this? Well, you can look at how x affects y in local areas of z for instance.

z_max = 0.2; z_min = 0.1
z_small_indices = umbrella_example_data$z_rainfall < 
  quantile(umbrella_example_data$z_rainfall, z_max) &
  umbrella_example_data$z_rainfall >
  quantile(umbrella_example_data$z_rainfall, z_min)

local_plot = ggplot(umbrella_example_data[z_small_indices, ]) +
  aes(x = x_umbrella_sales, y = y_car_accidents) +
  geom_point()
local_plot
local_plot +
  geom_smooth(method = "lm")

#If you force the common cause (lurking variable) to be an approximate constant, then you won't see any affect of x on y.
