# Here's an Example of REAL Correlation without Causation
# Remember, correlation (non-spurious) will persist as the sample size increases.

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
