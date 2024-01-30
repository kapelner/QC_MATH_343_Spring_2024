
visualize_chain_and_compute_estimates_and_cr = function(samples, true_value, alpha = 0.05){
  plot(
    ggplot(data.frame(samples = samples)) +
      geom_histogram(aes(x = samples)) +
      geom_vline(xintercept = mean(samples), col = "blue") + 
      geom_vline(xintercept = true_value, col = "green") + 
      geom_vline(xintercept = quantile(samples, .025), col = "red") + 
      geom_vline(xintercept = quantile(samples, .975), col = "red")
  )
  
  list(
    mmse = mean(samples),
    mmae = median(samples),
    theta = true_value,
    cr_one_minus_alpha_theta = c(
      quantile(samples, alpha / 2), 
      quantile(samples, 1 - alpha / 2)
    )
  )
}
