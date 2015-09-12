#Extract results from a stanfit object to summarize it
make_plot_data <- function(stanfit, coef_names, stat_names) {
  estimates <- summary(stanfit)$summary[coef_names, stat_names]
  data <- data.frame(estimates, factor(1:nrow(estimates), 
                                       labels = rownames(estimates)))
  colnames(data) <- c("mean", "lower95", "upper95", "name")
  data
}

# Plot coefficient estimates and intervals
plot_estimates <- function(data) {
  ggplot(data, aes(x = name, y = mean, ymin = lower95, ymax = upper95)) +
    geom_pointrange(size = .75) + 
    geom_hline(yintercept = 0, linetype = 2, color = "maroon") +
    labs(y = "Estimate", x = "Coefficient")
}


#Creates a vector of strings to get the results from the stanfit object 
#associated with the state_number
generate_state_labels <- function(state_number)
{
  ##Uses sapply to repeat joining m_Intercept_inc2_inc3_inc4_inc5_by_state[,  
  ##state_number, and i (1, 2, 3, 4, 5)
  sapply(1:5, function(i) {
    paste("m_Intercept_inc2_inc3_inc4_inc5_by_state[", state_number, ",", i, "]", sep = "")},
         simplify = T)
}


