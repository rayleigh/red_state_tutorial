#Extract results from a stanfit object to summarize it
extract_results_from_stanfit_object <- function(interested_coefficients, stanfit_obj, lower_bound = 0.025, upper_bound = 0.975)
{
  #Makes sure lower_bound is less than upper_bound for plotting purposes
  if (lower_bound > upper_bound)
  {
    temp_bound = upper_bound
    upper_bound = lower_bound
    lower_bound = temp_bound
  }
  
  #Set up labels to access lower and upper bounds information 
  lower_bound_percent = paste(lower_bound * 100, "%", sep = "")
  upper_bound_percent = paste(upper_bound * 100, "%", sep = "")
  interested_regression_cols <- c("mean", lower_bound_percent, upper_bound_percent)
  
  #Get data from the stanfit object and save it off in a different object
  extracted_regression_data <- 
    summary(stanfit_obj, 
            prob = c(lower_bound, upper_bound))$summary[interested_coefficients, 
                                                        interested_regression_cols]

  return(extracted_regression_data)
}

#Uses R's basic functions to plot coefficients and their credible intervals
#based on results from a summarized stanfit object generated from Stan code for a regression model
plot_coefficients_with_credible_intervals <- function(plot_title, x_labels, coefficient_result_matrix, lower_bound_percent = "2.5%", upper_bound_percent = "97.5%", plot_rows = row.names(coefficient_result_matrix))
{
  ##Store the number of interested coefficients for later use
  num_coefficients = length(x_labels)
  
  ##Create the initial plot based on the mean estimates in the stanfit object
  plot(coefficient_result_matrix[plot_rows, "mean"],
       xlim = c(1 - 0.25, num_coefficients + 0.25),
       ylim=range(c(coefficient_result_matrix[, lower_bound_percent], coefficient_result_matrix[, upper_bound_percent])),
       pch=19, xaxt = "n", xlab="Income", ylab="Estimated coefficient",
       main= plot_title
  )
  ##Re-add axis so all coefficients are listed and angled
  text(axis(1, 1:num_coefficients, labels = F), par("usr")[3], 
       labels = x_labels, 
       srt = 320,
       xpd = T,
       adj = c(-0.2, 1.2),
       cex = 0.9
  )
  ##Add bars
  arrows(1:num_coefficients, coefficient_result_matrix[plot_rows, lower_bound_percent], 1:num_coefficients, coefficient_result_matrix[plot_rows, upper_bound_percent], length=0.05, angle=90, code=3)
}

#Add a state's coefficients to a plot
add_state_to_plot <- function(state, state_labels, color, coefficient_result_matrix, lower_bound_percent = "2.5%", upper_bound_percent = "97.5%", point_offset = 0)
{
  points(1:5 + point_offset, coefficient_result_matrix[state_labels, "mean"], pch = 6)
  arrows(1:5 + point_offset, coefficient_result_matrix[state_labels, lower_bound_percent], 
         1:5 + point_offset, state_and_inc_regression_data[state_labels, upper_bound_percent], 
         length=0.05, angle=90, code=3, col = color)
  text(5 + point_offset, coefficient_result_matrix[state_labels[5], "mean"], labels = c(state), pos = 4)
  
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


