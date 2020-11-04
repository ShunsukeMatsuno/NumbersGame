plot_objFunc_CMD <- function(theta, index, param_vec, df_simulated, df_observed,  S, parallel = FALSE){
  # This function plots the objective function for each value of parameter.
  # index = 1 (eta; marginal cost),
  #         2 (gamma; curvature),
  #         3 (psi2; variance),
  #         4 (zeta; variance multiplier)
  

  
  if(parallel == TRUE){
    ##### parallel computation ######
    ## register
    library(doParallel)
    cl <- parallel::makePSOCKcluster(4)
    doParallel::registerDoParallel(cl)
    
    obj = foreach (i = seq_along(param_vec),
                   .packages = c('dplyr', 'purrr', 'NumbersGame')) %dopar% 
          {
                     theta_temp <- theta
                     theta_temp[index] <- param_vec[i]
                     NumbersGame::CMD_obj(theta_temp, df_observed, df_simulated, S)
                   }
    obj = unlist(obj)
    
    ## kill finished tasks
    parallel::stopCluster(cl)
    
  }else{
    ##### sequenctial computation #####
    # store the values of obj. func. to `obj`
    obj <- numeric(length(param_vec))
    
    # compute onj. func. for each parameter
    for(i in seq_along(param_vec)){
      theta_temp <- theta
      theta_temp[index] <- param_vec[i]
      obj[i] <- CMD_obj(theta = theta_temp, df_observed = df_observed, df_simulated = df_simulated, S = S)
    }
  }

  
  # To plot obj. func., make a data frame.
  df_obj <- tibble(
    param = param_vec,
    obj = obj
  )
  
  # label is determined according to `index`
  x_label <- case_when(
    index == 1 ~ 'eta',
    index == 2 ~ 'gamma',
    index == 3 ~ 'psi2',
    index == 4 ~ 'zeta'
  )
  
  # plot
  g <- ggplot(df_obj, aes(x = param, y = obj)) +
    geom_point(size = 2) + 
    xlab(x_label) +
    geom_vline(aes(xintercept = theta[index]), linetype = 'dotted') +
    theme_bw()
  plot(g)
}