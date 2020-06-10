count_all_bins <- function(df_firm){
  # INPUT: N x 2 data frame
  #        1st col: firm, 2nd col: R or e
  # OUTPUT: 41 x 2 data frame
  #         count of each bin
  # If some bins are not observed, this function make the row of the bins and count as 0
  
  bin <- colnames(df_firm)[2]   # This is R (observed data) or e (simulated data)
  
  # observed bins
  df_bin_observed <- df_firm %>% 
    group_by(!!rlang::sym(bin)) %>% 
    summarise(count = n())
  
  bin_observed <- df_bin_observed %>% pull(!!rlang::sym(bin))
  
  # unobserved bins
  bin_unobserved <- setdiff(-20:20, bin_observed) 
  
  df_bin_unobserved <- tibble(
    bin_name = bin_unobserved,
    count = 0
  ) %>% 
    setNames(c(bin, 'count'))
  
  # merge them
  df_bin <- bind_rows(df_bin_unobserved, df_bin_observed) %>% 
    arrange(!!rlang::sym(bin)) 
  
  return(df_bin)
}