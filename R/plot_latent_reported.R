plot_latent_reported <- function(df_firm_utility){
  df_summarised_R <- df_firm_utility %>% 
    group_by(R) %>% 
    summarise(freq_R = n()) %>% 
    rename(bin = R)
  df_summarised_e <- df_firm_utility %>% 
    group_by(bin_e) %>% 
    summarise(freq_e = n()) %>% 
    rename(bin = bin_e)
  
  df_summarised <- full_join(df_summarised_R, df_summarised_e, by = 'bin') %>% 
    pivot_longer(-bin, names_to = 'type', values_to = 'freq')
  
  g <- ggplot(df_summarised, aes(x = bin, y = freq, fill = type)) +
    geom_bar(aes(fill = type), stat = 'identity', position = 'dodge', alpha =0.7)+
    xlim(-20,20)
  plot(g)
}