plot_latent_reported <- function(df_firm_utility){
  df_summarised_R <- df_firm_utility %>% 
    group_by(R) %>% 
    summarise(freq_R = n(),
              .groups = "drop") %>% 
    rename(bin = R)
  df_summarised_e <- df_firm_utility %>% 
    group_by(e) %>% 
    summarise(freq_e = n(),
              .groups = "drop") %>% 
    rename(bin = e)
  
  df_summarised <- full_join(df_summarised_R, df_summarised_e, by = 'bin') %>% 
    pivot_longer(-bin, names_to = 'type', values_to = 'freq')
  
  g <- ggplot(df_summarised, aes(x = bin, y = freq, fill = type)) +
    geom_bar(aes(fill = type), stat = 'identity', position = 'dodge', alpha = .8)+
    xlim(-21,21) +
    scale_fill_hue(name = "type", labels = c(freq_e = "latent", freq_R ='reported')) +
    theme_bw()
  
  plot(g)
}
