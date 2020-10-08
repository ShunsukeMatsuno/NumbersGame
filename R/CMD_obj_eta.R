CMD_obj_eta <- function(eta, theta, df_observed, df_simulated, S){
  # This is a wrapper of `CMD_obj`.
  # This function fix parameters other than eta.
  CMD_obj <- CMD_obj(c(eta, theta), df_observed, df_simulated, S)
}