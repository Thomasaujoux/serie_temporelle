

# Calcul de R2 ajusté 
# A quoi correspondent le p et le q ?????
adj_r2 <- function(model){
  p <- model$arma[1]
  q <- model$arma[2]
  n <- model$nobs-max(p,q)
  ss_res <- sum(model$residuals^2)
  ss_tot <- sum(dindice[-c(1:max(p,q))]^2)
  adj_r2 <- 1-(ss_res/(n-p-q-1))/(ss_tot/(n-1))
  return(adj_r2)
}

