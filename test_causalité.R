
# Test la causalité du modèle
arma_causal = function(model){
  if(model$arma[1]==0){# gère le cas trivial d'un modèle MA
    return(TRUE)
  }
  else{
    phi = polynomial(c(1,-model$coef[1:(model$arma[1])]))
    racines = polyroot(phi) #les coefficients polynomiaux sont donnés dans l'ordre CROISSANT
    
    for(i in 1:length(racines)){
      if (abs(racines[i])<=1) {
        return(FALSE)
      }
    }
    return(TRUE) #si toutes les racines sont de module strictement supérieur à 1, alors ARMA causal
  }
}
