#' forest_growth
#'
#' Compute growth for a forest in terms of units of carbon
#' @param t  time
#' @param parms list containing the carrying capacity (K), growth rate (r) and linear growth rate after canopy closure (g)
#' @param C units of carbon
#' @return dC


forest_growth = function(t, C, parms) {
  
  dC = ifelse(C<50, parms$r*C, parms$g*(1-C/parms$K))
  return(list(dC))
  
}
  
 

