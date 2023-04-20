#' pv_energy
#'
#' calculating the energy of a Photovoltaic (PV) panel
#' This function computes the energy of a PV panel 
#' @param A is area of the PV panel (m2)
#' @param H is the average annual solar radiation
#' @param r is the panel yield (always 0-1, here defaulted to 0.2)
#' @param PR is the performance ratio of the panel (0-1, here defaulted to 0.75)
#' @examples pv_energy(20,300)
#' @author Hannah Irish


pv_energy <- function(A, H, r=0.2, PR=0.75){ ## assign the function and set the inputs, including initial values
  
  area = ifelse(A<=0, stop("Please check your panel area and try again"), A) ## if area is negative or 0, stop the function as this is illogical, and print and explanation that will help the user figure out what was wrong
  
  
  energy = area*r*H*PR ## calculate the energy of the panel
  return(energy) ##return energy as the output of the function
}