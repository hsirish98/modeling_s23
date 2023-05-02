#' alm_prof
#'
#' calculating the profit for the mean, min, and max almond yields 
#' This function computes the min, max, and mean profit of almond yield for years from 1988 to 2010
#' @param price is the price each ton acre of almonds are sold for
#' @param cost is the cost associated with growing each acre ton of almonds.
#' #' @param dataset is a time series data set containing information on day, month, year, and water year of data collection for max temperature in Celsius, min temperature in Celsius and precipitation
#' @examples yields(price = 4.0, cost = 3.5, dataset=almonds)
#' @author Hannah Irish % Nadine Snyder


alm_prof <- function(price, cost=3.5, dataset){
  
  yield_list <- yields(dataset)
  
  min_prof = (yield_list[[1]])*price - (yield_list[[1]])*cost
  max_prof = (yield_list[[2]])*price - (yield_list[[2]])*cost
  mean_prof = (yield_list[[3]])*price - (yield_list[[3]])*cost
  
  return(list(min_prof, max_prof, mean_prof))
  
}


