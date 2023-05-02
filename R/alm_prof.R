#' alm_prof
#'
#' calculating the profit for the mean, min, and max almond yields in $ 
#' This function computes the min, max, and mean profit of almond yield for years from 1988 to 2010
#' @param price is the price (in $) for each ton acre of almonds are sold for
#' @param cost is the cost (in $) associated with growing each acre ton of almonds.
#' @param dataset is a time series data set containing information on day, month, year, and water year of data collection for max temperature in Celsius, min temperature in Celsius and precipitation
#' @examples yields(price = 4.0, cost = 3.5, dataset=almonds)
#' @author Hannah Irish % Nadine Snyder


alm_prof <- function(price, cost=3.5, dataset){
  
  
  yield_list <- yields(dataset)
  
  min_prof = (yield_list$min)*price - (yield_list$min)*cost
  max_prof = (yield_list$max)*price - (yield_list$max)*cost
  mean_prof = (yield_list$mean)*price - (yield_list$mean)*cost
  
  return(list(min=min_prof, max=max_prof, mean=mean_prof))
  
}


