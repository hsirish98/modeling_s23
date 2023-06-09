#' yields
#'
#' calculating the min, max and mean anomaly almond yield
#' This function computes the min, max, and mean anomalies of almond yield for months from 1988 to 2010
#' @param dataset is a time series data set containing information on day, month, year, and water year of data collection for max temperature in Celsius, min temperature in Celsius and precipitation
#' @param P1 is the first constant in the almond yield equation
#' @param P2 is the second constant in the almond yield equation
#' @param P3 is the third constant in the almond yield equation
#' @param P4 is the third constant in the almond yield equation
#' @param P5 is the third constant in the almond yield equation
#' @examples yields(climate_data)
#' @author Hannah Irish % Nadine Snyder



yields <- function(dataset, P1=0.015, P2=0.0046, P3=0.07, P4=0.0043, P5=0.28){

  
  temp_feb <- data.frame(dataset) %>%
    filter(month==2) %>%
    group_by(year) %>%
    summarise(min_temp = mean(tmin_c))
  
  precip_jan <- data.frame(dataset) %>%
    filter(month==1) %>%
    group_by(year) %>%
    summarise(precip_sum = sum(precip))
  
  
  combine <- cbind(temp_feb, precip_jan)
  
  
  calculate_yield <- function(temp, p, var1=P1, var2=P2, var3=P3, var4=P4, var5=P5) {
    yield = -(var1*temp) - (var2*(temp^2)) -(var3*p) + (var4*p^2) + var5
    return(yield)
  }
  
  vector <- mapply(calculate_yield, temp=combine$min_temp, p=combine$precip_sum)
  
  ##calculate min, max, and mean with no na's
  min_val <- min(vector, na.rm=TRUE)
  max_val <- max(vector, na.rm=TRUE)
  mean_val <- mean(vector, na.rm=TRUE)
  
  ##return list of these three
  return(list(min=min_val,max=max_val,mean=mean_val))
}