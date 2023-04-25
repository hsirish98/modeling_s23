#' yields
#'
#' calculating the min, max and mean anomaly almond yield
#' This function computes the min, max, and mean anomalies of almond yield for months from 1988 to 2010
#' @param dataset is a time series data set conatining information on day, mont, year, and water year of data collection for max temperature in celcius, min temperature in celcius and precipitation
#' @examples yields(climate_data)
#' @author Hannah Irish % Nadine Snyder



yields <- function(dataset){

  
  ##group the data set by year and month to generate the precipitation sum
  ##and the min temp for each month
  
  data_test <- as.data.frame(dataset) %>%
    group_by(year, month) %>%
    mutate(min_temp = min(tmin_c), sum_precip = sum(precip)) %>%
    ungroup()
  
  
  ##make function that uses the formula included in the paper
  calculate_yield <- function(temp, p) {
    yield = (-0.015*temp) - (0.0046*(temp^2)) -(0.07*p) + (0.0043*(p^2)) + 0.28
    
    return(yield)
  }
  
  ##make empty vector
  vec_test <- c()
  
  ##and empty matrix
  yield_m <- matrix()
  
  ##initialize rhe year to 1988
  year_iter = 1988
  
  ##initialize a counter for while loop
  j=1
  
  ##while loop until year is past 2010
  while (year_iter <2011){
    
    ##filter data set by given year
    by_year <- data_test %>%
      filter(year==year_iter)
    
    ##for loop to go through each month
    for(i in 1:12){
     
      ##filter data set by month 
      month <- by_year %>%
        filter(month==i)
      
      ##select temperature variable which is the min
      temps <- month %>%
        select(min_temp)
      
      ##get any of them because they're all the min
      temp <- as.numeric(temps[1,1])
      
      ##select precipitation variable which is the sum for the month
      precipitations <- month %>%
        select(sum_precip)
      
      ##get any which is the sum
      precip_sum <- as.numeric(precipitations[1,1])
      
      ##If neither temp nor precipitation is NA (got a value)
      if(is.na(temp)== FALSE)
      {
        if(is.na(precip_sum)==FALSE)
        {
          ##then calculate value for yield
          addition = calculate_yield(temp, precip_sum)
        }
      } else( ##otherwise give NA
        addition = NA
      )
      
      ##add whatever "addition" is (value or NA) to vector 
      vec_test[i]=addition
      
    }
    
    ##add vector to matrix
    yield_m[j] = vec_test
    
    ##update year to year later
    year_iter = year_iter + 1
    
    ##update counter which gives column in matrix
    j=j+1
    
  }
  
  ##calculate min, max, and mean with no na's
  min <- min(yield_m, na.rm=TRUE)
  max <- max(yield_m, na.rm=TRUE)
  mean <- mean(yield_m, na.rm=TRUE)
  
  ##return list of these three
  return(list(min,max,mean))
}