# OUTLIER DETECTION PACKAGE: https://rdrr.io/cran/univOutl/man/LocScaleB.html [LocScaleB]
# FUNCTION FOR UNIVARIATE OUTLIER DETECTION USING "MAD" METHOD 
MAD_method = function(x){
  med = median(x)
  MAD = mad(x) 
  lower = med - 2*(MAD/0.6745)
  higher = med + 2*(MAD/0.6745)
  return(c(lower, higher))
}

# FUNCTION FOR UNIVARIATE OUTLIER DETECTION USING "IQR" METHOD 
IQR_method = function(x){
  Q1 = quantile(x, 0.25)
  Q3 = quantile(x, 0.75)
  IQR = Q3 - Q1
  lower = Q1 - 1.5*IQR 
  higher = Q3 + 1.5*IQR  
  return(c(lower, higher))
}

AdjBox_method = function(x){
  adj_box = adjbox(x, main  = "Adjusted Boxplot")
  return(adj_box$fence)
}

# FUNCTION FOR UNIVARIATE OUTLIER DETECTION USING "Adjusted Boxplot" METHOD
library(robustbase) 
# a = c(-1,2,4,5,6,10,15,20,209,302)
unique_driver_limit <- unique_driver_perc$perc_unique_drivers_served_by
IQR_method(unique_driver_limit)
MAD_method(unique_driver_limit)
adj_box = adjbox(unique_driver_limit, main  = "Adjusted Boxplot")
adj_box$fence
plot(density(unique_driver_limit))
x = unique_driver_limit[unique_driver_limit > 0.65 & unique_driver_limit < 0.80] 
IQR_method(x)
MAD_method(x) # SOWS BEST RESULTS 
adj_box_new = adjbox(x, main  = "Adjusted Boxplot")
adj_box_new$fence
plot(density(x))





























