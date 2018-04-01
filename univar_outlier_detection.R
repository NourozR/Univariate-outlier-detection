# LIBRARY 
library(robustbase) 

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

# FUNCTION FOR UNIVARIATE OUTLIER DETECTION USING "Adjusted Boxplot" METHOD
AdjBox_method = function(x){
  adj_box = adjbox(x, main  = "Adjusted Boxplot")
  return(adj_box$fence)
}

# Test data: a = c(-1,2,4,5,6,10,15,20,209,302)






























