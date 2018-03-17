########### Set my working directory

setwd("F:\\Marketing_Model")

##### <- input you local directory
getwd()

library(plyr)



install.packages("plyr")
library(plyr)
install.packages("lmtest")
library(lmtest)
install.packages("DataCombine")
library(DataCombine)
install.packages("lubridate")
library(lubridate)


data_orig<-read.csv("daypart_model_input_sample.csv",stringsAsFactors = FALSE)

####################################################################


####################################### data validation
typeof(data_orig)
## attributes(data_orig)
summary(data_orig)
names(data_orig)

data_main<-data_orig  ##### data_main is a copy of data_orig used for actually modeling
####################################################################

#################################################
################ Functions #######################
#################################################

scurve<-function(vector,beta, alpha, max){
  scurve <- beta^(alpha^( ( vector/max)*100) )
  return(scurve)
}

#---------- var_trans is an all purpose transformation function for lagging, stocking, and functional forms (SCurve, Log, or Linear)
#---------- transformation fuction for functional form (scurve, log, or linear), lags, and stocking
#---------- transf = "s" denotes scurve transformation
#---------- transf = "log" denotes log transformation
#---------- transf = "linear" denotes linear transformation
#---------- a cross section variable is required.  If there is only a single cross section, use a variable populated with a single string 
#---------- lags are denoted as a negative number.  For example, -2 denotes lag 2.
#---------- retention_aft is recommended for s curves. retenion_bef is recommeded for all others


var_trans <- function(transf = "linear",vector, beta, alpha, max , x_id, retention_bef=0, retention=0, lag=0, div_const = 1, add_const = 1 ){
  
  ### alpha, beta, and max are used for scurves
  ### div_const and add_const are used for log 
  
  assign("constant", retention, envir = .GlobalEnv)
  assign("constant_bef", retention_bef, envir = .GlobalEnv)
  assign("transform_type", transf, envir = .GlobalEnv)
  
  sc_frame_bef <- data.frame(x_id, vector)
  sc_frame_bef <-ddply(sc_frame_bef, ~x_id, transform, stock = as.numeric(filter(x=vector, filter=constant_bef, method="recursive")) )   
  
  sc <- scurve(sc_frame_bef$stock ,beta, alpha, max)
  lg <- log(sc_frame_bef$stock / div_const + add_const)
  lin <- sc_frame_bef$stock
  
  z <- sc*(transform_type == "s") + lin*(transform_type == "linear") + lg*(transform_type == "log")
  
  sc_frame <- data.frame(x_id, z)
  sc_frame <-ddply(sc_frame, ~x_id, transform, stock = as.numeric(filter(x=z, filter=constant, method="recursive")) )   
  
  lagged <- slide(sc_frame, Var = "stock", GroupVar = "x_id", slideBy = lag, NewVar = "lagged_var" )  
  
  return(lagged$lagged_var)
}
View(data_main)

attach(data_main)

data_main$daypart_A_model <-var_trans(transf="s", vector=data_main$daypart_A , beta=1E-10, alpha=.95, max=83, x_id=0, retention=0, lag=0, add_const=1, div_const=1)

data_main$daypart_B_model <-var_trans(transf="s", vector=data_main$daypart_B , beta=1E-11, alpha=.92 , max=20, x_id=0, retention=.7, lag=0, add_const=1, div_const=1)

data_main$daypart_C_model <-var_trans(transf="s", vector=data_main$daypart_C , beta=1E-9, alpha=.95 , max=35, x_id=0, retention=0, lag=-1, add_const=1, div_const=1)

data_main$daypart_D_model <-var_trans(transf="s", vector=data_main$daypart_D , beta=1E-9, alpha=.95 , max=37, x_id=0, retention=0, lag=0, add_const=1, div_const=1)

data_main2<-data_main

##Model Regression Equation
model1<-(lm(formula =  TV_Contrib ~
              
              daypart_A_model+daypart_B_model+daypart_C_model+daypart_D_model-1
            ,  
            data=data_main2,na.action="na.exclude"))

summary(model1)


out <- capture.output(summary(model1))
plot(resid(model1))


data_main2$err <- resid(model1)
data_main2$err_plus <- ( (data_main2$err >= 0 ) * data_main2$err )
data_main2$err_minus  <- ( (data_main2$err < 0 ) * data_main2$err )

err <- resid(model1)
err_plus <- ( (err >= 0 ) * err )
err_minus  <- ( (err < 0 ) *err )


coeff1<-data.frame(coefficients(model1))
m<-data.frame(model.matrix(model1,na.action=NULL))
m2<-m  
#### predictions
pred<-predict(model1,newdata=data_main2,na.action=NULL)

#### output contribution pct
for (i in 1:nrow(m))  for (j in 1:nrow(coeff1))  m2[i,j]<-  ( coeff1[j,1]*m[i,j] )
m3<-data.frame(data_main2$TV_Contrib, pred, data_main2$date, m2)
write.csv(m3, "TV_Daypart_contributions.csv", row.names = FALSE)
