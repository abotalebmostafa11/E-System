#Import library
# install.packages("Formattable")#install package for make tables
#install.packages("xlsx")
#install.packages("writexl")
#install.packages("stargazer")   # Install & load gridExtra
library("stargazer")
library(xlsx)
library(xlsx)
library(writexl)
library(readxl)
library("openxlsx")
library(writexl)
library(readxl)
library("openxlsx")
library(fpp2)
library(forecast)
library(ggplot2)
library("readxl")
library(moments)
library(forecast)
require(forecast)  
require(tseries)
require(markovchain)
require(data.table)
library(Hmisc)
library(ascii)
library(pander)
library(tseries)
require(tseries) # need to install tseries tj test Stationarity in time series 
library(forecast)   # install library forecast             
library(ascii) # for make tables
library(gridExtra)
##Global vriable##
Full_original_data <- read_excel("data.xlsx") # path of your data ( time series data)
original_data<-Full_original_data$Cases # select colum from your data 
y_lab <- "(Daily Covid 19 Infection cases in Russia)"   # input name of data
Actual_date_interval <- c("2020/03/01","2021/08/15") # put actual range date of your data
Forecast_date_interval <- c("2021/08/16","2021/08/31") #put forecasting date range 
validation_data_days <-7 # Number of testing data(#testing last 10 days)10
Number_Neural<-50# Number of Neural For model NNAR Model
NNAR_Model<- FALSE     #create new NNAR model (TRUE/FALSE)
frequency<-"days" # type of you data( daily-weekly-month-years)
country.name <- "Russia" # name of area or country or cases
# Data Preparation & calculate some of statistics measures
summary(original_data) # Summary your time series
# calculate Cofficient of kurtosis
# calculate Cofficient of skewness
# calculate standard deviation 
data.frame(kurtosis=kurtosis(original_data),skewness=skewness(original_data),Standard.deviation =sd(original_data))
#processing on data (input data)
rows <- NROW(original_data) # calculate number of rows in time series (number of days)
training_data<-original_data[1:(rows-validation_data_days)] # Training data
testing_data<-original_data[(rows-validation_data_days+1):rows] #testing data
AD<-fulldate<-seq(as.Date(Actual_date_interval[1]),as.Date(Actual_date_interval[2]), frequency)  #input range for actual date
FD<-seq(as.Date(Forecast_date_interval[1]),as.Date(Forecast_date_interval[2]), frequency)  #input range forecasting date
N_forecasting_days<-nrow(data.frame(FD))  #calculate number of days that you want to forecasting
validation_dates<-tail(AD,validation_data_days) # select validation_dates
validation_data_by_name<-weekdays(validation_dates) # put names of validation dates
forecasting_data_by_name<-weekdays(FD)  # put names of Forecasting dates
##############
# NNAR Model #
##############
if(NNAR_Model==TRUE){
  data_series<-ts(training_data)
  model_NNAR<-nnetar(data_series, size = Number_Neural)
  saveRDS(model_NNAR, file = "model_NNAR.RDS")
  my_model <- readRDS("model_NNAR.RDS")
  accuracy(model_NNAR)  # accuracy on training data #Print Model Parameters
  model_NNAR
}
if(NNAR_Model==FALSE){
  data_series<-ts(training_data)
  #model_NNAR<-nnetar(data_series, size = Number_Numeral)
  model_NNAR <- readRDS("model_NNAR.RDS")
  accuracy(model_NNAR)  # accuracy on training data #Print Model Parameters
  model_NNAR
}
# Testing Data Evaluation
forecasting_NNAR <- forecast(model_NNAR, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_NNAR$mean,validation_data_days)
data.frame(validation_forecast)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using NNAR Model for ==> ",y_lab, sep=" ")
MAPE_Mean_All<-paste(round(mean(MAPE_Per_Day),3),"% MAPE",validation_data_days,frequency,y_lab,sep=" ")
MAPE_Mean_All_NNAR<-round(mean(MAPE_Per_Day),3)
MAPE_NNAR<-paste(round(MAPE_Per_Day,3),"%")
MAPE_NNAR_Model<-paste(MAPE_Per_Day ,"%")
paste ("MAPE that's Error of Forecasting for ",validation_data_days," days in NNAR Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in NNAR Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_NNAR=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_NNAR=validation_forecast,MAPE_NNAR_Model)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=tail(forecasting_NNAR$mean,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 

df1<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,forecasting.NNAR=head(forecasting_NNAR$mean,validation_data_days),MAPE=MAPE_NNAR_Model)
df11<-data.frame(Forecasting.Date=FD,forecating.date=forecasting_data_by_name,forecasting.NNAR=tail(forecasting_NNAR$mean,N_forecasting_days))
write.csv(df1, file = "testing NNAR Model.csv")
write.csv(df11, file = "forecasting NNAR Model.csv")

plot(forecasting_NNAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph1<-autoplot(forecasting_NNAR,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
p1<-graph1+scale_y_continuous(labels = scales::comma)+
  forecast::autolayer(forecasting_NNAR$mean, series="NNAR Model",size = 0.7) +
  guides(colour=guide_legend(title="Forecasts"),fill = "black")+
  theme(legend.position="bottom")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.7, linetype="solid", 
                                         colour ="gray"))
p1
#################
##  bats model  #
#################
# Data Modeling
data_series<-ts(training_data) # make your data to time series
autoplot(data_series ,xlab=paste ("Time in", frequency, sep=" "), ylab = y_lab, main=paste ("Actual Data :", y_lab, sep=" "))
model_bats<-bats(data_series)
accuracy(model_bats)  # accuracy on training data
# Print Model Parameters
model_bats
#ploting BATS Model
plot(model_bats,xlab = paste ("Time in", frequency ,y_lab , sep=" "))
# Testing Data Evaluation
forecasting_bats <- predict(model_bats, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_bats$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using BATS Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All.bats_Model<-round(mean(MAPE_Per_Day),3)
MAPE_Mean_All.bats<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_bats<-paste(round(MAPE_Per_Day,3),"%")
MAPE_bats_Model<-paste(MAPE_Per_Day ,"%")
paste ("MAPE that's Error of Forecasting for ",validation_data_days," days in BATS Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All.bats,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in BATS Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_bats=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_bats=validation_forecast,MAPE_bats_Model)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_bats=tail(forecasting_bats$mean,N_forecasting_days),lower=tail(forecasting_bats$lower,N_forecasting_days),Upper=tail(forecasting_bats$lower,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 



df2<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,Forecasting.BATS=head(forecasting_bats$mean,validation_data_days),MAPE=MAPE_bats_Model)
df21<-data.frame(Forecasting.Date=FD,forecating.Date=forecasting_data_by_name,Forecasting.BATS=tail(forecasting_bats$mean,N_forecasting_days),lower=tail(forecasting_bats$lower,N_forecasting_days),Upper=tail(forecasting_bats$lower,N_forecasting_days))
write.csv(df2, file = "testing BATS Model.csv")
write.csv(df21, file = "forecasting BATS Model.csv")



plot(forecasting_bats)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph2<-autoplot(forecasting_bats,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
p2<-graph2+scale_y_continuous(labels = scales::comma)+
  forecast::autolayer(forecasting_bats$mean, series="BATS Model",size = 0.7) +
  guides(colour=guide_legend(title="Forecasts"),fill = "black")+
  theme(legend.position="bottom")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.7, linetype="solid", 
                                         colour ="gray"))

p2
###############
## TBATS Model#
###############
# Data Modeling
data_series<-ts(training_data)
model_TBATS<-forecast:::fitSpecificTBATS(data_series,use.box.cox=FALSE, use.beta=TRUE,  seasonal.periods=c(6),use.damping=FALSE,k.vector=c(2))
accuracy(model_TBATS)  # accuracy on training data
# Print Model Parameters
model_TBATS
plot(model_TBATS,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
# Testing Data Evaluation
forecasting_tbats <- predict(model_TBATS, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_tbats$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using TBATS Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All.TBATS_Model<-round(mean(MAPE_Per_Day),3)
MAPE_Mean_All.TBATS<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_TBATS<-paste(round(MAPE_Per_Day,3),"%")
MAPE_TBATS_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in TBATS Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All.TBATS,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in TBATS Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_TBATS=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_TBATS=validation_forecast,MAPE_TBATS_Model)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_TBATS=tail(forecasting_tbats$mean,N_forecasting_days),Lower=tail(forecasting_tbats$lower,N_forecasting_days),Upper=tail(forecasting_tbats$upper,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 



df3<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,forecasting.TBATS=head(forecasting_tbats$mean,validation_data_days),MAPE=MAPE_TBATS_Model)
df31<-data.frame(Forecasting.Date=FD,forecating.Date=forecasting_data_by_name,forecasting.TBATS=tail(forecasting_tbats$mean,N_forecasting_days),Lower=tail(forecasting_tbats$lower,N_forecasting_days),Upper=tail(forecasting_tbats$upper,N_forecasting_days))
write.csv(df3, file = "testing TBATS Model.csv")
write.csv(df31, file = "forecasting TBATS Model.csv")



plot(forecasting_tbats)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph3<-autoplot(forecasting_tbats,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
p3<-graph3+scale_y_continuous(labels = scales::comma)+
  forecast::autolayer(forecasting_tbats$mean, series="TBATS Model",size = 0.7) +
  guides(colour=guide_legend(title="Forecasts"),fill = "black")+
  theme(legend.position="bottom")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.7, linetype="solid", 
                                         colour ="gray"))

p3
#######################
## Holt's linear trend#
#######################
# Data Modeling
data_series<-ts(training_data)
model_holt<-holt(data_series,h=N_forecasting_days+validation_data_days,lambda = "auto")
accuracy(model_holt)  # accuracy on training data
# Print Model Parameters
summary(model_holt$model)
# Testing Data Evaluation
forecasting_holt <- predict(model_holt, h=N_forecasting_days+validation_data_days,lambda = "auto")
validation_forecast<-head(forecasting_holt$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using holt's Linear trend Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All.Holt_Model<-round(mean(MAPE_Per_Day),3)
MAPE_Mean_All.Holt<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_holt<-paste(round(MAPE_Per_Day,3),"%")
MAPE_holt_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in holt's Linear trend Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All.Holt,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in holt's Linear trend  Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_holt=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_holt=validation_forecast,MAPE_holt_Model)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_holt=tail(forecasting_holt$mean,N_forecasting_days),Lower=tail(forecasting_holt$lower,N_forecasting_days),Upper=tail(forecasting_holt$upper,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 




df4<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,forecasting.Holt=head(forecasting_holt$mean,validation_data_days),MAPE=MAPE_holt_Model)
df41<-data.frame(Forecasting.Date=FD,forecating.Date=forecasting_data_by_name,forecasting.Holt=tail(forecasting_holt$mean,N_forecasting_days),Lower=tail(forecasting_holt$lower,N_forecasting_days),Upper=tail(forecasting_holt$upper,N_forecasting_days))
write.csv(df4, file = "testing Holt Model.csv")
write.csv(df41, file = "forecasting Holt Model.csv")




plot(forecasting_holt)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph4<-autoplot(forecasting_holt,xlab = paste ("Time in", frequency ,y_lab , sep=" "),  ylab=y_lab)
p4<-graph4+scale_y_continuous(labels = scales::comma)+
  forecast::autolayer(forecasting_holt$mean, series="Holt's Linear Trend Model",size = 0.7) +
  guides(colour=guide_legend(title="Forecasts"),fill = "black")+
  theme(legend.position="bottom")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.7, linetype="solid", 
                                         colour ="gray"))

p4
##################
#Auto arima model#
##################
paste ("tests For Check Stationarity in series  ==> ",y_lab, sep=" ")
kpss.test(data_series) # applay kpss test
pp.test(data_series)   # applay pp test
adf.test(data_series)  # applay adf test
ndiffs(data_series)    # Doing first diffrencing on data
#Taking the first difference
diff1_x1<-diff(data_series)
autoplot(diff1_x1, xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab,main = "1nd differenced series")
##Testing the stationary of the first differenced series
paste ("tests For Check Stationarity in series after taking first differences in  ==> ",y_lab, sep=" ")
kpss.test(diff1_x1)   # applay kpss test after taking first differences
pp.test(diff1_x1)     # applay pp test after taking first differences
adf.test(diff1_x1)    # applay adf test after taking first differences
#Taking the second difference
diff2_x1=diff(diff1_x1)
autoplot(diff2_x1, xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab ,main = "2nd differenced series")
##Testing the stationary of the first differenced series
paste ("tests For Check Stationarity in series after taking Second differences in",y_lab, sep=" ")
kpss.test(diff2_x1)   # applay kpss test after taking Second differences
pp.test(diff2_x1)     # applay pp test after taking Second differences
adf.test(diff2_x1)    # applay adf test after taking Second differences
####Fitting an ARIMA Model
#1. Using auto arima function
model1 <- auto.arima(data_series,stepwise=FALSE, approximation=FALSE, trace=T, test = c("kpss", "adf", "pp"))  #applaying auto arima
model1 # show the result of autoarima 
#Make changes in the source of auto arima to run the best model
arima.string <- function (object, padding = FALSE) 
{
  order <- object$arma[c(1, 6, 2, 3, 7, 4, 5)]
  m <- order[7]
  result <- paste("ARIMA(", order[1], ",", order[2], ",", 
                  order[3], ")", sep = "")
  if (m > 1 && sum(order[4:6]) > 0) {
    result <- paste(result, "(", order[4], ",", order[5], 
                    ",", order[6], ")[", m, "]", sep = "")
  }
  if (padding && m > 1 && sum(order[4:6]) == 0) {
    result <- paste(result, "         ", sep = "")
    if (m <= 9) {
      result <- paste(result, " ", sep = "")
    }
    else if (m <= 99) {
      result <- paste(result, "  ", sep = "")
    }
    else {
      result <- paste(result, "   ", sep = "")
    }
  }
  if (!is.null(object$xreg)) {
    if (NCOL(object$xreg) == 1 && is.element("drift", names(object$coef))) {
      result <- paste(result, "with drift        ")
    }
    else {
      result <- paste("Regression with", result, "errors")
    }
  }
  else {
    if (is.element("constant", names(object$coef)) || is.element("intercept", 
                                                                 names(object$coef))) {
      result <- paste(result, "with non-zero mean")
    }
    else if (order[2] == 0 && order[5] == 0) {
      result <- paste(result, "with zero mean    ")
    }
    else {
      result <- paste(result, "                  ")
    }
  }
  if (!padding) {
    result <- gsub("[ ]*$", "", result)
  }
  return(result)
}


bestmodel <- arima.string(model1, padding = TRUE)
bestmodel <- substring(bestmodel,7,11)
bestmodel <- gsub(" ", "", bestmodel)
bestmodel <- gsub(")", "", bestmodel)
bestmodel <- strsplit(bestmodel, ",")[[1]]
bestmodel <- c(strtoi(bestmodel[1]),strtoi(bestmodel[2]),strtoi(bestmodel[3]))
bestmodel
strtoi(bestmodel[3])
#2. Using ACF and PACF Function
#par(mfrow=c(1,2))  # Code for making two plot in one graph 
acf(diff2_x1,xlab = paste ("Time in", frequency ,y_lab , sep=" ") , ylab=y_lab, main=paste("ACF-2nd differenced series ",y_lab, sep=" ",lag.max=20))    # plot ACF "auto correlation function after taking second diffrences
pacf(diff2_x1,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab,main=paste("PACF-2nd differenced series ",y_lab, sep=" ",lag.max=20))   # plot PACF " Partial auto correlation function after taking second diffrences
x1_model1= arima(data_series, order=c(bestmodel)) # Run Best model of auto arima  for forecasting
x1_model1  # Show result of best model of auto arima 
paste ("accuracy of autoarima Model For  ==> ",y_lab, sep=" ")
accuracy(x1_model1)  # aacuracy of best model from auto arima
x1_model1$x          # show result of best model from auto arima 
checkresiduals(x1_model1,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)  # checkresiduals from best model from using auto arima 
paste("Box-Ljung test , Ljung-Box test For Modelling for   ==> ",y_lab, sep=" ")
Box.test(x1_model1$residuals^2, lag=20, type="Ljung-Box")   # Do test for resdulas by using Box-Ljung test , Ljung-Box test For Modelling
jarque.bera.test(x1_model1$residuals)  # Do test jarque.bera.test 
#Actual Vs Fitted
plot(data_series, col='red',lwd=2, main="Actual vs Fitted Plot", xlab='Time in (days)', ylab=y_lab) # plot actual and Fitted model 
lines(fitted(x1_model1), col='black')
#Test data
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) ) # make testing data in time series 
forecasting_auto_arima <- forecast(x1_model1, h=N_forecasting_days+validation_data_days)
validation_forecast<-head(forecasting_auto_arima$mean,validation_data_days)
MAPE_Per_Day<-round(abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using bats Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All.ARIMA_Model<-round(mean(MAPE_Per_Day),3)
MAPE_Mean_All.ARIMA<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_auto_arima<-paste(round(MAPE_Per_Day,3),"%")
MAPE_auto.arima_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All.ARIMA,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in bats Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_auto.arima=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_auto.arima=validation_forecast,MAPE_auto.arima_Model)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_auto.arima=tail(forecasting_auto_arima$mean,N_forecasting_days),Lower=tail(forecasting_auto_arima$lower,N_forecasting_days),Upper=tail(forecasting_auto_arima$upper,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 




df5<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,forecasting.autoarima=head(forecasting_auto_arima$mean,validation_data_days),MAPE=MAPE_auto.arima_Model)
df51<-data.frame(Forecasting.Date=FD,forecating.Date=forecasting_data_by_name,forecasting.autoarima=tail(forecasting_auto_arima$mean,N_forecasting_days),Lower=tail(forecasting_auto_arima$lower,N_forecasting_days),Upper=tail(forecasting_auto_arima$upper,N_forecasting_days))
write.csv(df5, file = "testing autoarima Model.csv")
write.csv(df51, file = "forecasting autoarima Model.csv")




plot(forecasting_auto_arima)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph5<-autoplot(forecasting_auto_arima,xlab = paste ("Time in", frequency ,y_lab , sep=" "), ylab=y_lab)
p5<-graph5+scale_y_continuous(labels = scales::comma)+
  forecast::autolayer(forecasting_auto_arima$mean, series="auto.arima Model",size = 0.7) +
  guides(colour=guide_legend(title="Forecasts"),fill = "black")+
  theme(legend.position="bottom")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.7, linetype="solid", 
                                         colour ="gray"))

p5
#########################################################################################
# Returns local linear forecasts and prediction intervals using cubic smoothing splines.#
# Testing Data Evaluation                                                               #
#########################################################################################
forecasting_splinef <- splinef(original_data,h=N_forecasting_days+validation_data_days)
summary(forecasting_splinef)
validation_forecast<-head(forecasting_splinef$mean,validation_data_days)
MAPE_Per_Day<-round(  abs(((testing_data-validation_forecast)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using cubic smoothing splines Model for  ==> ",y_lab, sep=" ")
MAPE_Mean_All.splinef_Model<-round(mean(MAPE_Per_Day),3)
MAPE_Mean_All.splinef<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_splinef<-paste(round(MAPE_Per_Day,3),"%")
MAPE_splinef_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in cubic smoothing splines Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_All.splinef,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in cubic smoothing splines Model for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_splinef=validation_dates,validation_data_by_name,actual_data=testing_data,forecasting_splinef=validation_forecast,MAPE_splinef_Model)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_splinef=tail(forecasting_splinef$mean,N_forecasting_days),Lower=tail(forecasting_splinef$lower,N_forecasting_days),Upper=tail(forecasting_splinef$upper,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 




df6<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,forecasting.cubic.smoothing.splines=head(forecasting_splinef$mean,validation_data_days),MAPE=MAPE_splinef_Model)
df61<-data.frame(Forecasting.Date=FD,forecating.Date=forecasting_data_by_name,forecasting.cubic.smoothing.splines=tail(forecasting_splinef$mean,N_forecasting_days),Lower=tail(forecasting_splinef$lower,N_forecasting_days),Upper=tail(forecasting_splinef$upper,N_forecasting_days))
write.csv(df6, file = "testing cubic smoothing splines Model.csv")
write.csv(df61, file = "forecasting cubic smoothing splines Model.csv")




plot(forecasting_splinef)
x1_test <- ts(testing_data, start =(rows-validation_data_days+1) )
lines(x1_test, col='red',lwd=2)
graph6<-autoplot(forecasting_splinef,xlab = paste ("Time in", frequency ,y_lab , sep=" "),  ylab=y_lab)
p6<-graph6+scale_y_continuous(labels = scales::comma)+
  forecast::autolayer(forecasting_splinef$mean, series="cubic smoothing splines Model",size = 0.7) +
  guides(colour=guide_legend(title="Forecasts"),fill = "black")+
  theme(legend.position="bottom")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.7, linetype="solid", 
                                         colour ="gray"))

p6
######################
#Ensembling (Average)#
######################
re_NNAR<-forecasting_NNAR$mean
re_BATS<-forecasting_bats$mean
re_TBATS<-forecasting_tbats$mean
re_holt<-forecasting_holt$mean
re_autoarima<-forecasting_auto_arima$mean
splinef_model<-data.frame(forecasting_splinef)
splinef<-splinef_model$Point.Forecast
result_df<-data.frame(re_NNAR,re_BATS,re_TBATS,re_holt,re_autoarima,splinef)
average_models<-rowMeans(result_df)
# Testing Data Evaluation
Ensembling_average1<-head(average_models,validation_data_days)
MAPE_Per_Day<-round(abs(((testing_data-Ensembling_average1)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using Ensembling (Average) for  ==> ",y_lab, sep=" ")
MAPE_Mean_EnsemblingAverage<-round(mean(MAPE_Per_Day),3)
MAPE_Mean_Ensembling<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_Ensembling<-paste(round(MAPE_Per_Day,3),"%")
MAPE_Ensembling_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in Ensembling Model for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_EnsemblingAverage,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in Ensembling (Average) for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_Ensembling=validation_dates,validation_data_by_name,actual_data=testing_data,Ensembling=head(average_models,validation_data_days),MAPE_Ensembling)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,Ensembling_Average=tail(average_models,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 
df7<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,Ensembling.Average=head(average_models,validation_data_days),MAPE=MAPE_Ensembling)
df71<-data.frame(Forecasting.Date=FD,forecating.Date=forecasting_data_by_name,Forecasting.Ensembling.Average=tail(average_models,N_forecasting_days))



write.csv(df7, file = "testing Ensembling Average.csv")
write.csv(df71, file = "forecasting Ensembling Average.csv")




#############################
#Ensembling (weight average)#
#############################
weight.model<-0.90#  priotizer the weights ( weight average)
re_NNAR<-forecasting_NNAR$mean
re_BATS<-forecasting_bats$mean
re_TBATS<-forecasting_tbats$mean
re_holt<-forecasting_holt$mean
re_autoarima<-forecasting_auto_arima$mean
re_splinef<-c(forecasting_splinef$mean)
re_bestmodel<-min(MAPE_Mean_All_NNAR,MAPE_Mean_All.bats_Model,MAPE_Mean_All.TBATS_Model,MAPE_Mean_All.Holt_Model,MAPE_Mean_All.ARIMA_Model,MAPE_Mean_All.splinef_Model)
y1<-if(re_bestmodel >= MAPE_Mean_All.bats_Model) {re_BATS*weight.model
} else {
  (re_BATS*(1-weight.model))/5
}

y2<-if(re_bestmodel >= MAPE_Mean_All.TBATS_Model) {re_TBATS*weight.model
} else {
  (re_TBATS*(1-weight.model))/5
}

y3<-if(re_bestmodel >= MAPE_Mean_All.Holt_Model) {re_holt*weight.model
} else {
  (re_holt*(1-weight.model))/5
}
y4<-if(re_bestmodel >= MAPE_Mean_All.ARIMA_Model) {re_autoarima*weight.model
} else {
  (re_autoarima*(1-weight.model))/5
}
y5<-if(re_bestmodel >= MAPE_Mean_All_NNAR) {re_NNAR*weight.model
} else {
  (re_NNAR*(1-weight.model))/5
}
y6<-if(re_bestmodel >= MAPE_Mean_All.splinef_Model) {re_splinef*weight.model
} else {
  (splinef*(1-weight.model))/5
}
Ensembling.weight<-(y1+y2+y3+y4+y5+y6)
# Testing Data Evaluation
validation_forecast2<-head(Ensembling.weight,validation_data_days)
MAPE_Per_Day<-round(abs(((testing_data-validation_forecast2)/testing_data)*100)  ,3)
paste ("MAPE % For ",validation_data_days,frequency,"by using Ensembling (weight average) for  ==> ",y_lab, sep=" ")
MAPE_Mean_EnsemblingAverage1<-round(mean(MAPE_Per_Day),3)
MAPE_Mean_Ensembling<-paste(round(mean(MAPE_Per_Day),3),"% MAPE ",validation_data_days,frequency,y_lab,sep=" ")
MAPE_Ensembling<-paste(round(MAPE_Per_Day,3),"%")
MAPE_Ensembling_Model<-paste(MAPE_Per_Day ,"%")
paste (" MAPE that's Error of Forecasting for ",validation_data_days," days in Ensembling weight average for  ==> ",y_lab, sep=" ")
paste(MAPE_Mean_EnsemblingAverage1,"%")
paste ("MAPE that's Error of Forecasting day by day for ",validation_data_days," days in Ensembling weight average for  ==> ",y_lab, sep=" ")
print(ascii(data.frame(date_Ensembling=validation_dates,validation_data_by_name,actual_data=testing_data,Ensembling=validation_forecast2,MAPE_Ensembling)), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_Ensembling=tail(Ensembling.weight,N_forecasting_days))), type = "rest")
# extract results in Txt & csv file 
df8<-data.frame(Testing.Date=validation_dates,Day.Name=validation_data_by_name,Actual.Value=testing_data,Ensembling.weight.average=head(Ensembling.weight,validation_data_days),MAPE=MAPE_Ensembling)
df81<-data.frame(Forecasting.Date=FD,forecating.Date=forecasting_data_by_name,Forecasting.Ensembling.weight.Average=tail(Ensembling.weight,N_forecasting_days))



write.csv(df8, file = "testing Ensembling weight average.csv")
write.csv(df81, file = "forecasting weight average.csv")


graph8<-autoplot(Ensembling.weight,xlab = paste ("Time in", frequency ,y_lab,"by using Ensembling weight average" , sep=" "), ylab=y_lab)
p8<-graph8+scale_y_continuous(labels = scales::comma)+
  forecast::autolayer(Ensembling.weight, series="Ensembling weight average",size = 0.7) +
  guides(colour=guide_legend(title="Forecasts"),fill = "black")+
  theme(legend.position="bottom")+
  theme(legend.background = element_rect(fill="white",
                                         size=0.7, linetype="solid", 
                                         colour ="gray"))                                


p8
# Table for MAPE For counry
best_recommended_model <- min(MAPE_Mean_All_NNAR,MAPE_Mean_All.bats_Model,MAPE_Mean_All.TBATS_Model,MAPE_Mean_All.Holt_Model,MAPE_Mean_All.ARIMA_Model,MAPE_Mean_All.splinef_Model,MAPE_Mean_EnsemblingAverage,MAPE_Mean_EnsemblingAverage1)
paste("System Choose Least Error ==> ( MAPE %) of Forecasting  by using NNAR model, BATS Model, TBATS Model, Holt's Linear Model , autoarima Model, cubic smoothing splines Model, Ensembling (Average), and Ensembling weight average  ,  for  ==> ", y_lab , sep=" ")
best_recommended_model
x1<-if(best_recommended_model >= MAPE_Mean_All.bats_Model) {paste("BATS Model")}
x2<-if(best_recommended_model >= MAPE_Mean_All.TBATS_Model) {paste("TBATS Model")}
x3<-if(best_recommended_model >= MAPE_Mean_All.Holt_Model) {paste("Holt Model")}
x4<-if(best_recommended_model >= MAPE_Mean_All.ARIMA_Model) {paste("ARIMA Model")}
x5<-if(best_recommended_model >= MAPE_Mean_All_NNAR) {paste("NNAR Model")}
x6<-if(best_recommended_model >= MAPE_Mean_All.splinef_Model) {paste("cubic smoothing splines")}
x7<-if(best_recommended_model >= MAPE_Mean_EnsemblingAverage) {paste("Ensembling (Average)")}
x8<-if(best_recommended_model >= MAPE_Mean_EnsemblingAverage1) {paste("Ensembling weight average")}
panderOptions('table.split.table', Inf)
paste("Forecasting by using NNAR Model  ==> ", y_lab , sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_NNAR=tail(forecasting_NNAR$mean,N_forecasting_days))), type = "rest")
paste("Forecasting by using BATS Model  ==> ", y_lab , sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_bats=tail(forecasting_bats$mean,N_forecasting_days),lower=tail(forecasting_bats$lower,N_forecasting_days),Upper=tail(forecasting_bats$lower,N_forecasting_days))), type = "rest")
paste("Forecasting by using TBATS Model  ==> ", y_lab , sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_TBATS=tail(forecasting_tbats$mean,N_forecasting_days),Lower=tail(forecasting_tbats$lower,N_forecasting_days),Upper=tail(forecasting_tbats$upper,N_forecasting_days))), type = "rest")
paste("Forecasting by using Holt's Linear Trend Model  ==> ", y_lab , sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_holt=tail(forecasting_holt$mean,N_forecasting_days),Lower=tail(forecasting_holt$lower,N_forecasting_days),Upper=tail(forecasting_holt$upper,N_forecasting_days))), type = "rest")
paste("Forecasting by using ARIMA Model  ==> ", y_lab , sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_auto.arima=tail(forecasting_auto_arima$mean,N_forecasting_days),Lower=tail(forecasting_auto_arima$lower,N_forecasting_days),Upper=tail(forecasting_auto_arima$upper,N_forecasting_days))), type = "rest")
paste("Forecasting by using cubic smoothing splines Model  ==> ", y_lab , sep=" ")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_splinef=tail(forecasting_splinef$mean,N_forecasting_days),Lower=tail(forecasting_splinef$lower,N_forecasting_days),Upper=tail(forecasting_splinef$upper,N_forecasting_days))), type = "rest")
print(ascii(data.frame(FD,forecating_date=forecasting_data_by_name,forecasting_by_splinef=tail(forecasting_splinef$mean,N_forecasting_days),Lower=tail(forecasting_holt$lower,N_forecasting_days),Upper=tail(forecasting_holt$upper,N_forecasting_days))), type = "rest")
result<-c(x1,x2,x3,x4,x5,x6,x7,x8)
table.error<-data.frame(country.name,NNAR.model=MAPE_Mean_All_NNAR, BATS.Model=MAPE_Mean_All.bats_Model,TBATS.Model=MAPE_Mean_All.TBATS_Model,Holt.Model=MAPE_Mean_All.Holt_Model,ARIMA.Model=MAPE_Mean_All.ARIMA_Model,cubic_smoothing.splines=MAPE_Mean_All.splinef_Model,Ensembling_Average=MAPE_Mean_EnsemblingAverage,Ensembling_weight=MAPE_Mean_EnsemblingAverage1,Best.Model=result)
knitr::kable(table.error,caption = paste("Accuracy MAPE % daily Covid-19 infection cases for testing data last" , validation_data_days ,frequency, y_lab , sep=" "))
MAPE.Value<-c(MAPE_Mean_All_NNAR,MAPE_Mean_All.bats_Model,MAPE_Mean_All.TBATS_Model,MAPE_Mean_All.Holt_Model,MAPE_Mean_All.ARIMA_Model,MAPE_Mean_All.splinef_Model,MAPE_Mean_EnsemblingAverage,MAPE_Mean_EnsemblingAverage1)
Model<-c("NNAR model","BATS Model","TBATS Model","Holt Model","ARIMA Model","cubic smoothing splines","Ensembling (Average)","Ensembling weight")
channel_data<-data.frame(Model,MAPE.Value)
#comparison and visualization plot accuracy models.
p0<-ggplot(channel_data, aes(x = Model, y = MAPE.Value)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = MAPE.Value)) +  # x AND y INHERITED. WE JUST NEED TO SPECIFY "label"
  coord_flip() +
  scale_y_continuous(expand = c(0, 0))
p0

# Opening the graphical device
png("Forecast NNAR Model.png")
p1
dev.off() 
png("Forecast BATS Model.png")
p2
dev.off() 
png("Forecast TBATS Model.png")
p3
dev.off() 
png("Forecast Holt Model.png")
p4
dev.off() 
png("Forecast autoarima Model.png")
p5
dev.off() 
png("Forecast cubic smoothing splines Model.png")
p6
dev.off() 
png("Ensembling Average weight.png")
p8
dev.off() 
png("accuracy of testing data.png")
p0
# Closing the graphical device
dev.off() 
#Export All Models Results
stargazer(df1,df11, type = "text",out = "NNAR Model.txt",summary = FALSE)
stargazer(df2,df21, type = "text",out = "BATS Model.txt",summary = FALSE)
stargazer(df3,df31, type = "text",out = "TBATS Model.txt",summary = FALSE)
stargazer(df4,df41, type = "text",out = "Holt Model.txt",summary = FALSE)
stargazer(df5,df51, type = "text",out = "auto arima.txt",summary = FALSE)
stargazer(df6,df61, type = "text",out = "cubic smoothing splines.txt",summary = FALSE)
stargazer(df7,df71, type = "text",out = "Ensembling Average.txt",summary = FALSE)
stargazer(df8,df81, type = "text",out = "Ensembling Average weight.txt",summary = FALSE)
stargazer(channel_data, type = "text",out = "Accuracy of All models all testing data.txt",summary = FALSE)
message("System finished Modelling and Forecasting  by using NNAR, BATS, TBATS, Holt's Linear Trend, ARIMA, cubic smoothing splines, Ensembling (Average), and Ensembling weight  ==>",y_lab, sep=" ")
message(" Thank you for using our System For Modelling and Forecasting ==> ",y_lab, sep=" ")
