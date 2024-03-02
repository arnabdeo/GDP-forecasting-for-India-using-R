library(readr) #selecting the csv file
India_GDP_Data_1_ <- read_csv("C:/Users/ARNAB/Downloads/India_GDP_Data(1).csv")
View(India_GDP_Data_1_)
india=India_GDP_Data_1_[,2] #to select only the GDP(in USD billions) column
india
library(forecast)
library(tseries)
indiats=ts(india,frequency = 1)
autoplot(indiats)
adf.test(indiats,k = 1) #p value shouldn't be greater than 0.01 hence it is not stationary
#to make it stationary we need differencing for many times until we get p value=0.01
indiats_d1=diff(indiats,differences = 1)
#here with first differencing got p value as 0.01
adf.test(indiats_d1,k = 1) #hence d=1
autoplot(indiats_d1)
#to count the no of spikes outside the considerable range, here I have spikes at 4,7,9. 
#i consider p=9 such that it will be easier to forecast
pacf(indiats_d1) #p=4
#to count the no of spikes outside the considerable range, here I have spikes at 4,7,11. 
#i consider q=4 such that it will be easier to forecast
acf(indiats_d1) #q=4
india_arima=arima(indiats,order = c(9,1,4)) #Arima model(p,d,q) where p=9,d=1,q=4
forecast(india_arima,h = 30)
autoplot(forecast(india_arima,h=30))
