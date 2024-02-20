library(readr)
India_GDP_Data <- read_csv("C:/Users/ARNAB/Downloads/India_GDP_Data(1).csv")
View(India_GDP_Data)

plot(India_GDP_Data$Year,India_GDP_Data$GDP_In_Billion_USD, xlab="Year", ylab="Real GDP",  main="Graph of Real GDP")
#since we need to deal with GDP and year, eliminating the rest columns
India_GDP_Data_1<-India_GDP_Data[,1:2]
India_GDP_Data_1
View(India_GDP_Data_1)
library(ggplot2)
ggplot(India_GDP_Data_1, aes(x = Year, y= GDP_In_Billion_USD)) + 
  geom_line(color = "blue") + 
  ggtitle("Synthetic GDP Growth Over Time") + 
  xlab("Year") + 
  ylab("GDP")
#now we fit time series model to our gdp dataset
gdptime=ts(India_GDP_Data_1$GDP_In_Billion_USD,start = min(India_GDP_Data_1$Year),end = max(India_GDP_Data_1$Year),frequency = 1)
library("forecast")
library("tseries")
plot(gdptime,xlab="Year",ylab="GDP value")
acf(gdptime) #acf stands for auto correlation func
pacf(gdptime) #pacf stands for partial auto correlation
#since they are not stationary, so to eliminate the acf and pacf,we define ggpmodel
gdpmodel=auto.arima(gdptime,ic='aic',trace = T)
gdpmodel
acf(ts(gdpmodel$residuals)) #with new model, autocorr is eliminated
pacf(ts(gdpmodel$residuals))
gdpforecast=forecast(gdpmodel,level = c(99),h = 10*1) #c(99) means 99% confidence interval and h means no of frequency needed to forecast
gdpforecast
plot(gdpforecast)