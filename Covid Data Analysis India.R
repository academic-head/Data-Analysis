library(covid19.analytics)
library(prophet)
library(ggplot2)
library(dplyr)
library(lubridate)

tsc=covid19.data(case ='ts-confirmed' )

tsc=tsc %>% filter(Country.Region=='India' )
tsc


#Converting row to column
tsc=data.frame(t(tsc))


#Changing date,index to variable
tsc= cbind(rownames(tsc), data.frame(tsc, row.names=NULL))


#Giving Column Names
colnames(tsc)=c('Date','Confirmed')


#Remove the Rows as we know it is the case of USA

tsc=tsc[-c(1:4),]

#Specifying Order of Date
tsc$Date =ymd((tsc$Date))


str(tsc)
#Here we can see theconfirmed colum as Chatecter
#So convert it into numeric

tsc$Confirmed=as.numeric(tsc$Confirmed)
str(tsc)


#Quickplot

qplot(Date,Confirmed,data = tsc,main='Covid 19 Confirmed Cases in India')


#Prophet data set the arguments must be ds and y. 

ds= tsc$Date
y= tsc$Confirmed

#Create another data frame using ds and y

df= data.frame(ds,y)

# Forecasting 

m <- prophet(df) #we don' t have lot of data to capture seasonality,#so it has removed

# Prediction ( making a datafrane for future predictions)
future <- make_future_dataframe(m, periods = 28)
forecast <- predict(m, future)


#Plot Forecast
plot(m,forecast)
dyplot.prophet(m,forecast)  #Current Condition continue

#Forecast component 
prophet_plot_components (m, forecast)

#Model performance
pred = forecast$yhat [1: 481]
actual = m$history$y
plot (actual, pred)
abline(lm (pred~actual) ,col='red')
summary(lm(pred~actual))
