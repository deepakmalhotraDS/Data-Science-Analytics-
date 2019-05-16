############################################################################################################
###########################Global Mart - Retail-Giant Sales Forecasting#####################################

#########################################Overall Approach########################################
# A. Business Problem
# a.1) Forecast the sales and the demand for the next 6 months, to manage the revenue and inventory
# a.2) Find Top 2 most profitable & consistent segment 21 (i.e., 7 different market segments and in 3 major 
#        categories) to forecast the sales and demand for Top 2 segments.
####################################
# 1. Data Understanding & Cleanup
####################################
# 2. Data Preparation Requirements
# 2.1) Segment the whole dataset into the 21 subsets based on the 7 market and the 3 customer segment level
# 2.2) Convert the transaction-level data into a time series, aggregate the 3 attributes  - Sales, Quantity & 
#    Profit, over the Order Date to arrive at monthly values for these attributes
# 2.3) Arrive at 3 time series for each of the 21 segments (7 market and 3 customer segment)
# 2.4) Find the 2 most profitable and consistently profitable segments, using metric coefficient of 
#    variation of the Profit for all 21 market segments
####################################
# 3. Model Building
# 3.1) Forecast the sales and quantity for the next 6 months of Top 2 segments 
#   3.1.1) Smoothen the data to perform classical decomposition
#   3.1.2) Use classical decomposition & auto ARIMA for forecasting
####################################
# 4. Model Evaluation
# 4.1) Forecast the sales/demand for next 6 months using the Satisfactory Model
# 4.2) Test the accuracy of forecast
#   4.2.1) Separate out the last 6 months values from the dataset, after aggregating 
#          the transaction level data into the monthly data. 
#   4.2.2) Check 6 months forecast using the out-of-sample figures using MAPE
#  Address the Business Problem in 'A'
############################################################################################################


# 1. Data Understanding & Cleanup

 #Install & Load required Packages


 install.packages("dplyr")
 install.packages("ggplot2")
 install.packages("stringr")
 install.packages("zoo")
 install.packages("forecast")
 library(dplyr)
 library(ggplot2)
 library(tidyverse) 
 library(stringr)
 library(zoo)
 library(caret)
 library(forecast)
 library(tseries)
 require(graphics)
 
 #set working directory
 setwd("C://Masters//PGDDS//Time Series Analysis")
 #load files
 gss <- read.csv("Global Superstore.csv")
 summary(gss)
 #Ensure all 24 variables are present in data
 str(gss) #51290 obs. of  24 variables
 #Row ID is not mentioned in Data Dictionary as Observation. This does not impact processing
 #No duplicate columns in the data
 #As per 2.1 to 2.4, only Category, Market, Sales, Quantity, Profit, Order Date are needed
 #Remove all rows except Order Date, Market, Segment, Sales, Quantity and Profit
 superStoreData <- select(gss, Order.Date, Market, Segment, Sales, Quantity, Profit)
 #Check for NA values and Blank columns
 sapply(superStoreData, function(x) sum(is.na(x))) #0 NA values
 sapply(superStoreData, function(x) length(which(x == ""))) # 0 blank values
 #Change Order Date into date format
 superStoreData$Order.Date<-as.Date(superStoreData$Order.Date,"%d-%m-%Y")
 #Verify if 21 subsets are available (7 markets * 3 Categories)
 summary(superStoreData$Segment) #3 Segment - Consumer, Corporate and Home Office
 summary(superStoreData$Market) #7 Markets - Africa, APAC, Canada, EMEA, EU, LATAM and US 
 #Create 21 Subset column
 superStoreData$MarketnSegment <- paste(superStoreData$Market,"-",superStoreData$Segment)
 #Rename Order date to Order_Date
 names(superStoreData)[1] <- "Order_Date"
 #superStoreData$Order_Month <- months.Date(superStoreData$Order_Date,FALSE)
 superStoreData$Order_Year <- format(as.Date(superStoreData$Order_Date, format="%d/%m/%Y"),"%Y")
 superStoreData$Order_Month <- format(as.Date(superStoreData$Order_Date, format="%d/%m/%Y"),"%m")
 superStoreData$period <- paste(superStoreData$Order_Month,superStoreData$Order_Year)

 # 2. Data Preparation Requirements
  
 #Create Month View 
 #Reference Excel Pivot View https://www.rforexcelusers.com/make-pivottable-in-r/
 # Step a: filter to 21 Segments.  
 unique(superStoreData$MarketnSegment)
 ByMonthView = filter(superStoreData, MarketnSegment %in% c("US - Consumer" , "APAC - Corporate" , "APAC - Consumer" , "EU - Home Office" , "Africa - Consumer" , "US - Corporate" , "EMEA - Consumer" , "LATAM - Home Office" , "EU - Corporate" , "EMEA - Corporate" , "LATAM - Consumer" , "EU - Consumer" , "US - Home Office" , "LATAM - Corporate" , "Africa - Corporate" , "APAC - Home Office" , "EMEA - Home Office" , "Africa - Home Office" , "Canada - Corporate" , "Canada - Home Office" , "Canada - Consumer"))
 
 # Step b: set up data frame for by-group processing by Month
 ByMonthView = group_by(ByMonthView, MarketnSegment, period)
 #COV https://rcompanion.org/rcompanion/c_02.html 
 # Step c: calculate Month wise Sales, Profit and Quantity
 ByMonthView = summarise(ByMonthView, 
                        TotalSales = sum(Sales),
                        AvgSales = mean(Sales),
                        ProfitPercentage = (sum(Profit) / sum(Sales) * 100 ), 
                        TotalQuantity = sum(Quantity),
                        TotalProfit = sum(Profit),
                        Avgprofit = mean(Profit),
                        sdProfit = sd(Profit),
                        COV = sdProfit / Avgprofit )
 


 #Create MarketnCategory View 
 # Step a: filter to 21 Segments.  

 MarkSegdf = filter(superStoreData, MarketnSegment %in% c("US - Consumer" , "APAC - Corporate" , "APAC - Consumer" , "EU - Home Office" , "Africa - Consumer" , "US - Corporate" , "EMEA - Consumer" , "LATAM - Home Office" , "EU - Corporate" , "EMEA - Corporate" , "LATAM - Consumer" , "EU - Consumer" , "US - Home Office" , "LATAM - Corporate" , "Africa - Corporate" , "APAC - Home Office" , "EMEA - Home Office" , "Africa - Home Office" , "Canada - Corporate" , "Canada - Home Office" , "Canada - Consumer"))
 
 # Step b: set up data frame for by-group processing by Month
 MarkSegdf = group_by(MarkSegdf, MarketnSegment)
 
 # Step c: calculate Month wise Sales, Profit and Quantity
 MarkSegdf = summarise(MarkSegdf, 
                         TotalSales = sum(Sales),
                         AvgSales = mean(Sales),
                         ProfitPercentage = (sum(Profit) / sum(Sales) * 100 ), 
                         TotalQuantity = sum(Quantity),
                         TotalProfit = sum(Profit),
                         Avgprofit = mean(Profit),
                         sdProfit = sd(Profit),
                         COV = sdProfit / Avgprofit)
 

 # Coefficient of variation by Market and Segment
 plot1 <- ggplot(data = MarkSegdf, aes(x = MarketnSegment, y = COV)) + geom_point() + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) ;
 Plot2 <- ggplot(aes(x=MarketnSegment, y=COV, col=COV, size=TotalProfit), data=MarkSegdf) + geom_point() + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) ;
 Plot3 <- ggplot(aes(x=MarketnSegment, y=COV, col=COV, size=TotalQuantity), data=MarkSegdf) + geom_point() + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) ;
 #ggplot(aes(x=MarketnSegment, y=COV, col=COV, size=TotalQuantity), data=MarkSegdf) + geom_point() + theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1)) ;
 
 Plot4 <- ggplot(MarkSegdf, aes(x=MarketnSegment, y=TotalProfit, fill=factor(MarketnSegment))) + 
   stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
   stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
                color="grey40",position=position_dodge(1), width=.2) +
   scale_fill_discrete("MarketnSegment") +
   theme(text = element_text(size=10), axis.text.x = element_text(angle=90, hjust=1));

 #Based on Higher Total Profit, Higher Quantity  and Lower COV
 #Top 5 in Quantity
 #MarketnSegment	TotalQuantity	  Order
 #APAC - Consumer	  21414	          1
 #LATAM - Consumer	19853	          2
 #EU - Consumer	    19541	          3
 #US - Consumer	    19521	          4
 #APAC - Corporate	12142	          5
 
 #Top 5 in Total Profits
 #MarketnSegment	 TotalQuantity	Order
 #APAC - Consumer	  222817.56	     1
 #EU - Consumer	    188687.707	   2
 #US - Consumer	    134119.209	   3
 #APAC - Corporate  129737.235	   4
 #EU - Corporate	  123393.98	     5
 
 #1. APAC - Consumer , 2. EU - Consumer, 3. US - Consumer, 4. APAC - Corporate are common in Top 5 Quantity & Profits
 #Comparing COV of these 4 Market and Segments
 #MarketnSegment			        COV
 #1. APAC - Consumer			  4.206702
 #2. EU - Consumer			    4.718084
 #3. US - Consumer          9.38945
 #4. APAC - Corporate		    4.231301
 
 #Checking top 4 Market and Segment for Monthly consistency
 
 
 ByMonthView$MarketnSegment <- str_trim(ByMonthView$MarketnSegment)
 
 
 
 APACConsumer <-  ByMonthView[which(ByMonthView$MarketnSegment == "APAC - Consumer"), ] 
 APACConsumerPlot <- ggplot(aes(x=period, y=COV, col=COV, size=TotalProfit), data=APACConsumer) + geom_point() + theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)) ;
 ggplot(APACConsumer, aes(x=period, y=COV, fill=factor(period))) + 
   stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
   stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
                color="grey40",position=position_dodge(1), width=.2) +
   scale_fill_discrete("MarketnSegment") +
   theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)); 
 #APAC_Consumer is Mostly consistent on Monthy Coefficient of Variation

  
 EUConsumer <-  ByMonthView[which(ByMonthView$MarketnSegment == "EU - Consumer"), ] 
 EUConsumerPlot <- ggplot(aes(x=period, y=COV, col=COV, size=TotalProfit), data=EUConsumer) + geom_point() + theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)) ;
 ggplot(EUConsumer, aes(x=period, y=COV, fill=factor(period))) + 
   stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
   stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
                color="grey40",position=position_dodge(1), width=.2) +
   scale_fill_discrete("MarketnSegment") +
   theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)); 
 #Mostly consistent barring 5 scenarios
 
 USConsumer <-  ByMonthView[which(ByMonthView$MarketnSegment == "US - Consumer"), ] 
 USConsumerPlot <- ggplot(aes(x=period, y=COV, col=COV, size=TotalProfit), data=USConsumer) + geom_point() + theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)) ;
 ggplot(USConsumer, aes(x=period, y=COV, fill=factor(period))) + 
   stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
   stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
                color="grey40",position=position_dodge(1), width=.2) +
   scale_fill_discrete("MarketnSegment") +
   theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)); 
 #US_Consumer is Mostly consistent on Monthy Coefficient of Variation
 
 APACCorporate <-  ByMonthView[which(ByMonthView$MarketnSegment == "APAC - Corporate"), ] 
 APACCorporatePlot <- ggplot(aes(x=period, y=COV, col=COV, size=TotalProfit), data=APACCorporate) + geom_point() + theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)) ;
 ggplot(APACCorporate, aes(x=period, y=COV, fill=factor(period))) + 
   stat_summary(fun.y=mean, geom="bar",position=position_dodge(1)) + 
   stat_summary(fun.ymin=min,fun.ymax=max,geom="errorbar",
                color="grey40",position=position_dodge(1), width=.2) +
   scale_fill_discrete("MarketnSegment") +
   theme(text = element_text(size=5), axis.text.x = element_text(angle=90, hjust=1)); 
 #APAC Corporate is high spikes in Coefficient of Variation
 
 #Picking Top 2 based on COV less than 5 from MarkSegdf as Monthly COV and Month-on-Month COV are
 #Consistent
 #1. APAC - Consumer			  4.206702 - To be considered for Time Series Data Analysis
 #2. EU - Consumer			    4.718084 - To be considered for Time Series Data Analysis
 
 str(APACConsumer)
# TIME SERIES ANALYSIS for APAC conumer ###
 # While be taking required fields #
# Taking subset of APAC Consumer and EU Consumer for Sales and Quantity analysis#
 
 # APAC Consumer for Sales#
 
 APACCustSales<- subset(superStoreData, superStoreData$MarketnSegment== "APAC - Consumer")
 APACCustSales<-select(APACCustSales,Order_Date ,Sales)

 # APAC Consumer for Quantity #
 
 APACCustQty<- subset(superStoreData, superStoreData$MarketnSegment== "APAC - Consumer")
 APACCustQty<-select(APACCustQty, Order_Date,Quantity)
 
 
 # Taking subset of APAC Consumer and EU Consumer for Sales and Quantity analysis#

 # EU Consumer for Sales#
 
 EuCustSales<- subset(superStoreData, superStoreData$MarketnSegment== "EU - Consumer")
 EuCustSales<-select(EuCustSales, Order_Date, Sales)
 
 
 # EU Consumer for Quanityt#
 
 EuCustQty<- subset(superStoreData, superStoreData$MarketnSegment== "EU - Consumer")
 EuCustQty<-select(   EuCustQty,  Order_Date,Quantity)
 

 



# Time series analysis #

# APAC Consumer- Sales #

# Taking train data

APAC_Sales_Train  <- APACCustSales
APAC_Sales_TS <- ts(APAC_Sales_Train$Sales)
plot(APAC_Sales_TS)



# Smoothing the line #

w <-1
smoothedseries_Apacsales <- stats::filter(APAC_Sales_TS, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_Apacsales[w+2] - smoothedseries_Apacsales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_Apacsales[i] <- smoothedseries_Apacsales[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Sales_TS)
diff <- smoothedseries_Apacsales[n-w] - smoothedseries_Apacsales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_Apacsales[i] <- smoothedseries_Apacsales[i-1] + diff
}

#Plot the smoothed time series

plot(APAC_Sales_TS)
lines(smoothedseries_Apacsales, col="blue", lwd=2)
train_val_APACSales<-APAC_Sales_Train$Order_Date

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf_Apacsales <- as.data.frame(cbind(train_val_APACSales, as.vector(smoothedseries_Apacsales)))
colnames(smootheddf_Apacsales) <- c('Order_Date', 'Sales')


#Seasonality test #

# Global Prediction

lmfit_APACsales <- lm(smootheddf_Apacsales$Sales  ~ sin(0.5*smootheddf_Apacsales$Order_Date ) * poly( smootheddf_Apacsales$Order_Date ,3) + cos(0.5*smootheddf_Apacsales$Order_Date) * poly(smootheddf_Apacsales$Order_Date,3)
            + smootheddf_Apacsales$Order_Date, data=smootheddf_Apacsales)

summary(lmfit_APACsales)
global_pred_APACsales <- predict(lmfit_APACsales, Period=train_val_APACSales)
summary(global_pred_APACsales)

# Local Prediction

local_pred_APACsales <- APAC_Sales_TS-global_pred_APACsales
plot(local_pred_APACsales, col='blue', type = "l")
acf(local_pred_APACsales)
acf(local_pred_APACsales, type="partial")
armafit_APACsales <- auto.arima(local_pred_APACsales)
tsdiag(armafit_APACsales)
armafit_APACsales

# Checking for white noise


residual_APACsales <- local_pred_APACsales -fitted(armafit_APACsales)

adf.test(residual_APACsales,alternative = "stationary")
kpss.test(residual_APACsales)

# Data is stationary


# by checking p- value , the data is concluded a "Stationary"


test_APACsales<- APAC_Sales_Train
timevals_out <- test_APACsales$Order_Date
global_pred_out <- predict(lmfit_APACsales ,data.frame(Order_Date=timevals_out))
fcast <- global_pred_out
fcast

MAPE_class_dec <- accuracy(fcast, test_APACsales$Sales)
MAPE_class_dec

# Chcking Auto ARIMA

autoarima_APACsales <- auto.arima(APAC_Sales_TS)

autoarima_APACsales
tsdiag(autoarima_APACsales)
plot(autoarima_APACsales$x, col="black")
lines(fitted(autoarima_APACsales), col="red")


# Checking for white noise

resi_auto_arima <-  APAC_Sales_TS- fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

summary(autoarima_APACsales)

# AS MAPE of Auto ARIMA is lower, we willdo Auto ARIMA for APAC Sales


# APAC Consumer- Quantity #



# Taking train data

APAC_Qty_Train  <- APACCustQty
APAC_Qty_TS <- ts(APAC_Qty_Train$Quantity)
plot(APAC_Qty_TS)

# Smoothing the line #

w <-1
smoothedseries_Apacqty <- stats::filter(APAC_Qty_TS, 
                                          filter=rep(1/(2*w+1),(2*w+1)), 
                                          method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_Apacqty[w+2] - smoothedseries_Apacqty[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_Apacqty[i] <- smoothedseries_Apacqty[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APAC_Qty_TS)
diff <- smoothedseries_Apacqty[n-w] - smoothedseries_Apacqty[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_Apacqty[i] <- smoothedseries_Apacqty[i-1] + diff
}

#Plot the smoothed time series

plot(APAC_Qty_TS)
lines(smoothedseries_Apacqty, col="blue", lwd=2)
train_val_APACqty<-APAC_Qty_Train$Order_Date

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf_Apacqty <- as.data.frame(cbind(train_val_APACqty, as.vector(smoothedseries_Apacqty)))
colnames(smootheddf_Apacqty) <- c('Order_Date', 'Quantity')


#Seasonality test #

# Global Prediction

lmfit_APACqty <- lm(smootheddf_Apacqty$Quantity  ~ sin(0.5*smootheddf_Apacqty$Order_Date ) * poly( smootheddf_Apacqty$Order_Date ,3) + cos(0.5*smootheddf_Apacqty$Order_Date) * poly(smootheddf_Apacqty$Order_Date,3)
                      + smootheddf_Apacqty$Order_Date, data=smootheddf_Apacqty)

summary(lmfit_APACqty)
accuracy(lmfit_APACsales)
global_pred_APACqty <- predict(lmfit_APACqty, Period=train_val_APACqty)
summary(global_pred_APACqty)

# Local Prediction

local_pred_APACqty <- APAC_Qty_TS-global_pred_APACqty
plot(local_pred_APACqty, col='blue', type = "l")
acf(local_pred_APACqty)
acf(local_pred_APACqty, type="partial")
armafit_APACqty <- auto.arima(local_pred_APACqty)
tsdiag(armafit_APACqty)
armafit_APACqty

# Checking for white noise


residual_APACqty <- local_pred_APACqty -fitted(armafit_APACqty)

adf.test(residual_APACqty,alternative = "stationary")
kpss.test(residual_APACqty)


# by checking p- value , the data is concluded a "Stationary"


test_Apac_Qty<- APAC_Qty_Train
timevals_out_qty_APAC <- test_Apac_Qty$Order_Date
global_pred_out_sales_APAC <- predict(lmfit_APACqty ,data.frame(Order_Date= timevals_out_qty_APAC))
fcast_Apac_qty <- global_pred_out_sales_APAC
fcast_Apac_qty 

MAPE_class_dec_ApacQty <- accuracy(fcast_Apac_qty, test_Apac_Qty$Quantity)
MAPE_class_dec_ApacQty

# Chcking Auto ARIMA

autoarima_APACqty <- auto.arima(APAC_Qty_TS)

autoarima_APACqty
tsdiag(autoarima_APACqty)
plot(autoarima_APACqty$x, col="black")
lines(fitted(autoarima_APACqty), col="red")


# Checking for white noise

resi_auto_arima_APACqty <-  APAC_Qty_TS- fitted(autoarima_APACqty)

adf.test(resi_auto_arima_APACqty,alternative = "stationary")
kpss.test(resi_auto_arima_APACqty)

summary(autoarima_APACsales)

# AS MAPE of Auto ARIMA is lower, we willdo Auto ARIMA for APAC Quantity



# EU Consumer- Quantity #



# Taking train data

EU_Qty_Train  <- EuCustQty 
EU_Qty_TS <- ts(EU_Qty_Train$Quantity)
plot(EU_Qty_TS)

# Smoothing the line #

w <-1
smoothedseries_EUQty <- stats::filter(EU_Qty_TS, 
                                        filter=rep(1/(2*w+1),(2*w+1)), 
                                        method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_EUQty[w+2] - smoothedseries_EUQty [w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EUQty [i] <- smoothedseries_EUQty [i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Qty_TS)
diff <- smoothedseries_EUQty[n-w] - smoothedseries_EUQty[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_EUQty[i] <- smoothedseries_EUQty[i-1] + diff
}

#Plot the smoothed time series

plot(EU_Qty_TS)
lines(smoothedseries_EUQty , col="blue", lwd=2)
train_val_EUqty<-EU_Qty_Train$Order_Date

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf_EUqty <- as.data.frame(cbind(train_val_EUqty, as.vector(smoothedseries_EUQty)))
colnames(smootheddf_EUqty) <- c('Order_Date', 'Quantity')


#Seasonality test #

# Global Prediction

lmfit_Euqty <- lm(smootheddf_EUqty$Quantity  ~ sin(0.5*smootheddf_EUqty$Order_Date ) * poly( smootheddf_EUqty$Order_Date ,3) + cos(0.5*smootheddf_EUqty$Order_Date) * poly(smootheddf_EUqty$Order_Date,3)
                    + smootheddf_EUqty$Order_Date, data=smootheddf_EUqty)

summary(lmfit_Euqty)
accuracy(lmfit_Euqty)
global_pred_EUqty <- predict(lmfit_Euqty, Period=train_val_EUqty)
summary(global_pred_EUqty)

# Local Prediction

local_pred_Euqty <- EU_Qty_TS - global_pred_EUqty
plot(local_pred_Euqty, col='blue', type = "l")
acf(local_pred_Euqty)
acf(local_pred_Euqty, type="partial")
armafit_Euqty <- auto.arima(local_pred_Euqty)
tsdiag(armafit_Euqty)
armafit_Euqty

# Checking for white noise


residual_Euqty <- local_pred_Euqty -fitted(armafit_Euqty)

adf.test(residual_Euqty,alternative = "stationary")
kpss.test(residual_Euqty)


# by checking p- value , the data is concluded a "Stationary"


test_EU_Qty<- EU_Qty_Train
timevals_out_qty_EU <- test_EU_Qty$Order_Date
global_pred_out_Qty_EU <- predict(lmfit_Euqty ,data.frame(Order_Date= timevals_out_qty_EU))
fcast_EU_Qty <- global_pred_out_Qty_EU
fcast_EU_Qty

MAPE_class_dec_EuQty <- accuracy(fcast_EU_Qty, test_EU_Qty$Quantity)
MAPE_class_dec_EuQty

# Chcking Auto ARIMA

autoarima_EU_Qty <- auto.arima(EU_Qty_TS )

autoarima_EU_Qty
tsdiag(autoarima_EU_Qty)
plot(autoarima_EU_Qty$x, col="black")
lines(fitted(autoarima_EU_Qty), col="red")


# Checking for white noise

resi_auto_arima_Euqty <-  EU_Qty_TS - fitted(autoarima_EU_Qty)

adf.test(resi_auto_arima_Euqty,alternative = "stationary")
kpss.test(resi_auto_arima_Euqty)

summary(autoarima_EU_Qty)

# AS MAPE of Auto ARIMA is lower, we willdo Auto ARIMA for EU Quantity


# EU Consumer- Sales #



# Taking train data

EU_Sales_Train  <- EuCustSales
EU_Sales_TS <- ts(EU_Sales_Train$Sales)
plot(EU_Sales_TS)

# Smoothing the line #

w <-1
smoothedseries_EUSales <- stats::filter(EU_Sales_TS, 
                                        filter=rep(1/(2*w+1),(2*w+1)), 
                                        method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_EUSales[w+2] - smoothedseries_EUSales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EUSales[i] <- smoothedseries_EUSales[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EU_Sales_TS)
diff <- smoothedseries_EUSales[n-w] - smoothedseries_EUSales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_EUSales[i] <- smoothedseries_EUSales[i-1] + diff
}

#Plot the smoothed time series

plot(EU_Sales_TS)
lines(smoothedseries_EUSales, col="blue", lwd=2)
train_val_EUSales<-EU_Sales_Train$Order_Date

#Building a model on the smoothed time series using classical decomposition
#converting the time series to a dataframe

smootheddf_EUSales <- as.data.frame(cbind(train_val_EUSales, as.vector(smoothedseries_EUSales)))
colnames(smootheddf_EUSales) <- c('Order_Date', 'Sales')


#Seasonality test #

# Global Prediction

lmfit_Eusales <- lm(smootheddf_EUSales$Sales  ~ sin(0.5*smootheddf_EUSales$Order_Date ) * poly( smootheddf_EUSales$Order_Date ,3) + cos(0.5*smootheddf_EUSales$Order_Date) * poly(smootheddf_EUSales$Order_Date,3)
                    + smootheddf_EUSales$Order_Date, data=smootheddf_EUSales)

summary(lmfit_Eusales)
accuracy(lmfit_Eusales)
global_pred_EUsales <- predict(lmfit_Eusales, Period=train_val_EUSales)
summary(global_pred_EUsales)

# Local Prediction

local_pred_Eusales <- EU_Sales_TS - global_pred_EUsales
plot(local_pred_Eusales, col='blue', type = "l")
acf(local_pred_Eusales)
acf(local_pred_Eusales, type="partial")
armafit_Eusales <- auto.arima(local_pred_Eusales)
tsdiag(armafit_Eusales)
armafit_Eusales

# Checking for white noise


residual_Eusales <- local_pred_Eusales -fitted(armafit_Eusales)

adf.test(residual_Eusales,alternative = "stationary")
kpss.test(residual_Eusales)


# by checking p- value , the data is concluded a "Stationary"


test_EU_Sales<- EU_Sales_Train
timevals_out_sales_EU <- test_EU_Sales$Order_Date
global_pred_out_sales_EU <- predict(lmfit_Eusales ,data.frame(Order_Date= timevals_out_sales_EU))
fcast_EU_sales <- global_pred_out_sales_EU
fcast_EU_sales 

MAPE_class_dec_EUsales <- accuracy(fcast_EU_sales, test_EU_Sales$Sales)
MAPE_class_dec_EUsales

# Chcking Auto ARIMA

autoarima_EU_sales <- auto.arima(EU_Sales_TS)

autoarima_EU_sales
tsdiag(autoarima_EU_sales)
plot(autoarima_EU_sales$x, col="black")
lines(fitted(autoarima_EU_sales), col="red")


# Checking for white noise

resi_auto_arima_Eusale <-  EU_Sales_TS- fitted(autoarima_EU_sales)

adf.test(resi_auto_arima_Eusale,alternative = "stationary")
kpss.test(resi_auto_arima_Eusale)

summary(autoarima_EU_sales)

# AS MAPE of Auto ARIMA is lower, we willdo Auto ARIMA for EU sales

# Forecasting Sales and Quantity for EU and APAC regions

forecast_APAC_sales<- forecast(autoarima_APACsales, h=6)
forecast_APAC_sales
plot(forecast_APAC_sales)

forecast_EU_sales<- forecast(autoarima_EU_sales, h=6)
forecast_EU_sales
plot(forecast_EU_sales)

forecast_APAC_qty<- forecast(autoarima_APACqty, h=6)
forecast_APAC_qty
plot(forecast_APAC_qty)

forecast_EU_qty<- forecast(autoarima_EU_Qty, h=6)
forecast_EU_qty
plot(forecast_EU_qty)
