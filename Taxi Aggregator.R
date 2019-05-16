##Uber Demand Supply Gap Analysis##
##Importing all relevant packages. 
##Set working directory to path on local system containing the uber request data set.
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)

##Importing the Dataset into the R-enviornment
uber_data <- read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
str(uber_data)
View(uber_data)

########DATA CLEANING & PREPERATION#####################
##Check for duplicates, nothing found
sum(duplicated(uber_data$Request.id))

##Check NA values, nothing found
sum(is.na(uber_data$Request.id))
sum(is.na(uber_data$Pickup.point))
sum(is.na(uber_data$Status))
sum(is.na(uber_data$Request.timestamp))

#Formatting Date and Time in consistent format
uber_data$Request.timestamp<- str_replace_all(uber_data$Request.timestamp, "[/]", "-") 
uber_data$Drop.timestamp<- str_replace_all(uber_data$Drop.timestamp, "[/]", "-")

#Convert into R standard format, ignore seconds.
uber_data$Request.timestamp<- as.POSIXct(uber_data$Request.timestamp, format= "%d-%m-%Y %H:%M")
uber_data$Drop.timestamp<- as.POSIXct(uber_data$Drop.timestamp, format= "%d-%m-%Y %H:%M")

##Deriving Relevant(hourly and daily) metrics from the dataset. 
uber_data$Req.hr<- as.integer(format(uber_data$Request.timestamp, "%H"))
uber_data$Req.day<- as.factor(weekdays(uber_data$Request.timestamp))

#creating a categorical attribute to quantify service levels.
uber_data$Service.status[uber_data$Status=="Trip Completed"]<- "Customer Serviced"
uber_data$Service.status[uber_data$Status=="Cancelled"]<- "Customer Denied"
uber_data$Service.status[uber_data$Status=="No Cars Available"]<- "Customer Denied"

## Analysis on Daily requests ## not much variation
ggplot(uber_data, aes(x = as.factor(uber_data$Req.day),fill = Pickup.point))+geom_bar()+ labs(title = "Daily Demand Analysis",x = "Day", y = "Number of Requests", fill = "Pickup Point" )

##Check hourly Peak Demand Times:
ggplot(uber_data, aes(x = as.factor(uber_data$Req.hr),fill = Pickup.point))+geom_bar()+labs(title = "Hourly Demand Based on Pickup Points",x = "Hour", y = "Number of Requests", fill = "Pickup Point" )


#Plotting Overall Service Rate [Requests Denied vs. Requests Serviced %]
service_rate<- ggplot(uber_data, aes(x = as.factor(uber_data$Service.status), fill=factor(uber_data$Service.status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) + 
  labs(title = "Demand vs. Service Rate", y = "% of Requests", x = "Service Rate") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Service Status")
service_rate

#Breaking up Overall Denied Analysis into Completed, Denied or No Cars.
Req_analysis<- ggplot(uber_data, aes(x = as.factor(uber_data$Status), fill=factor(uber_data$Status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) + 
  labs(title = "Request Status Catagorized", y = "% of Requests", x = "Request Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Request Status")
Req_analysis

#Creating a column for the Time slot based on Demand pattern
uber_data$Time.slot<- ifelse(uber_data$Req.hr>=5 & uber_data$Req.hr<=9,"Morning_Peak", ifelse(uber_data$Req.hr>=10 & uber_data$Req.hr<=16,"Day_Time", ifelse(uber_data$Req.hr>=17 & uber_data$Req.hr<=21,"Evening_Peak", "Night_Time")))


#Let's Observe Total Hourly Demand vs. Total Supply breakdown for this Analysis.
##Creating a new grouped dataset showing Request hour, demand and supply.
hourly_demand<- uber_data %>% group_by("Request Hour"=uber_data$Req.hr) %>% summarise("Total_Demand" = n())
hourly_supply<- subset(uber_data, uber_data$Status=="Trip Completed") 
hourly_supply<- group_by(hourly_supply, "Request Hour"=hourly_supply$Req.hr) %>% summarise("Total_Supply" = n())
hourly_analysis<- cbind(hourly_demand,hourly_supply[,-1])
hourly_analysis$Supply_gap<- hourly_analysis$Total_Demand - hourly_analysis$Total_Supply

#Plotting Hourly Supply-Demand Gap Trend
hourly_gap<- ggplot(hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Demand, group=1))+ geom_line(col= "red", size=2)+ geom_point(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Demand),col="black", size=2)+
  geom_line(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Supply, group=1), col="green", size=1) + geom_point(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Total_Supply),col="black", size=1)+
  geom_line(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Supply_gap, group=1), col="blue", size=1.5) + geom_point(data=hourly_analysis, aes(x=as.factor(hourly_analysis$`Request Hour`), y=hourly_analysis$Supply_gap),col="yellow",size=1.5)+
  labs(title = "Aggregated Hourly Demand vs Supply and Gap Trend", y = "Number of Requests", x = "Request Hour")
hourly_gap

#Time-Slot basis Demand and Supply Analysis
##Creating a new grouped dataset showing Time slot, demand and supply.
time_slot_demand<- uber_data %>% group_by("Request Time Slot"= uber_data$Time.slot) %>% summarise("Total_Demand"=n())
time_slot_supply<- subset(uber_data, uber_data$Status=="Trip Completed") 
time_slot_supply<- group_by(time_slot_supply, "Request Time Slot"= time_slot_supply$Time.slot) %>% summarise("Total_Supply" = n())
time_slot_analysis<- cbind(time_slot_demand,time_slot_supply[,-1]) %>% arrange(desc(time_slot_demand$Total_Demand))
time_slot_analysis$Supply_gap<- time_slot_analysis$Total_Demand - time_slot_analysis$Total_Supply 

#Plotting Time slot Supply-Demand Gap Trend
timeslot_gap<- ggplot(time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Demand, group=1))+ geom_line(col= "red", size=2)+ geom_point(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Demand),col="black", size=2)+
  geom_line(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Supply, group=1), col="green", size=1) + geom_point(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Total_Supply),col="black", size=1)+
  geom_line(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Supply_gap, group=1), col="blue", size=1.5) + geom_point(data=time_slot_analysis, aes(x=as.factor(time_slot_analysis$`Request Time Slot`), y=time_slot_analysis$Supply_gap),col="yellow",size=1.5)+
  labs(title = "Demand vs. Supply and Gap Trend per Time Slots", y = "Number of Requests", x = "Time-Slot")
timeslot_gap

#Request analysis based upon pick up points:
#Airport requests:
airport_pickup<- subset(uber_data, uber_data$Pickup.point=="Airport" )
airport_analysis<-ggplot(airport_pickup, aes(x = as.factor(airport_pickup$Status), fill=factor(airport_pickup$Status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  labs(title = "Airport-Pickup Requests", y = "% of Requests", x = "Request Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Request Status")
airport_analysis

# City Requests:
city_pickup<- subset(uber_data, uber_data$Pickup.point=="City")
city_status_breakup<-ggplot(city_pickup, aes(x = as.factor(city_pickup$Status), fill=factor(city_pickup$Status))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = percent) +
  labs(title = "City-Pickup Request", y = "% of Requests", x = "Request Status") + geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust=-0.3)+
  scale_fill_discrete(name = "Request Status")
city_status_breakup

#Request Analysis based on Time Slots 
#Time-Slot Status Analysis based grouped time slots
timeslot_breakup<- ggplot(uber_data, aes(x=as.factor(uber_data$Time.slot), fill=factor(uber_data$Status)))+
  geom_bar()+ labs(title = "Request Status per Time-Slots", y = "Number of Requests", x = "Time-Slots", fill= "Request Status")
timeslot_breakup

#Time slot analysis for Airport Pick-up.
airport_pickup_analysis<- ggplot(airport_pickup, aes(x=as.factor(airport_pickup$Time.slot), fill=factor(airport_pickup$Status)))+
  geom_bar()+ labs(title = "Airport Pickup Analysis", y = "Number of Requests", x = "Request Time-Slots", fill= "Request Status")
airport_pickup_analysis

#Time slot analysis for City Pick-up.
city_pickup_analysis<- ggplot(city_pickup, aes(x=as.factor(city_pickup$Time.slot), fill=factor(city_pickup$Status)))+
  geom_bar()+ labs(title = "City Pickup Analysis", y = "Number of Requests", x = "Request Time-Slots", fill= "Request Status")
city_pickup_analysis


#Problem 1: Request cancellation by drivers during Morning Peak hour
#Location based Demand-Supply Gap for Morning_Peak_Hour. 
morning_peak_analysis<- ggplot(data=subset(uber_data, uber_data$Time.slot=="Morning_Peak"), aes(x=as.factor(Status), fill=factor(Pickup.point)))+
  geom_bar()+ labs(title = "Morning Peak Hour Analysis", y = "Number of Requests", x = "Request Status", fill= "Pickup Location")
morning_peak_analysis

#City demand Supply Gap
Problem1_a <- subset(uber_data, uber_data$Time.slot=="Morning_Peak" & uber_data$Pickup.point=="City")
nrow(subset(Problem1_a, Problem1_a$Pickup.point == "City" & Problem1_a$Status == "Trip Completed"))
nrow(subset(Problem1_a, Problem1_a$Pickup.point == "City" ))

#severity of the issue location wise
Problem1_b <- subset(uber_data,uber_data$Time.slot=="Morning_Peak" & uber_data$Status=="Cancelled")
nrow(subset(Problem1_b, Problem1_b$Pickup.point == "Airport"))
nrow(subset(Problem1_b, Problem1_b$Pickup.point == "City" ))


#Problem 2: No cars available during Evening Peak Hour Hours.
#Location based Demand-Supply Gap forEvening Peak Hour
evening_peak_analysis<- ggplot(data=subset(uber_data, uber_data$Time.slot=="Evening_Peak"), aes(x=as.factor(Status), fill=factor(Pickup.point)))+
  geom_bar()+ labs(title = "Evening Peak Hour Analysis", y = "Number of Requests", x = "Request Status", fill= "Pickup Location")
evening_peak_analysis

#Airpot Demand Supply Gap
Problem2_a <- subset(uber_data, uber_data$Time.slot=="Evening_Peak" & uber_data$Pickup.point=="Airport")
nrow(subset(Problem2_a, Problem2_a$Pickup.point == "Airport" & Problem2_a$Status == "Trip Completed"))
nrow(subset(Problem2_a, Problem2_a$Pickup.point == "Airport" ))

#severity of the issue location wise
Problem2_b <- subset(uber_data,uber_data$Time.slot=="Evening_Peak" & uber_data$Status=="No Cars Available")
nrow(subset(Problem2_b, Problem2_b$Pickup.point == "Airport"))
nrow(subset(Problem2_b, Problem2_b$Pickup.point == "City" ))

###Code end..The analysis and recommendations are mentioned in the .ppt file!!####
###############################################################################

