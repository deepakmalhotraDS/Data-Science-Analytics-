#############################Telecom Solution###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Based on the past and current employee information
# the company has maintained a database containing employee information sch as years of service, experience, etc
# Also the data in regards to Survey feedback
# Comany also maintanied punch in and punch out time of employees

## GOAL

#To understand what factors the company should focus on, in order to curb attrition
# What changes the company should make to their workplace, in order to get most of their employees to stay


###############################################################################

#### Data Understanding::

# Attrition---is the dependent variable
# Remaining other variables are independent variables
# Two Survey data available
# Seperate In time and out times are provided.

################################################################################33

# Install and Load the required packages


install.packages("MASS")
install.packages("e1071")
install.packages("cowplot")
install.packages("GGally")
install.packages("dplyr")
install.packages("ggthemes")
install.packages("ggplot2")
install.packages("plyr")
install.packages("data.table")
install.packages("lubricate")
install.packages("sqldf")
install.packages("sqldf")
install.packages("caret")

library(sqldf)
library(dplyr)
library(MASS)
library(car)
library(e1071)
library(ggplot2)
library(cowplot)
library(dplyr)
library(caTools)
library(reshape2)
library(plyr)
library(lubridate)
library(caret)




# Loading the files
emplyoee_survey<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general_data<- read.csv("general_data.csv", stringsAsFactors = F)
manager_survey<- read.csv("manager_survey_data.csv", stringsAsFactors = F)


# understanding the data

str(emplyoee_survey)  # 4410 obs. of  4 variables
str(general_data)     # 4410 obs. of  24 variables
str(manager_survey)   # 4410 obs. of  3 variables


# Checking for Duplicates

length(unique(emplyoee_survey$EmployeeID))
length(unique(general_data$EmployeeID))
length(unique(manager_survey$EmployeeID))  # No Duplicates found

# Reading "in time " and "out time" dat frames
# inserting "Employee ID" column name for "in time"  and "out time" data frame


in_TimeData <- read.csv("in_time.CSV", check.names=FALSE)
out_TimeData <- read.csv("out_time.CSV", check.names=FALSE)
names(in_TimeData)[1] <- "EmployeeID"
names(out_TimeData)[1] <- "EmployeeID"

str(in_TimeData)          # 4410 obs. of  262 variables
str(out_TimeData)         # 4410 obs. of  262 variables

#inTimeData_long <- gather(inTimeData, Empid, workingDay, '2015-01-01':'2015-12-31', factor_key=FALSE)
#Convert Wide data into Long format

in_TimeData_long <- melt(in_TimeData, id.vars=c("EmployeeID"))
names(in_TimeData_long)[2] <- "calendarday"
names(in_TimeData_long)[3] <- "Intime"
out_TimeData_long <- melt(out_TimeData, id.vars=c("EmployeeID"))
names(out_TimeData_long)[2] <- "calendarday"
names(out_TimeData_long)[3] <- "Outtime"

nrow(out_TimeData_long)
nrow(in_TimeData_long)

#Both Data frames has 1151010 rows with same Emp ID and dates, no discrepancy and hence can be merged

#Joining by: Empid, calendarday
Time_data <- join(in_TimeData_long, out_TimeData_long, type = "inner")

#Find the day of the week
Time_data$day <- weekdays(as.Date(Time_data$calendarday))
#Get time in office per day
Time_data$timeinOffice <- round(as.POSIXct(strptime(Time_data$Outtime, "%Y-%m-%d %H:%M:%S")) - as.POSIXct(strptime(Time_data$Intime, "%Y-%m-%d %H:%M:%S")),digits = 2)
#get week day
Time_data$weekNum <- week(Time_data$calendarday)
#identified worked Vs Holiday days
Time_data$daytype <- sapply(Time_data$Intime, function(x) {ifelse(is.na(x), "Didnot work","worked")})
#Identify Month
Time_data$Monthname <- month(Time_data$calendarday)
Time_dataworkinghours <- Time_data %>% filter(daytype != "Didnot work")


#avgworkinghours <- Timedataworkinghours %>%
#                    group_by(Empid) %>%
#                    select(Empid,  timeinOffice) %>%
#                    summarise(timeinOffice == mean(timeinOffice))

avg_workinghours <- sqldf('Select EmployeeID, avg(timeinOffice) as avghours from Time_dataworkinghours group by EmployeeID')
number_of_holidays <- sqldf('Select EmployeeID, count(daytype) as nonworkingdays from Time_data where daytype = "Didnot work" group by EmployeeID')
avg_timeinoffice <- sqldf('Select avg(timeinOffice) as avghours from Time_dataworkinghours')
#7.708608 is the average time of all employees in office

#Joining by: Employeeid
final_Timedata <- join(avg_workinghours, number_of_holidays, type = "inner")


# Merging all the data in a single file

employee_data<-merge(emplyoee_survey,manager_survey, by= "EmployeeID",all=F)
employee_data<-merge(employee_data,general_data, by= "EmployeeID",all=F)
employee_data<-merge(employee_data,final_Timedata,by= "EmployeeID",all=F )

str(employee_data)

# Rounding off "Working Hours"

employee_data$avghours<- round(employee_data$avghours,1)


# understandin the data by plotting graphs, and check on the major impact variables on Attrition

# Grouping all the numeric variable to understand the Impact of  "Grouping variable" to Attrition

# Grouping of Age

for (i in 1:nrow(employee_data))
{
  temp<- employee_data[i, ]
  temp
  if (temp$Age>= 18 & temp$Age <26)
  {
    employee_data[i,"Age Group" ]<- "18 to 25 Years"
  }
  else if ( temp$Age>= 26 & temp$Age < 36)
  {
    employee_data[i,"Age Group" ]<-"26 to 35 Years"
  }
  else if ( temp$Age>=  36 & temp$Age < 46)
  {
    employee_data[i,"Age Group" ]<- "36 to 45 Years"
  }
  else if ( temp$Age>=  46 & temp$Age < 56)
  {
    employee_data[i,"Age Group" ]<- "46 to 55 Years"
  }
  else 
  {
    employee_data[i,"Age Group" ]<- "Greater than 55 Years"
  }
}  

# Grouping "Distance from office"

for (i in 1:nrow(employee_data))
{
  temp_dist<- employee_data[i, ]
  temp_dist
  if (temp_dist$DistanceFromHome>= 1 & temp_dist$DistanceFromHome < 11)
  {
    employee_data[i,"Distance From Home" ]<- "Less than 10 Kms"
  }
  
  else if (temp_dist$DistanceFromHome>= 11 & temp_dist$DistanceFromHome <21)
  {
    employee_data[i,"Distance From Home" ]<- "11 to 20 Kms"
  }
  
  else if (temp_dist$DistanceFromHome>= 21 & temp_dist$DistanceFromHome <31)
  {
    employee_data[i,"Distance From Home" ]<- "21 to 30 Kms"
  }
  else 
  {
    employee_data[i,"Distance From Home" ]<- "Greater than 30 Kms"
  }
}  

# Grouping "Salary "

for (i in 1:nrow(employee_data))
{
  temp_sal<- employee_data[i, ]
  temp_sal
  if (temp_sal$MonthlyIncome>= 9000 & temp_sal$MonthlyIncome < 15001)
  {
    employee_data[i,"Salary Group" ]<- "9000 to 15000"
  }
  
  else if (temp_sal$MonthlyIncome>= 15001 & temp_sal$MonthlyIncome < 50001)
  {
    employee_data[i,"Salary Group" ]<- "15001 to 50000"
  }
  
  else if (temp_sal$MonthlyIncome>= 50001 & temp_sal$MonthlyIncome < 100001)
  {
    employee_data[i,"Salary Group" ]<- " 50001 to 100000"
  }
  else if (temp_sal$MonthlyIncome>= 100001 & temp_sal$MonthlyIncome < 150001)
  {
    employee_data[i,"Salary Group" ]<- " 100001 to 150000 "
  }
  
  else 
  {
    employee_data[i,"Salary Group" ]<- "Greater than 150000 "
  }
} 


# Grouping on "Percent Salary Hike"

for (i in 1:nrow(employee_data))
{
  temp_hike<- employee_data[i, ]
  temp_hike
  if (temp_hike$PercentSalaryHike>= 11 & temp_hike$PercentSalaryHike < 16)
  {
    employee_data[i,"Salary Hike %" ]<- "11 to 15"
  }
  
  else if (temp_hike$PercentSalaryHike>= 16 & temp_hike$PercentSalaryHike < 21)
  {
    employee_data[i,"Salary Hike %" ]<- "16 to 20"
  }
  else 
  {
    employee_data[i,"Salary Hike %" ]<- "Greater 20 "
  }
} 

# Grouping on "Working years"

for (i in 1:nrow(employee_data))
{
  temp_exp<- employee_data[i, ]
  temp_exp
  if (!is.na(temp_exp$TotalWorkingYears) & temp_exp$TotalWorkingYears >=0 & temp_exp$TotalWorkingYears <6)
  {
    employee_data[i,"Experience" ]<- "Below 5 Years"
  }
  
  else if ((!is.na(temp_exp$TotalWorkingYears))&temp_exp$TotalWorkingYears>= 6 & temp_exp$TotalWorkingYears < 11)
  {
    employee_data[i,"Experience" ]<- "6 to 10 Years"
  }
  else if ((!is.na(temp_exp$TotalWorkingYears))&temp_exp$TotalWorkingYears>= 11 & temp_exp$TotalWorkingYears < 16)
  {
    employee_data[i,"Experience" ]<- "11 to 15 Years"
  }
  else if ((!is.na(temp_exp$TotalWorkingYears))&temp_exp$TotalWorkingYears>= 16  & temp_exp$TotalWorkingYears < 25)
  {
    employee_data[i,"Experience" ]<- "16 to 25 Years"
  }
  
  else 
  {
    employee_data[i,"Experience"]  <-"Greater 25 "
  }
} 

# Grouping on "Years At Company"

for (i in 1:nrow(employee_data))
{
  temp_years<- employee_data[i, ]
  temp_years
  if ( temp_years$YearsAtCompany >= 0 & temp_years$YearsAtCompany<6)
  {
    employee_data[i,"Years at Company" ]<- "Below 5 Years"
  }
  
  else if (temp_years$YearsAtCompany>= 6 & temp_years$YearsAtCompany < 11)
  {
    employee_data[i,"Years at Company" ]<- "6 to 10 Years"
  }
  else if (temp_years$YearsAtCompany>= 11 & temp_years$YearsAtCompany < 16)
  {
    employee_data[i,"Years at Company" ]<- "11 to 15 Years"
  }
  else if (temp_years$YearsAtCompany>= 16  & temp_years$YearsAtCompany < 25)
  {
    employee_data[i,"Years at Company" ]<- "16 to 25 Years"
  }
  
  else 
  {
    employee_data[i,"Years at Company"]  <-"Greater 25 "
  }
} 

# Grouping on "Years since promotion"

for (i in 1:nrow(employee_data))
{
  temp_prom<- employee_data[i, ]
  temp_prom
  if ( temp_prom$YearsSinceLastPromotion >= 0 & temp_prom$YearsSinceLastPromotion<6)
  {
    employee_data[i,"Since Promotion" ]<- "Below 5 Years"
  }
  
  else if ( temp_prom$YearsSinceLastPromotion >= 6 & temp_prom$YearsSinceLastPromotion<11)
  {
    employee_data[i,"Since Promotion" ]<- "6 to 10 years"
  }
  
  else if ( temp_prom$YearsSinceLastPromotion >= 11 & temp_prom$YearsSinceLastPromotion<16)
  {
    employee_data[i,"Since Promotion" ]<- "11 to 15 years"
  }
  
  else 
  {
    employee_data[i,"Since Promotion" ]<- "Greater than 15 years"
  }
} 

# Grouping on "Years with smae manager"

for (i in 1:nrow(employee_data))
{
  temp_same<- employee_data[i, ]
  temp_same
  if ( temp_same$YearsWithCurrManager >= 0 & temp_same$YearsWithCurrManager <6)
  {
    employee_data[i,"Years with current manager" ]<- "Below 5 Years"
  }
  
  else if ( temp_same$YearsWithCurrManager >= 6 & temp_same$YearsWithCurrManager <11)
  {
    employee_data[i,"Years with current manager" ]<- "6 to 10 years"
  }
  
  else 
  {
    employee_data[i,"Years with current manager" ]<- "Greater than 10 years"
  }
} 

# Grouping on "Average hours per day"

for (i in 1:nrow(employee_data))
{
  temp_hrs<- employee_data[i, ]
  temp_hrs
  if ( temp_hrs$avghours >= 6.0 & temp_hrs$avghours  <7.1)
  {
    employee_data[i,"Avg hours per day" ]<- "6 to 7 hours"
  }
  
  else if ( temp_hrs$avghours >= 7.1 & temp_hrs$avghours  <8.1)
  {
    employee_data[i,"Avg hours per day" ]<- "7 to 8 hours"
  }
  else if ( temp_hrs$avghours >= 8.1 & temp_hrs$avghours  <9.1)
  {
    employee_data[i,"Avg hours per day" ]<- "8 to 9 hours"
  }
  else if ( temp_hrs$avghours >= 9.1 & temp_hrs$avghours  <10.1)
  {
    employee_data[i,"Avg hours per day" ]<- "9 to 10 hours"
  }
  else 
  {
    employee_data[i,"Avg hours per day" ]<- "Greater than 10 hours"
  }
} 
# Grouping on "Non Working Daya in the year"

for (i in 1:nrow(employee_data))
{
  temp_hol<- employee_data[i, ]
  temp_hol
  if ( temp_hol$nonworkingdays >= 13 & temp_hol$nonworkingdays<21)
  {
    employee_data[i,"Non Working Days" ]<-" 13 to 20 Days"
  }
  
  else if ( temp_hol$nonworkingdays >= 21 & temp_hol$nonworkingdays<31)
  {
    employee_data[i,"Non Working Days" ]<-" 21 to 30 Days"
  }
  else if ( temp_hol$nonworkingdays >= 31 & temp_hol$nonworkingdays<36)
  {
    employee_data[i,"Non Working Days" ]<-" 31 to 35 Days"
  }
   else 
  {
    employee_data[i,"Non Working Days" ]<-" Greater than 35 Days"
  }
} 
# Analysing on Categorical independent varibles on Attrition

# "Business Travel:, "Department", "Education", "Gender", "Job Role", "MaritalStatus" are the Categorical Variables

# "Age", "Distance from home", "Education", "Job Level", "Monthly income", "NumCompaniesWorked", "PercentSalaryHike", "StockOptionLevel", "TotalWorkingYears", "TrainingTimesLastYear", "YearsAtCompany", 
# "YearsSinceLastPromotion", "YearsWithCurrManager", "Working hours", "Survery" are the numeric variables

# Attrition Vs Business Travel

ggplot(employee_data,aes(x=BusinessTravel,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Business Travel") + ylab("Number of Employees") + ggtitle("Attrition Vs Business Travel ")

# "Travel ready" has contributed 68 % of Attrition rate

# Attrition Vs Department

ggplot(employee_data,aes(x=Department,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Department") + ylab("Number of Employees") + ggtitle("Attrition Vs Department ")

# R& D -Department has more Attrition rate with 64%

# Attrition Vs Gender

ggplot(employee_data,aes(x=Gender,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Gender") + ylab("Number of Employees") + ggtitle("Attrition Vs Gender ")

# 62% who left the organization are male empoyees

# Attrition Vs Job Role

ggplot(employee_data,aes(x=JobRole,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Job Role") + ylab("Number of Employees") + ggtitle("Attrition Vs Job Role ")


# Attrition Vs Martial Status

ggplot(employee_data,aes(x=MaritalStatus,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Marital Status") + ylab("Number of Employees") + ggtitle("Marital Status ")

# 50% who left organization are "Single"

# Attrition Vs Age Group

ggplot(employee_data,aes(x=`Age Group`,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Age Group") + ylab("Number of Employees") + ggtitle("Age Group ")

# 50 % of employees who left organization are falls in the "Age Group" "26 to 35 Years"
# Attrition Vs Distance From Home

ggplot(employee_data,aes(x=`Distance From Home`,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Distance From Home") + ylab("Number of Employees") + ggtitle("Distance From Home ")

# Nearly 70 % of employees who left company are below 10 kms from office

# Attrition Vs Salary Group

ggplot(employee_data,aes(x=`Salary Group`,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Salary Group") + ylab("Number of Employees") + ggtitle("Salary Group ")

# 50 % of attrition is noted in the salary group " 50,000 to 1,00,000"

# Attrition Vs Experience

ggplot(employee_data,aes(x=Experience,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Experiencee") + ylab("Number of Employees") + ggtitle("Overall Experience ")

# 75 % of employees who left organizatioon are with experience less than 10 years
# Attrition Vs Years at Company

ggplot(employee_data,aes(x=`Years at Company`,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Years at Company") + ylab("Number of Employees") + ggtitle("Years at Company ")

# More attrition 1.e, 68% was foung in the employees who works below 5 years for the company

# Attrition Vs Years Since Promotion

ggplot(employee_data,aes(x=`Since Promotion`,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Years Since Promotion") + ylab("Number of Employees") + ggtitle("Years Since Promotion ")

# Employees who are not promoted within 5 years have attrition rate with 85%

# Attrition Vs Years with Current Maanger

ggplot(employee_data,aes(x=`Years with current manager`,fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Years With Current Manager") + ylab("Number of Employees") + ggtitle("Years with Current Manager ")

# Employees who are with same manager with in 5 years have more attrion rate with 76 %

# Attrition Vs Years with "Env Satisfaction"

ggplot(employee_data,aes(x=factor(EnvironmentSatisfaction),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Environment Satisfaction") + ylab("Number of Employees") + ggtitle("Survey- Environment Satisfaction ")

# Attrition Vs Years with "Job Satisfaction"

ggplot(employee_data,aes(x=factor(JobSatisfaction),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Job Satisfaction") + ylab("Number of Employees") + ggtitle("Survey- Job Satisfaction ")

# Attrition Vs Years with "Work life Balance"

ggplot(employee_data,aes(x=factor(WorkLifeBalance),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Work Life Balance") + ylab("Number of Employees") + ggtitle("Survey- Work life Balance ")

# Attrition Vs Years with "Job involvement"

ggplot(employee_data,aes(x=factor(JobInvolvement),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Job involvement") + ylab("Number of Employees") + ggtitle("Survey- Job Involvement ")

# Attrition Vs Years with "Performance Rating"

ggplot(employee_data,aes(x=factor(PerformanceRating),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Performance Rating") + ylab("Number of Employees") + ggtitle("Survey- Performance Rating ")


# Attrition Vs Years with "Education"

ggplot(employee_data,aes(x=factor(Education),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Education") + ylab("Number of Employees") + ggtitle("Education ")

# Attrition Vs Years with "Job Level"

ggplot(employee_data,aes(x=factor(JobLevel),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Job Level") + ylab("Number of Employees") + ggtitle("Job Level ")

# Attrition Vs Years with "Number of Companies Worked"

ggplot(employee_data,aes(x=factor(NumCompaniesWorked),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Number of Companies Worked") + ylab("Number of Employees") + ggtitle("Number of Companies Worked ")

# Attrition Vs Years with "Stock option level"

ggplot(employee_data,aes(x=factor(StockOptionLevel),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Stock option level") + ylab("Number of Employees") + ggtitle("Stock option level ")

# Attrition Vs Years with "Average hours per day"

ggplot(employee_data,aes(x=factor(`Avg hours per day`),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Average hours per day") + ylab("Number of Employees") + ggtitle("Average Hours Per Day ")


# Attrition Vs Years with "Non Working Days"

ggplot(employee_data,aes(x=factor(`Non Working Days`),fill=Attrition))+geom_bar()+facet_wrap(~Attrition) +geom_text(stat='count',aes(label=..count..),vjust=-1)+ xlab("Non Working Days") + ylab("Number of Employees") + ggtitle("Non Working Days ")


# Removing all " un wanted variables from data frame formodeling
# Removing "Over 18", "Emploee count", "Standard hours"
# also removing all the "Grouping variables" which is created for Data Visualization

employee_data<- employee_data[,- c(14,21,23,27,28,29,30,32:39)]
str(employee_data)

colnames(employee_data)


# Converting "Attrition" to 1 and 0

employee_data$Attrition<- ifelse(employee_data$Attrition=="Yes",1,0)
table(employee_data$Attrition)
sum(is.na(employee_data))# 111 NAs found , can be deleted
employee_data<- na.omit(employee_data)

str(employee_data)

# Creating Dummy Variables for all the catergorical variables
# Dividing categorical variables in seperate dat frame and convert to factors

employee_data_chr<- employee_data[,c(9,10,13,15,16)]
employee_data_num<- employee_data[, -c(9,10,13,15,16)]
str(employee_data_chr)
str(employee_data_num)
employee_data_fact<- data.frame(sapply(employee_data_chr, function(x) factor(x)))
str(employee_data_fact)

# Creating dummy for all caregorical variables

dummies_fact<- data.frame(sapply(employee_data_fact , 
                            function(x) data.frame(model.matrix(~x-1,data =employee_data_fact))[,-1]))

# Final dataset
employee_data_final<- cbind(employee_data_num,dummies_fact) 
View(employee_data_final)   


#######################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(employee_data_final$Attrition, SplitRatio = 0.7)


train = employee_data_final[indices,]

test = employee_data_final[!(indices),]

###########################################################################


# Logistic Regression: 


#Initial model

model_1 = glm(Attrition ~ ., data = train, family = "binomial")

summary(model_1) 


# Stepwise selection

model_2<- stepAIC(model_1, direction="both")

summary(model_2)

#####################################################################


# Removing multicollinearity through VIF check

vif(model_2)

model_3<- glm(formula = Attrition ~  EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + PerformanceRating + Age + JobLevel + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                nonworkingdays + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle,family = "binomial", data = train)
summary(model_3)

vif(model_3)

# Job level is having high VIF value, and high p- value


model_4<- glm(formula = Attrition ~ + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + PerformanceRating + Age  + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                nonworkingdays + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xMarried + MaritalStatus.xSingle,family = "binomial", data = train)
summary(model_4)
vif(model_4)

# status- "married" is having high VIF value, and high p- value


model_5<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + PerformanceRating + Age  + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                nonworkingdays + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
                MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_5)
vif(model_5)

# "JobRole.xManufacturing.Director" is having high VIF value, and high p- value

model_6<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + PerformanceRating + Age  + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                nonworkingdays + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + 
                MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_6)
vif(model_6)

model_7<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + PerformanceRating + Age  + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                nonworkingdays + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_7)
vif(model_7)



model_8<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + PerformanceRating + Age  + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_8)
vif(model_8)


model_9<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance  + Age  + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_10)
vif(model_9)

model_10<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance  + Age  + NumCompaniesWorked + 
                TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                BusinessTravel.xTravel_Frequently +  
                EducationField.xLife.Sciences + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_10)
vif(model_10)


model_11<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance  + Age  + NumCompaniesWorked + 
                 TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                 BusinessTravel.xTravel_Frequently +  
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 JobRole.xResearch.Director + 
                 MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_11)
vif(model_11)



model_12<- glm(formula = Attrition ~ EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance  + Age  + NumCompaniesWorked + 
                 TotalWorkingYears + YearsAtCompany + YearsSinceLastPromotion + 
                 BusinessTravel.xTravel_Frequently +  
                 EducationField.xLife.Sciences + EducationField.xMarketing + 
                 EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                 MaritalStatus.xSingle,family = "binomial", data = train)

summary(model_12)
vif(model_12)

# model_12 is finaised model

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of attrition for test data

test_pred = predict(model_10, type = "response", 
                    newdata = test[,-1])
summary(test_pred)

test$prob <- test_pred
View(test)

# Probabability cutoff of 50%.

employee_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
employee_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(employee_actual_attrition , employee_pred_attrition )

# Probabability cutoff of 40%.

employee_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))

employee_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

test_conf <- confusionMatrix(employee_pred_attrition, employee_actual_attrition, positive = "Yes")
test_conf


#Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_att <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_att, employee_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
  
  
  plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(s,OUT[,2],col="darkgreen",lwd=2)
  lines(s,OUT[,3],col=4,lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
  
  
  cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
  cutoff
  
  # 18 % of probability is optimal cut off
  
  # Probabability cutoff of 18%.
  
  employee_pred_attrition <- factor(ifelse(test_pred >= 0.18, "Yes", "No"))
  
  employee_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
  
  test_conf <- confusionMatrix(employee_pred_attrition, employee_actual_attrition, positive = "Yes")
  test_conf
  
  