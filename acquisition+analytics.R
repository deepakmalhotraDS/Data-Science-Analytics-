########## Business Understanding and problem statement ############
#to build another model without the variable 'duration'. That will help you understand the relationship of other variables with the response?
#to achieve 80% of total responders at the minimum possible cost? i.e increase responders and reduce cost.
#To find the number of prospects you should target (i.e. how many top deciles you should target)?
# how many prospects should be called to meet the business objective?
#####################################################################


##### Data understanding and prep ########

# Loading Libraries
library(ggplot2)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(e1071)
require(dplyr)
library(dplyr)

# Loading bank marketing data in the working directory. 

bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 

str(bank_data)
#41188 observations and 21 variables
# Summary of dataset

summary(bank_data)

# Checking response rate of prospect customer

bank_data <- unique(bank_data)  ##41176 observation left

response <- 4639/(36537+4639)
response 
## 11.2%

# Checking missing values

sum(is.na(bank_data))

##### Check outliers #######

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 

quantile(bank_data$age,seq(0,1,0.01))

# Box plot 

boxplot(bank_data$age)

# Capping the upper values of age with 71.

bank_data[(which(bank_data$age>71)),]$age <- 71


# Binning the age variable and store it into "binning.age".

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# changing column name of each variables in agg_age dataframe

colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values

agg_age$response_rate <- format(round(agg_age$response_rate, 2))

agg_age

# Let's see the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

## The response rate for 16-20 and above 60 is > 41% which is very high and crucial #######


# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

#-------------------------------------------------------

# Now, Let's see the "duration" variable: Which is Quantitative variable

# Let's check the histogram 

ggplot(bank_data,aes(duration))+geom_histogram()

# Let's see the summary of this variable once 

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)

bank_data <- bank_data[,-22]

## Definitely the outlier is present in the dataset

# So let's check the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))  ## 99percentile is 1271.25

# So, capping the duration seconds at 99% which is 1271.25sec 

bank_data[(which(bank_data$duration>1271.25)),]$duration <- 1271.25

#Lets plot
ggplot(bank_data,aes(duration))+geom_histogram()

#-------------------------------------------------------
# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 

summary(bank_data$campaign)

# Let's see the percentile distribution of this variable

boxplot(bank_data$campaign)


quantile(bank_data$campaign,seq(0,1,0.01))

# Capping this at 99% which the value is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Visualizing it with plot

ggplot(bank_data,aes(campaign))+geom_histogram()

##--------------------------------------------------------  

#-----Next Variable is "job"

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")
## Students and retired have high response level as confirmed from age level also that 16-20 and above 55 have highest response levels.


#  Next variable is Contact, Let's see the response rate of each mode 

summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")
#Telephone and Cellular
##--------------------------------------------------------  

# Checking Marital status

summary(bank_data$marital)

# Let's replace Unknown level to married

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")

# Let's see the education variables

plot_response(bank_data$education,"Education")
##Not uselful in deducing anything
#-------------------------------------------------

# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")
##Not helpful either

#-------------------------------------------------------
# Let's see the default variable

table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#-------------------------------------------------------

# Let's understand the housing variables 

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

#-------------------------------------------------------

#-- Let's see the next variable which is "loan"

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")
#-------------------------------------------------------

#Back to campaign data
# Next variable is "Month" i.e contact month. 

plot_response(bank_data$month,"Contact_month")

#-------------------------------------------------------

# Let's do the same of "day_of_week" variable

plot_response(bank_data$day_of_week,"day_of_week")

#-------------------------------------------------------
#-- Next variable is "pdays"
# Let's first convert this variable to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the respose rate of each levels. 

plot_response(bank_data$pday,"Pday")

# Number of prospects under each category

table(bank_data$pdays)

#-------------------------------------------------------

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)

summary(bank_data$previous)
# Max=7, best is to convert this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"


summary(bank_data$previous)


plot_response(bank_data$previous,"Previous_contacts")


# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#-------------------------------------------------------

#### Model Building and Evaluation #####

## Loading the data without variable duration as required ##

final_data <- subset (bank_data, select = -duration)
final_data <- final_data[, -20]
str(final_data)

#creating dummy variables

final_data$response <- as.integer(final_data$response)

k1 <- final_data

final_data <- dummy.data.frame(final_data)

final_data$response <- as.factor(ifelse(final_data$response == 1, "yes", "no"))

# splitting into train and test data

set.seed(123)

split_indices <- sample.split(final_data$response, SplitRatio = 0.70) 
#Splitting 70:30 for tran and test data

train <- final_data[split_indices, ]

test <- final_data[!split_indices, ]

nrow(train)/nrow(final_data)

nrow(test)/nrow(final_data)

## Model building using logistic regression ##

logistic_1 <- glm(response ~ ., family = "binomial", data = train)
summary(logistic_1)

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")
summary(logistic_2)
vif(logistic_2)

## Removing "previousLess_than_3_times" as it is insignificant
logistic_3 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + maritaldivorced + 
                    maritalmarried + educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + cons.price.idx + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_3)
vif(logistic_3)

## Removing "emp.var.rate" as it is insignificant and has high VIF
logistic_4 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + maritaldivorced + 
                    maritalmarried + educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.price.idx + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_4)
vif(logistic_4)

## Removing "cons.price.idx" as it is insignificant and has high VIF
logistic_5 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + maritaldivorced + 
                    maritalmarried + educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthapr + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_5)
vif(logistic_5)

## Removing "monthapr" as it is insignificant and has high VIF
logistic_6 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + maritaldivorced + 
                    maritalmarried + educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_6)
vif(logistic_6)

## Removing "monthoct" as it is insignificant and has high VIF
logistic_7 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + maritaldivorced + 
                    maritalmarried + educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_7)
vif(logistic_7)

## Removing "maritalmarried" as it is insignificant and has high VIF
logistic_8 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + maritaldivorced + 
                    educationPrimary_Education + educationSecondary_Education + 
                    contactcellular + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_8)
vif(logistic_8)

## Removing "educationSecondary_Education" as it is insignificant and has high VIF
logistic_9 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + maritaldivorced + 
                    educationPrimary_Education + contactcellular + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_9)
vif(logistic_9)

## Removing "maritaldivorced" as it is insignificant and has high VIF
logistic_10 <- glm(response ~ `jobblue-collar` + jobretired + jobservices + 
                    educationPrimary_Education + contactcellular + monthjul + monthjun + monthmar + 
                    monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + cons.conf.idx + nr.employed, 
                  family = "binomial", data = train)
summary(logistic_10)
vif(logistic_10)

## Removing "`jobblue-collar`" as it is insignificant and has high VIF
logistic_11 <- glm(response ~ jobretired + jobservices + 
                     educationPrimary_Education + contactcellular + monthjul + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + cons.conf.idx + nr.employed, 
                   family = "binomial", data = train)
summary(logistic_11)
vif(logistic_11)

## Removing "day_of_weekfri" as it is insignificant and has high VIF
logistic_12 <- glm(response ~ jobretired + jobservices + 
                     educationPrimary_Education + contactcellular + monthjul + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + cons.conf.idx + nr.employed, 
                   family = "binomial", data = train)
summary(logistic_12)
vif(logistic_12)

## Removing "jobservices" as it is insignificant and has high VIF
logistic_13 <- glm(response ~ jobretired + educationPrimary_Education + contactcellular + monthjul + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + cons.conf.idx + nr.employed, 
                   family = "binomial", data = train)
summary(logistic_13)
vif(logistic_13)

## Removing "monthjul" as it is insignificant and has high VIF
logistic_14 <- glm(response ~ jobretired + educationPrimary_Education + contactcellular + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + cons.conf.idx + nr.employed, 
                   family = "binomial", data = train)
summary(logistic_14)
vif(logistic_14)

## Removing "cons.conf.idx" as it is insignificant and has high VIF
logistic_15 <- glm(response ~ jobretired + educationPrimary_Education + contactcellular + monthjun + monthmar + 
                     monthmay + monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + nr.employed, 
                   family = "binomial", data = train)
summary(logistic_15)
vif(logistic_15)

## Removing "monthjun" as it is insignificant and has high VIF
logistic_16 <- glm(response ~ jobretired + educationPrimary_Education + contactcellular + monthmar + 
                     monthmay + monthnov + day_of_weekmon + campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + nr.employed, 
                   family = "binomial", data = train)
summary(logistic_16)
vif(logistic_16)

## All the remaining values seems to be significant hence selecting model 16 as the final model

logistic_final <- logistic_16

###Model Evaluation###

#---------------------------------------------------------    

# Predicting probabilities of responding for the test data

predictions_logit <- predict(logistic_final, newdata = test[, -61], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 
## Model Evaluation: Logistic Regression

# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")

conf

## Accuracy : 90%, Sensitivity 23%, Specificity 98% ( Sensitivity is very low)

#---------------------------------------------------------    

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#---------------------------------------------------------    

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
s[8]

# Let's choose a cutoff value of 7.9% for final model

predicted_response <- factor(ifelse(predictions_logit >= 0.079, "yes", "no"))

conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec
###############################################################################################
### Based on the above query results, choosing above cutoff the Accuracy : 76%, Sensitivity 71%, Specificity 77% are achieved ####
################################################################################################


###Creating a data frame with the variables prospect ID, actual response, predicted response, predicted probability of response, duration of call in seconds, and cost of call

colnames(final_data)

# Lets find the probability of response for each prospect
final_data$prob <- predict(logistic_final, newdata = final_data[,-61], type = "response")
summary(final_data$prob)
####################################################
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 0.01167 0.03988 0.06302 0.11255 0.09928 0.90973
####################################################

additional_df <- dplyr::select(final_data, add_Response = response, Prob = prob)
additional_df$pred_response <- factor(ifelse(additional_df$Prob >= 0.079, "yes", "no")) #Chosen cutoff
additional_df$call_duration <- bank_data$duration #Adding Duration of Call
call_cost <- 0.033*(additional_df$call_duration) + 0.8
additional_df <- cbind(call_cost, additional_df)
prospect_ID = seq(1, nrow(additional_df), by = 1)
additional_df <- cbind(prospect_ID, additional_df)

###  Find the number of top X% prospects you should target to meet the business objective ###
summary(additional_df$add_Response)
summary(additional_df$pred_response)
#top_x <- head(arrange(additional_df, desc(Prob)))
#average_duration <- mean(top_x$call_duration)
#average_duration

##############
lift <- function(labels , predicted_prob, groups=10, duration, cost_per_call) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob, duration, cost_per_call))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE),
                                     avg_duration=mean(duration),
                                     decile_duration=sum(duration),
                                     avg_costpercall=mean(cost_per_call),
                                     per_decile_cost=sum(cost_per_call))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 
Response_pred <- as.factor(ifelse(additional_df$pred_response=="yes",1,0))
LG = lift(Response_pred, additional_df$Prob, groups = 10, additional_df$call_duration, additional_df$call_cost)
View(LG)

## Per business, need to find 80% of responders and target them to achieve business goals and cost targets
summary(additional_df$add_Response) ## 4639 responders
0.8 * length(which(additional_df$add_Response == "yes")) ## 3711 constitutes 80% responders

# Gain and Lift Chart 
plot(LG$bucket,LG$Gain,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "% of positive Response")
plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Gain Chart",xlab="% of total targeted",ylab = "Lift")
View(LG)

#############################################################
#### Per the above table, 3rd decile shows approx 3800 responders hence the following can be ascertained:
#### Average call duration : 255.13 seconds
#### Average Cost per call : 9.21 Rs
#### Per lift chart, by Selecting only 10% of the responders through the model we get a lift of 3.4 that means we can cover that many times more responders than a random model
#### Hence we conclude that by targeting top 30% (3rd decile) of total populations we can find the 80% of responders to keep the optimal cost for the organiation
##############################################################


