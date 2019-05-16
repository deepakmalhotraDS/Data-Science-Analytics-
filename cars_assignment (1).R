#############################################################
# Linear Regression Assignment: Car Price Prediction Model
#############################################################
library(tidyr)
library(dplyr)
library(MASS) # stepAIC
library(car) # VIF 

###Import and Analyze CSV file
cars <- read.csv('CarPrice_Assignment.csv')
str(cars)
summary(cars)

###Data Cleaning
colnames(cars) <- tolower(colnames(cars))

# check for duplicated rows
cars[which(duplicated(cars)), ]

# check for missing values; none
sum(is.na(cars))

# segregate carname into company and model & exclude model from analysis
cars <- separate(cars, carname, c('company', 'model'), sep = '[[:blank:]]', extra = 'merge', fill = 'right')
cars <- cars[, -c(4)] 

# misspelt company names
table(cars$company)

###Rectify misspelt car names
change_company <- function(name){
  changed_name <- name
  if(name=='maxda'){
    changed_name = 'mazda'
  } else if (name=='Nissan') {
    changed_name = 'nissan'
  } else if (name == 'porcshce') {
    changed_name = 'porsche'
  } else if (name == 'toyouta') {
    changed_name = 'toyota'
  } else if (name %in% c('vokswagen', 'vw')){
    changed_name = 'volkswagen'
  } else if (name=='alfa-romero'){
    changed_name = 'alfa-romeo'
  }
  return(changed_name)
}
cars$company <- sapply(cars$company, change_company)
table(cars$company) 

#############################################
## outlier treatment  for numeric types ###
#############################################
# helper function to fix outliers 
fix_outliers <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
}
# fix outliers from numeric data fields of cars dataset
cars$wheelbase <- fix_outliers(cars$wheelbase)
cars$carlength <- fix_outliers(cars$carlength)
cars$carwidth <- fix_outliers(cars$carwidth)
cars$enginesize <- fix_outliers(cars$enginesize)
cars$stroke <- fix_outliers(cars$stroke)
cars$compressionratio <- fix_outliers(cars$compressionratio)
cars$horsepower <- fix_outliers(cars$horsepower)
cars$peakrpm <- fix_outliers(cars$peakrpm)
cars$citympg <- fix_outliers(cars$citympg)
cars$highwaympg <- fix_outliers(cars$highwaympg) 

## Tidyup number of doors
levels(cars$doornumber) <- c(4, 2)
cars$doornumber <- as.numeric(levels(cars$doornumber))[cars$doornumber]

## Clean and tidy number of cylinders
levels(cars$cylindernumber) <- c(8, 5, 4, 6, 3, 12, 2)
cars$cylindernumber <- as.numeric(levels(cars$cylindernumber))[cars$cylindernumber]

#################################
## derived metrics  ###
#################################
cars$company <- as.factor(cars$company)

# Categorisation based on company's origin

european <- c('alfa-romeo','audi','bmw','jaguar','peugeot','porsche','renault','saab','volkswagen','volvo')
asian <- c('honda','isuzu','mazda','mitsubishi','nissan','subaru','toyota')
us <- c('buick','chevrolet','dodge','mercury','plymouth')

# # exclude 
cars$origin <- ifelse(cars$company %in% european, 'european', 
                      ifelse(cars$company %in% us, 'us','asian'))
cars$origin <- as.factor(cars$origin)
table(cars$origin)

#################################
## dummy variable creation  ###
#################################
summary(cars[which(sapply(cars, class)=='factor')])
# we have 8 factors types for which we need dummy variables
### 2-lvl: fueltype, aspiration, enginelocation; ###
cols_2 <- c('fueltype', 'aspiration', 'enginelocation')
# helper function for encoding 2 level types giving weightage to popular level type
encode_cols <- function(x){ # popular level types: gas, turbo, front
  if(table(x)[1] > table(x)[2]){
    levels(x) <- c(1,0)  
  } else {
    levels(x) <- c(0,1)
  }
  x <- as.numeric(levels(x))[x]
}
cars[cols_2] <- sapply(cars[cols_2], encode_cols)
sapply(cars[cols_2],table)
# 3-lvl: company, drivewheel
cars1 <- cars
# factor columns
factor_cols <- which(sapply(cars1, is.factor))

## outlier treatment for factor variables
sapply(cars1[,factor_cols], function(x){round(table(x)/nrow(cars1)*100, 2)})

# we should combine obsearvations < 5% together
#cars1$company <- NULL # Since car origin is derived from car company 
fact_cols <- c('carbody', 'enginetype', 'fuelsystem') ## MISSED STEPS
cars1[,fact_cols] <- sapply(cars1[,fact_cols], as.character) ## MISSED STEPS
cars1$fuelsystem <- ifelse(cars1$fuelsystem %in% c('spdi','4bbl','mfi','spfi'),
                           'others', cars1$fuelsystem)
cars1$enginetype <- ifelse(cars1$enginetype %in% c('dohcv','rotor'), 
                           'others', cars1$enginetype)
cars1$carbody <- ifelse(cars1$carbody %in% c('convertible','hardtop'), 
                        'others', cars1$carbody)
cars1[,fact_cols] <- sapply(cars1[,fact_cols], as.factor) ## MISSED STEPS
#sapply(cars1[,factor_cols], function(x){round(table(x)/nrow(cars1)*100, 2)})
###########################################################################
dummy_drivewheel <- model.matrix(~drivewheel, data=cars1)
dummy_drivewheel <- dummy_drivewheel[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='drivewheel')],dummy_drivewheel)

dummy_origin <- model.matrix(~origin, data=cars1)
dummy_origin <- dummy_origin[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='origin')],dummy_origin)
# 5-lvl: carbody
dummy_carbody <- model.matrix(~carbody, data=cars1)
dummy_carbody <- dummy_carbody[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='carbody')],dummy_carbody)

# 7-lvl: fuelsystem, enginetype
# dummy variable creation
dummy_fuelsystem <- model.matrix(~fuelsystem, data=cars1)
dummy_fuelsystem <- dummy_fuelsystem[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='fuelsystem')],dummy_fuelsystem)
## ENGINE TYPE ##
dummy_enginetype <- model.matrix(~enginetype, data=cars1)
dummy_enginetype <- dummy_enginetype[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='enginetype')],dummy_enginetype)

# company
dummy_company <- model.matrix(~company, data=cars1)
dummy_company <- dummy_company[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='company')],dummy_company)

##########################
## Data Modeling ##
########################
set.seed(100)
# population size
pop_size <- nrow(cars1)
# sample size
ntrain <- sample(1:pop_size, 0.7 * pop_size)
# creating training and testing data sets
train_id <- cars1$car_id[ntrain]
test_id <- cars1$car_id[-ntrain]
cars1 <- cars1[,-which(colnames(cars1)=='car_id')]
train <- cars1[ntrain,]
test <- cars1[-ntrain,]

# model building: model1 - consisting of all variables
model <- lm(price ~ ., data = train) 
summary(model)
## Using StepAIC to select important features ##
step <- stepAIC(model, direction = 'both')
step
## creating second model based on stepAIC selection
model2 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
               carbodywagon + fuelsystem2bbl + fuelsystemmpfi + enginetypeohc + 
               enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
               companybuick + companyhonda + companyjaguar + companymercury + 
               companyporsche + companyvolvo, data = train) 
summary(model2)
## check for multicollinearity an alternative to correlation analysis
vif(model2)

# remove companymercury: it is not significant p-value  > 0.05
model3 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
               carbodywagon + fuelsystem2bbl + fuelsystemmpfi + enginetypeohc + 
               enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
               companybuick + companyhonda + companyjaguar + 
               companyporsche + companyvolvo, data = train) 
summary(model3)

# remove carbodywagon
model4 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
               fuelsystem2bbl + fuelsystemmpfi + enginetypeohc + 
               enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
               companybuick + companyhonda + companyjaguar + 
               companyporsche + companyvolvo, data = train)  
summary(model4)

# remove fuelsystem2bbl
model5 <-lm(formula = price ~ symboling + aspiration + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
              fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyhonda + companyjaguar + 
              companyporsche + companyvolvo, data = train) 
summary(model5)

# remove companyhonda
model6 <-lm(formula = price ~ symboling + aspiration + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
              fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) 
summary(model6)
vif(model6)

# remove aspiration
model7 <-lm(formula = price ~ symboling + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
              fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) 
summary(model7)
vif(model7)

# remove carbodyothers
model8 <-lm(formula = price ~ symboling + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) 
summary(model8)
vif(model8)

# remove enginetypeohcf
model9 <-lm(formula = price ~ symboling + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + fuelsystemmpfi + enginetypeohc + 
              enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train)
summary(model9)
vif(model9)

# remove enginetypeoh
model10 <-lm(formula = price ~ symboling + enginelocation + 
               curbweight + cylindernumber + drivewheelfwd + fuelsystemmpfi + 
               enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
               companyporsche + companyvolvo, data = train) 
summary(model10)
vif(model10)

# remove drivewheelfwd
model11 <-lm(formula = price ~ symboling + enginelocation + 
               curbweight + cylindernumber + fuelsystemmpfi + 
               enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
               companyporsche + companyvolvo, data = train) 
summary(model11)
vif(model11)

# remove fuelsystemmpfi
model12 <-lm(formula = price ~ symboling + enginelocation + curbweight + cylindernumber + 
               enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
               companyporsche + companyvolvo, data = train) 
summary(model12)
vif(model12)

# remove symboling:  
model13 <-lm(formula = price ~ enginelocation + curbweight + cylindernumber + 
               enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
               companyporsche + companyvolvo, data = train) 
summary(model13)
vif(model13)


################################################# 
## predicting the car price in test dataset  ##
#################################################
predicted_price <- predict(model13, test[,-which(colnames(test)=='price')])
test$test_price <- predicted_price

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

####################
# Driver factors are:
# 1. enginelocation
# 2. curbweight
# 3. cylindernumber
# 4. enginetypeothers: 'dohcv','rotor'
# 5. company audi *
# 6. company bmw *
# 7. company buick *
# 8. company jaguar *
# 9. company porsche *
# 10.company volvo *
####################