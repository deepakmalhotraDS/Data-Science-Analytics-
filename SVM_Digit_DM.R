############################ SVM Digit Recogniser #################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation
# 4. Model Building 
#  4.1 Linear kernel
#  4.2 RBF Kernel
# 5 Hyperparameter tuning and cross validation

#####################################################################################

######################### Business & Data Understanding #############################
#The MNIST database contains images of handwritten digits with different writing styles. 
# Train Dataset= 60,000 images
# Test Dataset = 10,000 images
# The 784 columns apart from the first (digit label) consist of  28*28 matrix describing the scanned image of the digits
# The digits have been size-normalized and centered in a fixed-size image
# The Objective of this assignent is to develop a model using Support Vector Machine which should correctly classify the handwritten digits based on the pixel values given as features.

#####################################################################################

library(kernlab)
library(caret)
library(readr)
library(caTools)
library(dplyr)
library(stats)
library(ggplot2)

#################### Data Understanding #################

train_data <- read.csv("mnist_train.csv", header = FALSE)
test_data <- read.csv("mnist_test.csv", header = FALSE)

## View Data
View(train_data)
View(test_data)

## Assigning first colum names
colnames(train_data)[1]<- "Digit"
colnames(test_data)[1]<- "Digit"

## Checking data set
dim(train_data)
dim(test_data)

## Checking for headers
head(test_data)
head(train_data)

## All dependent variables are integer type
str(train_data)
str(test_data)

## No NA values
max(sapply(train_data, function(x) sum(is.na(x))))
max(sapply(test_data, function(x) sum(is.na(x))))


################# Data Preperation #########################

##Since the data is huge selecting only ~20% of the train data set

set.seed(100)
sample_train <- sample(1: nrow(train_data), 10500) 
train_final <- train_data[sample_train, ]
##sample_train <- sample.int(n = nrow(train_data), size = floor(.15*nrow(train_data)), replace = F)
##train_final <- train_data[sample_train, ]

test_final <- test_data[]

### Convert digit colum to factor
train_final$Digit<- factor(train_final$Digit)
test_final$Digit<- factor(test_final$Digit)


################## Model Building ##########################

#Using Linear Kernel
Model_linear <- ksvm(Digit~ ., data = train_final, scale = FALSE, kernel = "vanilladot")
print(Model_linear)
Eval_linear<- predict(Model_linear, test_final)


#confusion matrix - Linear Kernel
Linear_CM <- confusionMatrix(Eval_linear,test_final$Digit)
Linear_CM
####Overall Statistics - Linear
## Accuracy :    91% ,   95% CI : (0.9128, 0.9236)
## Sensitivity : 86%
## Specificity : 99%

#Using RBF Kernel
Model_RBF <- ksvm(Digit~ ., data = train_final, scale = FALSE, kernel = "rbfdot")
print(Model_RBF)
Eval_RBF<- predict(Model_RBF, test_final)
### c=1, Sigma= 1.627e-07

#confusion matrix - RBF Kernel
RBF_CM <- confusionMatrix(Eval_RBF,test_final$Digit)
RBF_CM
###Overall Statistics - RBF
## Accuracy :    95% ,   95% CI : (0.955, 0.9629)
## Sensitivity : 93%
## Specificity : 99%

############### Hyper parameters tuning and cross validation control #################

##Created 3 fold after going through multiple iterations of 5 fold since it was taking more time and with not much change in yield#####
trainControl <- trainControl(method="cv", number=3, verboseIter=TRUE)
metric <- "Accuracy"

##Selected near value range for sigma results per eval RBF##
set.seed(7)
hypr_pm_grid <- expand.grid(.sigma = c(1.627e-07,3.627e-07,5.627e-07),.C=c( 1, 2, 3))

## Evaluating 3-Fold paramter based on accuracy metric
## Ignore the warning message
digit_fit_svm <- train(Digit~., data=train_final, method="svmRadial", metric=metric, 
                 tuneGrid=hypr_pm_grid, trControl=trainControl)

print(digit_fit_svm)
plot(digit_fit_svm)
##Support Vector Machines with Radial Basis Function Kernel 
##
##10500 samples
##  784 predictor
##   10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
##
##No pre-processing
##Resampling: Cross-Validated (3 fold) 
##Summary of sample sizes: 6999, 6999, 7002 
##Resampling results across tuning parameters:
##
##  sigma      C  Accuracy   Kappa    
##  1.627e-07  1  0.9531435  0.9479281
##  1.627e-07  2  0.9588575  0.9542782
##  1.627e-07  3  0.9614291  0.9571362
##  3.627e-07  1  0.9644773  0.9605237
##  3.627e-07  2  0.9682865  0.9647568
##  3.627e-07  3  0.9683816  0.9648623
##  5.627e-07  1  0.9659059  0.9621113
##  5.627e-07  2  0.9665726  0.9628523
##  5.627e-07  3  0.9668584  0.9631699
##
##Accuracy was used to select the optimal model using the largest value.
##The final values used for the model were sigma = 3.627e-07 and C = 3.

############# Final Model Selected ##################

Final_Model_RBF <- ksvm(Digit~ ., data = train_final, scale = FALSE, kernel = "rbfdot", C= 3, kpar=list(sigma=3.627e-07))
Final_Model_RBF

###### Validating the accuracy of tuned model against train / test data ########

Final_RBF<- predict(Final_Model_RBF, train_final)
Final_RBF_CM<- confusionMatrix(Final_RBF, train_final$Digit)
Final_RBF_CM
###Overall Tuned Stats prediction with train data set
## Accuracy :    99.9% ,   
## Sensitivity : 99%
## Specificity : 99%

Final_RBF_test<- predict(Final_Model_RBF, test_final)
Final_RBF_CM_test<- confusionMatrix(Final_RBF_test, test_final$Digit)
Final_RBF_CM_test
###Overall Tuned Stats prediction with test data set
## Accuracy :    97% ,   
## Sensitivity : 95%
## Specificity : 99%

##### End #####

