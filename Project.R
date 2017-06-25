## SALARY PREDICTION  

## First set the directory to where you have saved your train and test files 
## Loading libraries
install.packages("data.table")
install.packages("randomForest")
install.packages("rockchalk")
install.packages("xgboost")
install.packages("mlr")
library(mlr)
library(caret)
library(rockchalk)
library(xgboost)
library(data.table)
library(ggplot2)
library(plotly)



## Read the files into R 

train <- fread("train.csv", na.strings = c(""," ", "?","NA",NA))
test <- fread("test.csv", na.strings = c(""," ", "?","NA",NA))

## Look at the data 
## Generally test data comes with one less column, but we have the predictions right away, which will help us.

dim(train)
dim(test)
str(train)

## Lets look at response variable 

train$income_level <- as.factor(train$income_level)
str(train$income_level)

train$income_level<- ifelse(train$income_level == "-50000", 0, 1)
test$income_level<- ifelse(test$income_level == "-50000", 0, 1)

round(prop.table(table(train$income_level))*100)

## Set column classes 
# In this, we would set aside the categorical and numerical data type columns aside using setdiff
# and then subset the columns for two different datasets. 

#set column classes
 factcols <- c(2:5,7,8:16,20:29,31:38,40,41)
 numcols <- setdiff(1:40,factcols)
 
 factcols1 <- c(1,6,17,18,19,30,39)
 numcols1 <- setdiff(1:41, factcols1)
 
 train[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
 train[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

 test[,(factcols) := lapply(.SD, factor), .SDcols = factcols]
 test[,(numcols) := lapply(.SD, as.numeric), .SDcols = numcols]

 #subset categorical variables
  cat_train <- train[,factcols, with=FALSE]
  cat_test <- test[,factcols,with=FALSE]
 
 #subset numerical variables
  num_train <- train[,numcols,with=FALSE]
  num_test <- test[,numcols,with=FALSE]
  rm(train,test) 
#to save memory 
  
## DATA EXPLORATION 
  
## Numerical data analysis 
  
## Lets create a function rather than writing the same code each time. 
  num_graph <- function(a){
    ggplot(data = num_train, aes(x= a, y=..density..)) + geom_histogram(fill="blue",color="red",alpha = 0.5,bins =100) +
      geom_density()
      ggplotly()
  }
  
## Age Variable
  num_graph(num_train$age)
  
## Add target variable to the numerical train set 
  num_train[,income_level := cat_train$income_level]
  
  ggplot(data=num_train,aes(x = age, y=wage_per_hour)) +
    geom_point(aes(colour=income_level)) + 
    scale_x_continuous("Age", breaks = seq(0,90,15))

## As we see that people ages below 20 are having incomelevel as 0, so we can bin the age variable into 
## bins 

  num_train$group <- as.numeric(cut(num_train$age, 3))

## Wage per hour 
  
num_graph(log10(num_train$wage_per_hour))
num_graph(log10(num_train$capital_gains))
num_graph(num_train$dividend_from_Stocks)

# This way, all the numerical variables are passed on to this function and then data analysis is done. 

## Categorical variables 
# Again a function is written for easy access. 

cat_graph <- function(i){
    ggplot(cat_train,aes(x=i,fill=income_level))+geom_bar(position = "dodge",  color="black") + 
    scale_fill_brewer(palette = "Pastel1") + 
    theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10))
  }
  
## Education

cat_graph(cat_train$education)

round(prop.table(table(cat_train$education,cat_train$income_level))*100)

##cat_train$education <- gsub("grade","School",cat_train$education)
##cat_train$education <- ifelse("School" %in% cat_train$education, "Inexperienced",cat_train$education)

##  Reducing down the levels 

install.packages("rockchalk")
library(rockchalk)

# This package has (combineLevels) function which is used to combine levels which are of less than 5% frequency. 

cat_train$education <- combineLevels(cat_train$education,levs = c("Bachelors degree(BA AB BS)" ,"Masters degree(MA MS MEng MEd MSW MBA)"), newLabel = "B_or_M")
cat_train$education <- combineLevels(cat_train$education,levs= c("10th grade","11th grade","1st 2nd 3rd or 4th grade",
                                                                 "5th or 6th grade","9th grade","7th and 8th grade","Associates degree-academic program",
                                                                 "Associates degree-occup /vocational","12th grade no diploma",
                                                                 "Some college but no degree","Less than 1st grade"),newLabel = "Inexperienced")

cat_train$education <- as.factor(cat_train$education)
##levels(cat_train$education)
round(prop.table(table(cat_train$education,cat_train$income_level))*100)

cat_train$education <- combineLevels(cat_train$education,levs = c("Doctorate degree(PhD EdD)" ,"Prof school degree (MD DDS DVM LLB JD)"), newLabel = "Irrevelant")

## Education as per class of the worker and Sex

ggplot(cat_train, aes(x=education,fill = income_level)) + geom_bar() +
  xlab("Class of Workers") + ylab("Count") + facet_grid(.~class_of_worker + sex )+ 
  theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10)) 

## Industry Code 

str(cat_train$industry_code)

library(dplyr)
a <- cat_train %>% group_by(industry_code,income_level) %>% tally()

names(a)[3] <- "Code_Count"
head(a)

## Class of Worker 

str(cat_train$class_of_worker)

cat_graph(cat_train$class_of_worker)

cat_train$class_of_worker <- combineLevels(cat_train$class_of_worker,levs = c("Federal government" ,"Local government","State government"), newLabel = "Government")

levels(cat_train$class_of_worker)
round(prop.table(table(cat_train$class_of_worker,cat_train$income_level))*100)
table(cat_train$class_of_worker,cat_train$income_level)

## Class of worker as per their citizenship 

ggplot(cat_train, aes(x=class_of_worker,fill = income_level)) + geom_bar() +
  xlab("Class of Workers") + ylab("Count") + facet_grid(.~citizenship)+ 
  theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10)) 

## Class of worker as per their sex 


ggplot(cat_train, aes(x=class_of_worker,fill = income_level)) + geom_bar() +
  xlab("Class of Workers") + ylab("Count") + facet_wrap(~sex)+ 
  theme(axis.text.x =element_text(angle  = 60,hjust = 1,size=10)) 

## 
str(cat_train$marital_status)
levels(cat_train$d_household_family_stat)


 ggplot(cat_train, aes(x=d_household_family_stat ,fill = income_level)) + geom_bar() +
  xlab("Class of Workers") + ylab("Count") +
  theme(axis.text.x =element_text(angle  = 50,hjust = 1,size=10)) 

# Similarly, we can go on doing ggplot for all other categorical variables.
 
## DATA CLEANING
 
#set threshold as 0.7
a <-findCorrelation(cor(num_train), cutoff = 0.7)
 num_train <- num_train[,-a,with=FALSE] 
  num_test[,weeks_worked_in_year := NULL]
 
#The variable weeks_worked_in_year gets removed. 
   
#Now, let's check for missing values in categorical data. We'll use base sapply() 
#to find out percentage of missing values per column.
 
#check missing values per columns
 mtrain <- sapply(cat_train, function(x){sum(is.na(x))/length(x)})*100
 mtest <- sapply(cat_test, function(x){sum(is.na(x)/length(x))}*100)
  
# We find that some of the variables have ~50% missing values. High proportion of missing value
# can be attributed to difficulty in data collection. For now, we'll remove these category levels.
# A simple subset() function does the trick.
 
#select columns with missing value less than 5%
 cat_train <- subset(cat_train, select = mtrain < 5 )
 cat_test <- subset(cat_test, select = mtest < 5)
 
#For the rest of missing values, a nicer approach would be to label them as 'Unavailable'.
#Imputing missing values on large data sets can be painstaking. data.table's set() function makes this computation insanely fast.
 
#set NA as Unavailable - train data
#convert to characters
 cat_train <- cat_train[,names(cat_train) := lapply(.SD, as.character),.SDcols = names(cat_train)]
 for (i in seq_along(cat_train)) set(cat_train, i=which(is.na(cat_train[[i]])), j=i, value="Unavailable")
#convert back to factors
 cat_train <- cat_train[, names(cat_train) := lapply(.SD,factor), .SDcols = names(cat_train)]
 
#set NA as Unavailable - test data
  cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, as.character), .SDcols = names(cat_test)]
  for (i in seq_along(cat_test)) set(cat_test, i=which(is.na(cat_test[[i]])), j=i, value="Unavailable")
#convert back to factors
  cat_test <- cat_test[, (names(cat_test)) := lapply(.SD, factor), .SDcols = names(cat_test)]
  
## MACHINE LEARNING  
  
# Combine the train and test datasets 
  
  c_train <- cbind(num_train,cat_train)
  c_test <- cbind(num_test,cat_test)
  
# Remove unwanted files 
  rm(num_train,num_test,cat_train,cat_test)
  
# Create Task for this machine learning approach
  
  train.task <- makeClassifTask(data = c_train,target = "income_level")
  test.task <- makeClassifTask(data=c_test,target = "income_level")

# Remove Zero Variance features 

rf_train <- randomForest(income_level~., data = c_train, ntree = 100, importance =TRUE)
varImpPlot(rf_train)

# As we can see that Occupation code provides the model with maximum information. 
# Just for the sake of looking, we will look at the confusion matrix, which shows that this model has 94% accuracy with random forest. 

rf_train$confusion

# But, accuracy is not our main intention. 

## BALANCED CLASSIFICATION

# As we have seen that this is an imbalanced classification problem, we will try to balance it using SMOTE.
# For the purpose of comparing it with the regular sampling methods, we will also try Undersampling and Oversampling. 

#undersampling 
 train.under <- undersample(train.task,rate = 0.1) #keep only 10% of majority class
 table(getTaskTargets(train.under))

#oversampling
 train.over <- oversample(train.task,rate=15) #make minority class 15 times
 table(getTaskTargets(train.over))

#SMOTE
 train.smote <- smote(train.task,rate = 15,nn = 5)
 table(getTaskTargets(train.smote))
 
# Lets see which algorithms are available 
 
 listLearners("classif","twoclass")[c("class","package")]
 
# NAIVE BAYES  

  naive_learner <- makeLearner("classif.naiveBayes",predict.type = "response")
  naive_learner$par.vals <- list(laplace = 1)
 
 #10fold CV - stratified
  folds <- makeResampleDesc("CV",iters=10,stratify = TRUE)
 
 #cross validation function
  fun_cv <- function(a){
   crv_val <- resample(naive_learner,a,folds,measures = list(acc,tpr,tnr,fpr,fp,fn))
   crv_val$aggr
 }
 
  fun_cv (train.task) 
 
 # acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
 # 0.8777        0.8854134     0.7570270    0.2429730
 
  fun_cv(train.under) 
 # acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
 # 0.8377315      0.8326978     0.8451696     0.1548304
 
  fun_cv(train.over)
 # acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
 #   0.8361459     0.8145749     0.85586852    0.1443148
 
  fun_cv(train.smote)
 # acc.test.mean tpr.test.mean tnr.test.mean fpr.test.mean 
 #   0.9632135     0.98168955    0.94610638     0.5389362
 
# This package names cross validated results are test.mean. After comparing, we see that 
# train.smote gives the highest true positive rate and true negative rate. Hence, we learn that 
# SMOTE technique outperforms the other two sampling methods and the non-sampled one.
 
#Now, let's build our model SMOTE data and check our final prediction accuracy.
 
#train and predict
 nB_model <- train(naive_learner, train.smote)
 nB_predict <- predict(nB_model,test.task)
 
#evaluate
 nB_prediction <- nB_predict$data$response
 dCM <- confusionMatrix(d_test$income_level,nB_prediction)
 
# Accuracy : 0.8774
# Sensitivity : 0.9862
# Specificity : 0.3299
  
## XGBoost 
 
# Lets try out xgboost and observe that how ensemble methods help in achieving better models. 
 
set.seed(2002)

xgb_learner <- makeLearner("classif.xgboost",predict.type = "response")
   
xgb_learner$par.vals <- list(
    objective = "binary:logistic",
    eval_metric = "error",
    nrounds = 150,
    print.every.n = 50
  )
  
#define hyperparameters for tuning
   xg_ps <- makeParamSet( 
    makeIntegerParam("max_depth",lower=3,upper=10),
    makeNumericParam("lambda",lower=0.05,upper=0.5),
    makeNumericParam("eta", lower = 0.01, upper = 0.5),
    makeNumericParam("subsample", lower = 0.50, upper = 1),
    makeNumericParam("min_child_weight",lower=2,upper=10),
    makeNumericParam("colsample_bytree",lower = 0.50,upper = 0.80)
  )
  
#define search function
  rancontrol <- makeTuneControlRandom(maxit = 5L) #do 5 iterations
  
#5 fold cross validation
  set_cv <- makeResampleDesc("CV",iters = 5L,stratify = TRUE)
  
#tune parameters
  xgb_tune <- tuneParams(learner = xgb_learner, task = train.task, resampling = set_cv, measures = list(acc,tpr,tnr,fpr,fp,fn), par.set = xg_ps, control = rancontrol)
  
# Tune result:
# Op. pars: max_depth=3; lambda=0.221; eta=0.161; subsample=0.698; min_child_weight=7.67; colsample_bytree=0.642
# acc.test.mean=0.948,tpr.test.mean=0.989,tnr.test.mean=0.324,fpr.test.mean=0.676
  
#Now, we can use these parameter for modeling using xgb_tune$x which contains the best tuned parameters.
  
#set optimal parameters
  xgb_new <- setHyperPars(learner = xgb_learnerr, par.vals = xgb_tune$x)
  
#train model
  xgmodel <- train(xgb_new, train.task)
  
#test model
  predict.xg <- predict(xgmodel, test.task)
  
#make prediction
  xg_prediction <- predict.xg$data$response
  
#make confusion matrix
  xg_confused <- confusionMatrix(d_test$income_level,xg_prediction)
#Accuracy : 0.968
#Sensitivity : 0.9374
#Specificity : 0.6985
  
  
## As we can see that boosting method has helped in categorizing minority classes with almost 70%,
## which is way higher than our previous naive bayes learner giving us just 32%. 
  
  
## SUMMARY
  
# There are many ways to explore the data and manipulate it. I used mine and took a lot of help from various 
# websites but this project has opened my eyes.
#  1. WE have seen that when presented with a million data, first we should start exploring it irrespective
#     of the features they have. Exploratory Data Analysis is very important. 
#  2. After exploration, feature engineering can also be applied but we have not done that because sometimes
#     explaining a meaningful data makes sense rather than extracting features out of it just for the sake of 
#     accuracy. 
# 3. when presented with imbalanced classification problem, we have proved that Synthetic Minority Over-Sampling 
#    technique(SMOTE) gives us better sampling than the conventional ones.
# 4. Also, when ensemble methods are applied, like we did with XGBoost, we get maximum specificity of about 
#    70% of the prediction of the minority classes. 
  

  