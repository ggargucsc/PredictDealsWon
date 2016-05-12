```{r}
setwd('~/Desktop/case study/baseCRM/')

library('xlsx')
library('ggplot2')
library('randomForest')
library('glmnet')
library('kernlab')
library('ROCR')
library('dplyr')
library('mice')
library('plyr')
library('rpart')
library('rpart.plot')
```

**FUNCTIONS**

```{r}

#function to create plots 
plots <- function(y){
  allx <- setdiff(names(data), y)
  for (i in seq_along(allx)) {
    df <- as.data.frame(table(data[,y], data[,allx[i]]))
    names(df) <- c(y, allx[i], "Freq")
    plots <- ggplot(df, aes_string(y, allx[i])) +
      geom_tile(aes(fill = Freq), colour = "black") +
      scale_fill_gradient(low = "white", high = "steelblue")
    print(plots)
    #ggsave(plots,filename=paste("plot",y, allx[i],".png",sep=""))
  }
}


#function to get metrics like precision, recall, accuracy, auc
accuracyStats <- function(pred, orig){
  
  #get auc value
  pr <- prediction(pred, orig)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  
  pred <- as.factor(ifelse(pred > 0.5, 'TRUE', 'FALSE'))
  levels(pred) <- levels(orig)
  confMatrix <- table(truth=orig, predict= pred)
  FP <- confMatrix[1,2]
  FN <- confMatrix[2,1]
  TP <- confMatrix[2,2]
  TN <- confMatrix[1,1]
  
  precision <- (TP)/(TP+FP)
  
  recall <- (TP)/(TP+FN)
  accuracy <- sum(diag(confMatrix))/sum(confMatrix)
  
  data.frame(precision, recall, accuracy, auc)
  
  #cat(sprintf('%s: %2.3f\n, %s: %2.3f\n, %s: %2.3f\n',  "precision", precision , "recall", recall, "classification accuracy", accuracy))
}

#this function takes the predicted values, and original values, plot the roc curve and return auc value
roc <- function(pred,  orig){
  
  pr <- prediction(pred, orig)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")
  #plot(prf)
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  
  return (auc)  
}


fit_penalizedLogit <- function(train, vars, yColumn, test){
  xfactorsTrain <- model.matrix(paste(y, "~", paste(vars, collapse="+")), data=train)[,-1]
  xfactorsTest <- model.matrix(paste(y, "~", paste(vars, collapse="+")), data=test)[,-1]
  
  #ridge regression
  fit = glmnet(xfactorsTrain, train[[yColumn]], family = "binomial", alpha=0)
  plot(fit, xvar = "dev", label = TRUE)
  cv.glmmod <- cv.glmnet(xfactorsTrain, train[[yColumn]], family = "binomial", type.measure = "class")
  plot(cv.glmmod)
  best_lambda <- cv.glmmod$lambda.min
  
  #predTrain<- predict(fit, newx = xfactorsTrain, type = "class", s = best_lambda)
  predTest <- predict(fit, newx = xfactorsTest, type = "class", s = best_lambda)
  
}

fit_logit = function(train, vars, yColumn, test){
  formula = paste(yColumn, "~", paste(vars, collapse="+"))
  model= glm(as.formula(formula), data=train, family="binomial")
  
  return (predict(model, newdata=test, type='response'))
}


fit_rf = function(train, vars, yColumn,test ) {
  formula = paste(yColumn, "~", paste(vars, collapse="+"))
  model = randomForest(as.formula(formula), data=train, ntree=500)
  predict = predict(model,newdata=test,type='prob')[,'TRUE',drop=TRUE]
  
  return (predict)
}


fit_svm = function(train, vars, yColumn, test) {
  formula = paste(yColumn, "~", paste(vars, collapse="+"))
  model = ksvm(as.formula(formula),data=train,type='C-svc',C=1.0,
               kernel='rbfdot',
               prob.model=TRUE)
  predict = predict(model,newdata=test,type='prob')[,'TRUE',drop=TRUE]
  
  return (predict)
}
```

**READING DATA**

```{r}
#read data
data <- read.xlsx("Base Leads Exercise 2.xlsx", sheetName="Base Leads Exercise .csv", header=T)
str(data)


#Lead Qualified and Deal Won are logical so convert it to factor
levels(data$Channel) 
#Channel variable has 13 levels, in which 3 of the levels were "Walk On / Unknown", "Walk-On   / Unknown" and "Walk-On / Unknown". These 3 levels are converted to a single level by removing non-alphanumeric characters using gsub function
table(data$Channel) 
#each of the Channel levels ContentLeads and Outbound has just one record
#delete these two records or always keep them in training data 
data <- subset(data, !(Channel %in% c("Content Leads", "Outbound")))
data$Channel <- factor(data$Channel)
table(data$Lead.Qualified) #classes are imbalanced 
table(data$Deal.Won) #classes are imbalanced 


levels(data$Stated.business_size)
#Stated size levels "1 to 3" , "1 to 5",  "11 to 25", "4 to 10" and "6 to 25" 
#are converted to a single level "1 to 25". After this, Stated size will have 3 factor levels "1 to 25", "26 to 100" and "101+".

levels(data$Stated.industry_name)


#data is modified using mutate function
data <- data %>% mutate(Channel = factor(gsub(pattern="[^a-zA-Z0-9]", replacement="", Channel)), 
                        Lead.Qualified = factor(Lead.Qualified),
                        Deal.Won = factor(Deal.Won),
                        Stated.business_size = revalue(Stated.business_size, c("1 to 3"="1 to 25", 
                                                                               "1 to 5"= "1 to 25",
                                                                               "4 to 10" = "1 to 25", 
                                                                               "6 to 25" = "1 to 25", 
                                                                               "11 to 25"= "1 to 25")))


#Channel level "Organic" level was converted to "Organic Search". Now Channel has 10 factor levels.
data$Channel = revalue(data$Channel, c("Organic" = "OrganicSearch"))


#check for missing data
colSums(is.na(data))
md.pattern(data)
#57 rows have 2 missing values, 38 rows have 1 missing values. 
#Usually when there are missing values in more than one column for a record, 
#it's better to ignore that record if one have enough data. Now 38 records have 1 missing values, 
#which is just 1% of data. We can simply ignore it as it will just bias our analysis.

#remove NA values
data <- na.omit(data)

```

***EXPLORATORY DATA ANALYSIS***

```{r}
#use function plots to understand the relationship between different variables
plots(y="Lead.Qualified")
plots(y="Deal.Won")

df <- as.data.frame(table(data$Lead.Qualified, data$Deal.Won))
names(df) <- c("Lead.Qualified", "Deal.Won", "Freq")
```

**Table showing how many qualified leads gets converted to deals won**
```{r}
df
```
**Qualified Leads are dependent on variables Channel,email ,Stated.businesssize, and Stated.industryname** <br/>
**Deals Won are dependent on variables Leads qualified, Channel,email ,Stated.businesssize, and Stated.industryname** <br/>

```{r}
#find the most important variables affecting Qualified Leads and Lead won using random forests 
model_rf <- randomForest(Lead.Qualified ~ Channel+ email + Stated.business_size + Stated.industry_name, data=data, importance=TRUE, ntree=500)
varImpPlot(model_rf, main="Variable Importance Plot for Qualified Leads")
model_rf <- randomForest(Deal.Won ~ Lead.Qualified + Channel+ email + Stated.business_size + Stated.industry_name, data=data, importance=TRUE, ntree=500)
varImpPlot(model_rf, main="Variable Importance Plot for Leads Won")
```

**PREDICTION ON IMBALANCED DATA**<br/>
**Logistic Regression is used for predicting Qualified Leads and Leads Won using 5-fold cross validation** 

```{r}
nfolds <- 5
case.folds <- rep(1:nfolds,length.out=nrow(data))
# divide the cases as evenly as possible
case.folds <- sample(case.folds)

varsLeadQual <- c("Channel", "email", "Stated.business_size", "Stated.industry_name")
yLeadQual <- "Lead.Qualified"
varsDealsWon <- c("Lead.Qualified","Channel", "email", "Stated.business_size", "Stated.industry_name")
yDealsWon <- "Deal.Won"


accTrainQualLeads <- list()
accTestQualLeads <- list()
accTrainLeadsWon <- list()
accTestLeadsWon <- list()

for(fold in 1:nfolds)
{ 
  train <- data[case.folds!=fold,]
  test <- data[case.folds==fold, ]
  
  #using logistic regression model
  ############## QUALIFIED LEADS ############################
  #predict training data 
  predTrain_QualLeads <- fit_logit(train, vars = varsLeadQual, yColumn = yLeadQual, train)
  #training accuracy
  accTrainQualLeads[[fold]] <- accuracyStats(predTrain_QualLeads, train$Lead.Qualified)
  
  #predict test data
  predTest_QualLeads <- fit_logit(train, vars = varsLeadQual, yColumn = yLeadQual, test)
  #test accuracy metrics
  accTestQualLeads[[fold]] <- accuracyStats(predTest_QualLeads, test$Lead.Qualified)
  
  ############# LEADS WON ##################################
  #predict training data and accuracy stats
  predTrain_LeadsWon <- fit_logit(train, vars = varsDealsWon, yColumn = yDealsWon, train)
  accTrainLeadsWon[[fold]] <- accuracyStats(predTrain_LeadsWon, train$Deal.Won)
  
  #predict test data and accuracy stats
  predTest_LeadsWon <- fit_logit(train, vars = varsDealsWon, yColumn = yDealsWon, test)
  accTestLeadsWon[[fold]] <- accuracyStats(predTest_LeadsWon, test$Deal.Won)
}

Stats_QualLeadsTrain <- Reduce( function(x,y) mapply("+",x,y), accTrainQualLeads)/length(accTrainQualLeads)
Stats_QualLeadsTest <- Reduce( function(x,y) mapply("+",x,y), accTestQualLeads)/length(accTestQualLeads)
```
**Logistic Regression**<br/>
**Performance metrics for Qualified Leads for both train and test data on imbalanced dataset**
```{r}
rbind(Stats_QualLeadsTrain, Stats_QualLeadsTest)


Stats_LeadsWonTrain <- Reduce( function(x,y) mapply("+",x,y), accTrainLeadsWon)/length(accTrainLeadsWon)
Stats_LeadsWonTest <- Reduce( function(x,y) mapply("+",x,y), accTestLeadsWon)/length(accTestLeadsWon)
```
**Logistic Regression**<br/>
**Performance metrics for Leads Won for both train and test data on imbalanced dataset**
```{r}
rbind(Stats_LeadsWonTrain, Stats_LeadsWonTest)
```

**Precision and Recall have low values on both training and test data for both Qualified Leads and Leads Won.This is** **because of imbalanced class problem**<br/>


**PREDICTION ON BALANCED DATA**<br/>
**There are many methods to work on imbalanced class problem, one is to balance the dataset either by undersampling of** **majority class or oversampling of minority class. I have used undersampling below to deal with the problem.**
**Created a nearly balanced dataset(40% TRUE, 60% FALSE) by subsampling the majority class. Run the model around 10 times** **and averaged the results. This approach is similar to bootstrap averaging or model averaging**

```{r}
#create a test dataset with 120 TRUE records and 180 FALSE records (40% TRUE, 60% FALSE)
dataTrue <- data[data$Lead.Qualified=="TRUE",]
dataFalse <- data[data$Lead.Qualified=="FALSE",]

#sample the indices from data
test.ind.true <- sample(nrow(dataTrue), 120)
test.ind.false <- sample(nrow(dataFalse), 180)

#create the test data
test.data <- rbind(dataTrue[test.ind.true,], dataFalse[test.ind.false,])

#create training data by removing test data
dataFalse <- dataFalse[-test.ind.false, ]
numModels <- 15

predict_logitTest <- matrix(NA, nrow=nrow(test.data), ncol=numModels)
predict_svmTest <- matrix(NA, nrow=nrow(test.data), ncol=numModels)
#data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5, N=1000, seed = 1)$data
predict_logitTrain <- list()
predict_SVMTrain <- list()

statLogitTrain <- list()
statSVMTrain <- list()
for(model in 1:numModels){
  
  #create training data for majority class by bootstrap sampling
  train.data.false  <- dataFalse[sample(nrow(dataFalse), 642, replace=TRUE),]  
  #final training data containing both the classes
  train.data <- rbind(dataTrue[-test.ind.true,], train.data.false)
  
  #prediction on test data using logistic regression
  predict_logitTrain[[model]] <- fit_logit(train.data, vars = varsLeadQual, yColumn = yLeadQual, train.data)
  statLogitTrain[[model]] <- accuracyStats(predict_logitTrain[[model]], train.data$Lead.Qualified)
  
  predict_logitTest[,model] <- fit_logit(train.data, vars = varsLeadQual, yColumn = yLeadQual, test.data)
  
  #prediction on test data using SVM
  predict_SVMTrain[[model]] <- fit_svm(train.data, vars = varsLeadQual, yColumn = yLeadQual, train.data)
  statSVMTrain[[model]] <- accuracyStats(predict_SVMTrain[[model]], train.data$Lead.Qualified)
  
  predict_svmTest[,model] <- fit_svm(train.data, vars = varsLeadQual, yColumn = yLeadQual, test.data)
  
}

#stats on test data using different models
predTest <- list(rowMeans(predict_logitTest), 
                 rowMeans(predict_svmTest))
names(predTest) <- c("logistic",  "SVM")

statLogitTrain <- Reduce( function(x,y) mapply("+",x,y), statLogitTrain)/length(statLogitTrain)
statSVMTrain <- Reduce( function(x,y) mapply("+",x,y), statSVMTrain)/length(statSVMTrain)
```

**Performance Metrics for Qualified Leads **<br/>
**First table shows results on train data using logistic regression and SVM **<br/>
**Second table shows results on test data using logistic regression and SVM**<br/>
**There is not much improvement in the performance metrics using models on balanced data**
```{r}
rbind(statLogitTrain, statSVMTrain)
lapply(predTest, accuracyStats, test.data$Lead.Qualified)

```

**Follow the same process for Leads Won**<br/>
```{r}
#create a test dataset with 80 TRUE records and 120 FALSE records (40% TRUE, 60% FALSE)
dataTrue <- data[data$Deal.Won=="TRUE",]
dataFalse <- data[data$Deal.Won=="FALSE",]

#sample the indices from data
test.ind.true <- sample(nrow(dataTrue), 80)
test.ind.false <- sample(nrow(dataFalse), 120)

#create the test data
test.data <- rbind(dataTrue[test.ind.true,], dataFalse[test.ind.false,])

#create training data by removing test data
dataFalse <- dataFalse[-test.ind.false, ]
numModels <- 15

predict_logitTest <- matrix(NA, nrow=nrow(test.data), ncol=numModels)
predict_svmTest <- matrix(NA, nrow=nrow(test.data), ncol=numModels)
#data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5, N=1000, seed = 1)$data
predict_logitTrain <- list()
predict_SVMTrain <- list()

statLogitTrain <- list()
statSVMTrain <- list()
for(model in 1:numModels){
  
  #create training data for majority class by bootstrap sampling
  train.data.false  <- dataFalse[sample(nrow(dataFalse), 354, replace=TRUE),]  
  #final training data containing both the classes
  train.data <- rbind(dataTrue[-test.ind.true,], train.data.false)
  
  #prediction on test data using logistic regression
  predict_logitTrain[[model]] <- fit_logit(train.data, vars = varsDealsWon, yColumn = yDealsWon, train.data)
  statLogitTrain[[model]] <- accuracyStats(predict_logitTrain[[model]], train.data$Deal.Won)
  
  predict_logitTest[,model] <- fit_logit(train.data, vars = varsDealsWon, yColumn = yDealsWon, test.data)
  
  #prediction on test data using SVM
  predict_SVMTrain[[model]] <- fit_svm(train.data, vars = varsDealsWon, yColumn = yDealsWon, train.data)
  statSVMTrain[[model]] <- accuracyStats(predict_SVMTrain[[model]], train.data$Deal.Won)
  
  predict_svmTest[,model] <- fit_svm(train.data, vars = varsDealsWon, yColumn = yDealsWon, test.data)
  
}

#stats on test data using different models
predTest <- list(rowMeans(predict_logitTest), 
                 rowMeans(predict_svmTest))
names(predTest) <- c("logistic",  "SVM")
statLogitTrain <- Reduce( function(x,y) mapply("+",x,y), statLogitTrain)/length(statLogitTrain)
statSVMTrain <- Reduce( function(x,y) mapply("+",x,y), statSVMTrain)/length(statSVMTrain)


```

**Performance Metrics for Leads Won**<br/>

**First table shows results on train data using logistic regression and SVM **<br/>
**Second table shows results on test data using logistic regression and SVM**<br/>
**SVM Model is doing better than logistic. Precision and recall have improved using models on balanced dataset**
```{r}
rbind(statLogitTrain, statSVMTrain)
lapply(predTest, accuracyStats, test.data$Deal.Won)

################################################################################################
```

**Conclusion**

**Model on imbalanced dataset have a very low recall and precision values whereas models on balanced dataset have better** **results for Leads Won. There is not much difference in the results for Qualified leads using balanced dataset, though** **prediction accuracy is reduced a bit, and recall is slightly improved. Other methods like weighted logistic regression** **can also be tried by giving less weights to majority class.**




