# *****************Begin ***************
# *********Date : 20/02/2019************
# ***************Developers*************
#********* Pavan Kumar Sudhakar******* 
#********* Krishna Chander Rajendran******* 
#********* Swathi Raman******* 
#********* Varun Sundaram******* 

getwd()
setwd("G:\\ADM\\Project")
library(tidyr)
library(caret)
library(dummies)
library(countrycode)
library(randomForest)
library(DMwR)
library(dplyr)
install.packages("liquidSVM")
library(liquidSVM)
install.packages("utility")
library(utils)
data <- read.csv("Global_Landslide_Final_New.csv", stringsAsFactors = FALSE)
data[,c('formatted_date', 'event_date', 'event_day', 'event_title',
        'event_id', 'event_description', 'location_description',
        'location_accuracy', 'gazeteer_closest_point', 'country_code', 'gazeteer_distance')] <- NULL

# Initial data exploration and cleaning


names(data)[1] <- 'Landslide_Trigger'
data <- data %>% drop_na(Landslide_Trigger)

xMean<-mean(data$fatality_count, na.rm = TRUE)
xMean<-as.numeric(xMean)
data$fatality_count<-ifelse(is.na(data$fatality_count), xMean, data$fatality_count)
data$fatality_count<-round(data$fatality_count,digit=0)

xMean<-median(data$injury_count, na.rm = TRUE)
xMean<-as.numeric(xMean)
data$injury_count<-ifelse(is.na(data$injury_count), xMean, data$injury_count)
data$injury_count<-round(data$injury_count,digit=0)

xMean<-median(data$population, na.rm = TRUE)
xMean<-as.numeric(xMean)
data$population<-ifelse(is.na(data$population), xMean, data$population)
data$population<-round(data$population,digit=0)

data <- data[data$landslide_size != "catastrophic",]
data <- data[data$landslide_category != "unknown",]
data <- data[data$country_name != "other",]

data$event_time<-ifelse(data$event_time=='NaN', "unknown", data$event_time)
data$landslide_size<-ifelse(data$landslide_size=='very_large', "large", data$landslide_size)
data$event_time <- as.character(data$event_time)
data$landslide_category <- as.character(data$landslide_category)
data$country_name <- as.character(data$country_name)



# datadummy <- data
# datawithoutdummy <- data[,c(1,2,6,7,9,10,11)]
# dataencoded <- cbind(datawithoutdummy,datadummy)
# datadummy <- dummy.data.frame(datadummy[, c(3,4,5,8)])
names(data)[1] <- 'Landslide_Trigger'
data$Landslide_Trigger <- as.factor(data$Landslide_Trigger)
data$event_month <- as.factor(data$event_month)
data$event_time <- as.factor(data$event_time)
data$landslide_size <- as.factor(data$landslide_size)
data$landslide_category <- as.factor(data$landslide_category)
#data$country_name <- as.factor(data$country_name)


data$country_name <- ifelse(data$country_name == 'Himachal Pradesh', "India", data$country_name)
data$country_name <- ifelse(data$country_name == 'NaN', "Singapore", data$country_name)

countrynames <- (data$country_name)
df <- data.frame(country = countrynames)
data$continent <- countrycode(sourcevar = df[, "country"],
                              origin = "country.name", destination = "continent")
data$continent <- ifelse(data$country_name == 'Singapore', NA , data$continent)
data$country_name <- NULL
data$continent <- as.factor(data$continent)

# Imputing missing values using Random Forest Imputation


set.seed(333)
#data$continent <- ifelse(data$country_name == 'NaN', NA , data$country_name)
data$Landslide_Trigger <- factor(data$Landslide_Trigger)
data$Landslide_Trigger <- na.roughfix(data$Landslide_Trigger)
data <- rfImpute(Landslide_Trigger ~ ., data)
sapply(data, function(x) {sum(is.na(x))})

write.csv(data, "datafiltered.csv", row.names = FALSE)
index <- sample(1:dim(data)[1], dim(data)[1] * 0.75 , replace = FALSE)
train <- data[index,]
test <- data[-index,]

# Chi square tests

datanew <- read.csv("datanew_afterimputation.csv")
# Chi -square test for co-relation test
Landslide_Size <- datanew[,5]
Trigger <-  datanew[,1]
chisq.test(Trigger, Landslide_Size)
Landslide_Category <- datanew[,4]
chisq.test(Trigger, Landslide_Category)
Continent <- datanew[,11]
chisq.test(Trigger, Continent)
# Chi square test ends.


# Outlier Start
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
datanew1 <- data.frame(matrix(ncol=0,nrow=9129))
datanew1$injury_count <- as.data.frame(remove_outliers(datanew$injury_count))
datanew1$fatality_count <- remove_outliers(datanew$fatality_count)
datanew1$population <- remove_outliers(datanew$population)
sapply(datanew1, function(x) {sum(is.na(x))})
datanew2 <- datanew1[rowSums(is.na(datanew1[ , 1:3])) == 0, ]

# Outlier End

datanew$event_month <- as.factor(datanew$event_month)
datanew$fatality_count <- as.numeric(datanew$fatality_count)
datanew$injury_count <- as.numeric(datanew$injury_count)
str(datanew)
str(data)
index <- sample(1:dim(datanew)[1], dim(datanew)[1] * 0.75 , replace = FALSE)
train <- datanew[index,]
test <- datanew[-index,]


# Test with imputed data

datanew <- read.csv("datafiltered.csv")
datanew$event_month <- as.factor(datanew$event_month)
datanew$fatality_count <- as.numeric(datanew$fatality_count)
datanew$injury_count <- as.numeric(datanew$injury_count)
str(datanew)
str(data)
index <- sample(1:dim(datanew)[1], dim(datanew)[1] * 0.75 , replace = FALSE)
train <- datanew[index,]
test <- datanew[-index,]

# End manual data

set.seed(333)
data.rf <- randomForest(Landslide_Trigger ~ ., data = train, importance = TRUE, ntree = 2000)
print(data.rf)
par(mfrow=c(1,2))
varImpPlot(data.rf)
rf <- predict(data.rf, test, type="class")
confusionMatrix(rf, test$Landslide_Trigger, positive = "Yes")

# str(train)
# table(train$Landslide_Trigger)
# table(test$Landslide_Trigger)


# random forest model development


index <- sample(1:dim(data)[1], dim(data)[1] * 0.75 , replace = FALSE)
train <- data[index,]
test <- data[-index,]
table(train$Landslide_Trigger)
train <- SMOTE(Landslide_Trigger ~., train,perc.over = 400, perc.under = 200, k = 6)

table(train$Landslide_Trigger)

# RF Model 1

set.seed(333)
data.rf <- randomForest(Landslide_Trigger ~ ., data = train, importance = TRUE, ntree = 2000)
print(data.rf)
par(mfrow=c(1,2))
varImpPlot(data.rf)
rf1 <- predict(data.rf, test, type="class")
confusionMatrix(rf, test$Landslide_Trigger, positive = "Yes")

# RF model 2

data.rf <- randomForest(Landslide_Trigger ~ ., data = train, importance = TRUE, ntree = 1500)
print(data.rf)
par(mfrow=c(1,2))
varImpPlot(data.rf)
rf2 <- predict(data.rf, test, type="class")
confusionMatrix(rf, test$Landslide_Trigger, positive = "Yes")

# RF Model 3

data.rf <- randomForest(Landslide_Trigger ~ ., data = train, importance = TRUE, ntree = 1000)
print(data.rf)
par(mfrow=c(1,2))
varImpPlot(data.rf)
rf3 <- predict(data.rf, test, type="class")
confusionMatrix(rf, test$Landslide_Trigger, positive = "Yes")

# library(rfUtilities)
# rf.crossValidation(data.rf, train, p = 0.20, n = 5, seed = NULL,
#                    normalize = FALSE, bootstrap = FALSE, trace = FALSE)
# 
# trainx <- train[,-1]
# trainy <- train[,1]
# trainy <- factor(trainy)
# #trainy <- as.data.frame(train[,1])
# fold <- rfcv(trainx, trainy , cv.fold = 10, ntree = 1000)
# levels(trainy)
# randomForest(x = trainx, y = trainy, importance = TRUE, ntree = 1000)
# trainx$population <- as.numeric(trainx$population)
# trainx$event_month <- as.factor(trainx$event_month)
# trainx$event_time <- as.factor(trainx$event_time)
# trainx$landslide_category <- as.factor(trainx$landslide_category)
# trainx$landslide_size <- as.factor(trainx$landslide_size)
# trainx$fatality_count <- as.factor(trainx$fatality_count)
# trainx$injury_count <- as.factor(trainx$injury_count)
# trainx$continent <- as.factor(trainx$continent)
# 
# 
# trainy <- droplevels(trainy)
# trainy <- factor(trainy)
# unclass(trainy)
# fold <- rfcv(trainx, factor(trainy) , cv.fold = 10)
# length(trainy)
# nrow(trainx)
# 
# train.control <- trainControl(method = "cv", number = 3)
# # Train the model
# levels(train$Landslide_Trigger)
# 
# train$Landslide_Trigger <- droplevels(train$Landslide_Trigger)
# train$Landslide_Trigger <- factor(train$Landslide_Trigger)
# train$Landslide_Trigger
# model <- train(Landslide_Trigger ~., data = train, method = "rf",
#                trControl = train.control)
# print(model)





# Support Vector Machine

install.packages("kernlab")
library(kernlab)
data1 <- SMOTE(Landslide_Trigger ~., data,perc.over = 100, perc.under = 200, k = 6)
library(e1071)
n <- nrow(data1)
ntrain <- round(n*0.75)
set.seed(314)
tindex <- sample(n,ntrain)
train_iris <- data1[tindex,]
test_iris <- data1[-tindex,]
sapply(test_iris, function(x) {sum(is.na(x))})

Results<-data.frame(matrix(ncol=19,nrow=0))

colnames(Results)<-c('class_nm',
                     'tp_value',
                     'tn_value',
                     'fp_value',
                     'fn_value',
                     'sensitivity',
                     'specificity',
                     'pos_pred_value',
                     'neg_pred_value',
                     'precision',
                     'recall',
                     'f1',
                     'prevalence',
                     'detection_rate',
                     'detection_prevalence',
                     'balanced_accuracy',
                     'kernel',
                     'gamma',
                     'cost')

x = seq(0.1,0.5,by=0.1)

a<- -2
b<- 2
c = seq(a,b,by=1)
y = (10^c)
for (i in 1:5)
{
  for(j in 1:5)
  {
    svm1 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "sigmoid",gamma = x[i], cost = y[j])
    svm1$SV
    prediction <- predict(svm1, test_iris)
    xtab <- table(test_iris$Landslide_Trigger,prediction)
    cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
    cm_byclass<-as.data.frame(cm[["byClass"]])
    cm_byclass<-cbind(class_nm = rownames(cm_byclass), cm_byclass)
    row.names(cm_byclass)<-NULL
    cm_byclass$class_nm<-trimws(substring(cm_byclass$class_nm, regexpr(":", cm_byclass$class_nm) + 1),"l")
    
    #Recall_Construction<-cm_byclass[which(cm_byclass$class_nm=='Class: Construction'),"Sensitivity"]
    cm_pred_act<-as.data.frame(cm[["table"]]) 
    
    cm_pred_act$flag<-ifelse(cm_pred_act$Prediction==cm_pred_act$Reference,1,0)
    cm_pred_act_same_flag<-cm_pred_act[cm_pred_act$Prediction==cm_pred_act$Reference,]
    cm_pred_act_nsame_flag<-cm_pred_act[cm_pred_act$Prediction!=cm_pred_act$Reference,]
    
    #TP_Value
    TP_SVM1<- aggregate(cm_pred_act_same_flag$Freq,by = list(cm_pred_act_same_flag$Prediction),sum)
    colnames(TP_SVM1)<-c('class_nm','TP_Value')
    #FP value
    FP_SVM1<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Prediction),sum)
    colnames(FP_SVM1)<-c('class_nm','FP_Value')
    #FN Value
    FN_SVM1<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Reference),sum)
    colnames(FN_SVM1)<-c('class_nm','FN_Value')
    #TN_Value
    Total_TP<-sum(TP_SVM1$TP_Value)
    TP_SVM1$Total_TP<-Total_TP
    TP_SVM1$TN_value<-TP_SVM1$Total_TP-TP_SVM1$TP_Value
    TN_SVM1<-TP_SVM1[,c(1,4)]
    #TP_SVM1$Total_TP<-NULL
    #TP_SVM1$TN_value<-NULL
    pos_neg_values<-Reduce(merge, list(TP_SVM1, TN_SVM1, FP_SVM1,FN_SVM1))
    
    names(pos_neg_values) <- sub(" ", "", names(pos_neg_values))
    names(cm_byclass) <- sub(" ", "", names(cm_byclass))
    
    #All Metrics Collated
    all_metrics_svm1<-merge(pos_neg_values,cm_byclass ,by=c("class_nm"))
    all_metrics_svm1$kernel<-"Sigmoid"
    all_metrics_svm1$gamma<-x[i]
    all_metrics_svm1$cost<-y[j]
    
    Results<-rbind(Results,all_metrics_svm1)
    
  }
}


#------------------------------------------------------

Results_svm2<-data.frame(matrix(ncol=19,nrow=0))

colnames(Results_svm2)<-c('class_nm',
                     'tp_value',
                     'tn_value',
                     'fp_value',
                     'fn_value',
                     'sensitivity',
                     'specificity',
                     'pos_pred_value',
                     'neg_pred_value',
                     'precision',
                     'recall',
                     'f1',
                     'prevalence',
                     'detection_rate',
                     'detection_prevalence',
                     'balanced_accuracy',
                     'kernel',
                     'gamma',
                     'cost')

x = seq(0.1,0.5,by=0.1)

a<- -2
b<- 2
c = seq(a,b,by=1)
y = (10^c)
for (i in 1:5)
{
  for(j in 1:5)
  {
    
    svm2 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "linear",gamma = x[i], cost = y[j])
    svm2$SV
    prediction <- predict(svm2, test_iris)
    xtab <- table(test_iris$Landslide_Trigger,prediction)
    cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
    cm_byclass<-as.data.frame(cm[["byClass"]])
    cm_byclass<-cbind(class_nm = rownames(cm_byclass), cm_byclass)
    row.names(cm_byclass)<-NULL
    cm_byclass$class_nm<-trimws(substring(cm_byclass$class_nm, regexpr(":", cm_byclass$class_nm) + 1),"l")
    
    #Recall_Construction<-cm_byclass[which(cm_byclass$class_nm=='Class: Construction'),"Sensitivity"]
    cm_pred_act<-as.data.frame(cm[["table"]]) 
    
    cm_pred_act$flag<-ifelse(cm_pred_act$Prediction==cm_pred_act$Reference,1,0)
    cm_pred_act_same_flag<-cm_pred_act[cm_pred_act$Prediction==cm_pred_act$Reference,]
    cm_pred_act_nsame_flag<-cm_pred_act[cm_pred_act$Prediction!=cm_pred_act$Reference,]
    
    #TP_Value
    TP_SVM2<- aggregate(cm_pred_act_same_flag$Freq,by = list(cm_pred_act_same_flag$Prediction),sum)
    colnames(TP_SVM2)<-c('class_nm','TP_Value')
    #FP value
    FP_SVM2<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Prediction),sum)
    colnames(FP_SVM2)<-c('class_nm','FP_Value')
    #FN Value
    FN_SVM2<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Reference),sum)
    colnames(FN_SVM2)<-c('class_nm','FN_Value')
    #TN_Value
    Total_TP<-sum(TP_SVM2$TP_Value)
    TP_SVM2$Total_TP<-Total_TP
    TP_SVM2$TN_value<-TP_SVM2$Total_TP-TP_SVM2$TP_Value
    TN_SVM2<-TP_SVM2[,c(1,4)]
    #TP_SVM1$Total_TP<-NULL
    #TP_SVM1$TN_value<-NULL
    pos_neg_values<-Reduce(merge, list(TP_SVM2, TN_SVM2, FP_SVM2,FN_SVM2))
    
    names(pos_neg_values) <- sub(" ", "", names(pos_neg_values))
    names(cm_byclass) <- sub(" ", "", names(cm_byclass))
    
    #All Metrics Collated
    all_metrics_svm2<-merge(pos_neg_values,cm_byclass ,by=c("class_nm"))
    all_metrics_svm2$kernel<-"Linear"
    all_metrics_svm2$gamma<-x[i]
    all_metrics_svm2$cost<-y[j]
    
    Results<-rbind(Results,all_metrics_svm2)
    
  }
}

#------------

Results_svm3<-data.frame(matrix(ncol=19,nrow=0))

colnames(Results_svm3)<-c('class_nm',
                          'tp_value',
                          'tn_value',
                          'fp_value',
                          'fn_value',
                          'sensitivity',
                          'specificity',
                          'pos_pred_value',
                          'neg_pred_value',
                          'precision',
                          'recall',
                          'f1',
                          'prevalence',
                          'detection_rate',
                          'detection_prevalence',
                          'balanced_accuracy',
                          'kernel',
                          'gamma',
                          'cost')

x = seq(0.1,0.5,by=0.1)

a<- -2
b<- 2
c = seq(a,b,by=1)
y = (10^c)
for (i in 1:5)
{
  for(j in 1:5)
  {
    svm3 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "radial",gamma = x[i], cost = y[j])
    svm3$SV
    prediction <- predict(svm3, test_iris)
    xtab <- table(test_iris$Landslide_Trigger,prediction)
    cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
    cm_byclass<-as.data.frame(cm[["byClass"]])
    cm_byclass<-cbind(class_nm = rownames(cm_byclass), cm_byclass)
    row.names(cm_byclass)<-NULL
    cm_byclass$class_nm<-trimws(substring(cm_byclass$class_nm, regexpr(":", cm_byclass$class_nm) + 1),"l")
    
    #Recall_Construction<-cm_byclass[which(cm_byclass$class_nm=='Class: Construction'),"Sensitivity"]
    cm_pred_act<-as.data.frame(cm[["table"]]) 
    
    cm_pred_act$flag<-ifelse(cm_pred_act$Prediction==cm_pred_act$Reference,1,0)
    cm_pred_act_same_flag<-cm_pred_act[cm_pred_act$Prediction==cm_pred_act$Reference,]
    cm_pred_act_nsame_flag<-cm_pred_act[cm_pred_act$Prediction!=cm_pred_act$Reference,]
    
    #TP_Value
    TP_SVM3<- aggregate(cm_pred_act_same_flag$Freq,by = list(cm_pred_act_same_flag$Prediction),sum)
    colnames(TP_SVM3)<-c('class_nm','TP_Value')
    #FP value
    FP_SVM3<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Prediction),sum)
    colnames(FP_SVM3)<-c('class_nm','FP_Value')
    #FN Value
    FN_SVM3<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Reference),sum)
    colnames(FN_SVM3)<-c('class_nm','FN_Value')
    #TN_Value
    Total_TP<-sum(TP_SVM3$TP_Value)
    TP_SVM3$Total_TP<-Total_TP
    TP_SVM3$TN_value<-TP_SVM3$Total_TP-TP_SVM3$TP_Value
    TN_SVM3<-TP_SVM3[,c(1,4)]
    #TP_SVM1$Total_TP<-NULL
    #TP_SVM1$TN_value<-NULL
    pos_neg_values<-Reduce(merge, list(TP_SVM3, TN_SVM3, FP_SVM3,FN_SVM3))
    
    names(pos_neg_values) <- sub(" ", "", names(pos_neg_values))
    names(cm_byclass) <- sub(" ", "", names(cm_byclass))
    
    #All Metrics Collated
    all_metrics_svm3<-merge(pos_neg_values,cm_byclass ,by=c("class_nm"))
    all_metrics_svm3$kernel<-"Radial"
    all_metrics_svm3$gamma<-x[i]
    all_metrics_svm3$cost<-y[j]
    
    Results<-rbind(Results,all_metrics_svm3)
    
  }
}
#-------------------------

all_metrics_svm3


Results_svm4<-data.frame(matrix(ncol=19,nrow=0))

colnames(Results_svm4)<-c('class_nm',
                          'tp_value',
                          'tn_value',
                          'fp_value',
                          'fn_value',
                          'sensitivity',
                          'specificity',
                          'pos_pred_value',
                          'neg_pred_value',
                          'precision',
                          'recall',
                          'f1',
                          'prevalence',
                          'detection_rate',
                          'detection_prevalence',
                          'balanced_accuracy',
                          'kernel',
                          'gamma',
                          'cost')

x = seq(0.1,0.5,by=0.1)

a<- -2
b<- 2
c = seq(a,b,by=1)
y = (10^c)
for (i in 1:5)
{
  for(j in 1:5)
  {
    svm4 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "polynomial",gamma = x[i], cost = y[j])
    svm4$SV
    prediction <- predict(svm4, test_iris)
    xtab <- table(test_iris$Landslide_Trigger,prediction)
    cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
    cm_byclass<-as.data.frame(cm[["byClass"]])
    cm_byclass<-cbind(class_nm = rownames(cm_byclass), cm_byclass)
    row.names(cm_byclass)<-NULL
    cm_byclass$class_nm<-trimws(substring(cm_byclass$class_nm, regexpr(":", cm_byclass$class_nm) + 1),"l")
    
    #Recall_Construction<-cm_byclass[which(cm_byclass$class_nm=='Class: Construction'),"Sensitivity"]
    cm_pred_act<-as.data.frame(cm[["table"]]) 
    
    cm_pred_act$flag<-ifelse(cm_pred_act$Prediction==cm_pred_act$Reference,1,0)
    cm_pred_act_same_flag<-cm_pred_act[cm_pred_act$Prediction==cm_pred_act$Reference,]
    cm_pred_act_nsame_flag<-cm_pred_act[cm_pred_act$Prediction!=cm_pred_act$Reference,]
    
    #TP_Value
    TP_SVM4<- aggregate(cm_pred_act_same_flag$Freq,by = list(cm_pred_act_same_flag$Prediction),sum)
    colnames(TP_SVM4)<-c('class_nm','TP_Value')
    #FP value
    FP_SVM4<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Prediction),sum)
    colnames(FP_SVM4)<-c('class_nm','FP_Value')
    #FN Value
    FN_SVM4<-aggregate(cm_pred_act_nsame_flag$Freq,by = list(cm_pred_act_nsame_flag$Reference),sum)
    colnames(FN_SVM4)<-c('class_nm','FN_Value')
    #TN_Value
    Total_TP<-sum(TP_SVM4$TP_Value)
    TP_SVM4$Total_TP<-Total_TP
    TP_SVM4$TN_value<-TP_SVM4$Total_TP-TP_SVM2$TP_Value
    TN_SVM4<-TP_SVM4[,c(1,4)]
    #TP_SVM1$Total_TP<-NULL
    #TP_SVM1$TN_value<-NULL
    pos_neg_values<-Reduce(merge, list(TP_SVM4, TN_SVM4, FP_SVM4,FN_SVM4))
    
    names(pos_neg_values) <- sub(" ", "", names(pos_neg_values))
    names(cm_byclass) <- sub(" ", "", names(cm_byclass))
    
    #All Metrics Collated
    all_metrics_svm4<-merge(pos_neg_values,cm_byclass ,by=c("class_nm"))
    all_metrics_svm4$kernel<-"Polynomial"
    all_metrics_svm4$gamma<-x[i]
    all_metrics_svm4$cost<-y[j]
    
    Results<-rbind(Results,all_metrics_svm4)
    
  }
}

#---------------------------

Results

all_metrics_svm1

table(all_metrics_svm1$Sensitivity)
table(all_metrics_svm2$Sensitivity)
table(all_metrics_svm3$Sensitivity)
table(all_metrics_svm4$Sensitivity)

table(Results$Recall)
table(datanew$Landslide_Trigger)
summary(svm1)
summary(svm2)
summary(svm3)
summary(svm4)


all_metrics_svm1


prediction <- predict(svm1, test_iris)

xtab <- table(test_iris$Landslide_Trigger,prediction)

confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")

write.csv(datanew, file = "datanew.csv", row.names = FALSE)

write.csv(Results, file = "datanewresults.csv", row.names = FALSE)

table(data$Landslide_Trigger)


svm5 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "sigmoid",gamma = 0.1, cost = 100)
svm5$SV
prediction <- predict(svm5, test_iris)
xtab <- table(test_iris$Landslide_Trigger,prediction)
cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
cm            

svm6 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "linear",gamma = 0.1, cost = 100)
svm6$SV
prediction <- predict(svm6, test_iris)
xtab <- table(test_iris$Landslide_Trigger,prediction)
cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
cm 

svm7 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "radial",gamma = 0.1, cost = 100)

svm7$SV
prediction <- predict(svm7, test_iris)
xtab <- table(test_iris$Landslide_Trigger,prediction)
cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
cm  
                     
              

svm8 <- svm(Landslide_Trigger ~., data = train_iris, method = "C-classification", kernel = "polynomial",gamma = 0.1, cost = 100)
svm8$SV
prediction <- predict(svm8, test_iris)
xtab <- table(test_iris$Landslide_Trigger,prediction)
cm<-confusionMatrix(prediction, test_iris$Landslide_Trigger, positive = "Yes")
cm                       
                     

#****************** End ******************


















































































