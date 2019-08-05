#### SAN FRANCISCO CRIME ANALYSIS ####

#### ENVIRONMENT SET-UP AND LOADING PACKAGES ####

setwd("C:/Users/Eashani/Desktop/Spring 2019/Data analytics/project/sf-crime")
train1 = read.csv("train.csv")
test1 = read.csv("test.csv")



library(ggplot2)
library(plyr)
library(dplyr)
library(ggmap)
library('data.table')
library(readr)
library(lubridate)
set.seed(5)
################################################


#### DATA EXPLORATION ####

# create maps to visualize spatial data

register_google(key = "AIzaSyBueqSXA9SLOxZwG1aVz0e82-dhwAfOgqI")
bos_plot=ggmap(get_map('San Francisco, California',
                       zoom=13,
                       source='google',
                       maptype='terrain')) + 
  geom_point(data=train1, aes(x = train1$X, y = train1$Y, color = "#27AE60"))

 

#PRE-PROCESSING AND FEATURE ENGINEERING.
trainSF = train1
testSF= test1
# Rename columns of testing and training data set.
setnames(trainSF, names(trainSF), c('date', 'category_predict', 'description_ignore', 'day_of_week', 'pd_district', 'resolution', 'address', 'x', 'y'))
setnames(testSF, names(testSF), c('id', 'date', 'day_of_week', 'pd_district', 'address', 'x', 'y'))

# Get hour of each crime.
trainSF$hour = as.numeric(substr(trainSF$Dates, 12, 13))
testSF$hour = as.numeric(substr(testSF$Dates, 12, 13))
trainSF$year = as.factor(substr(trainSF$Dates, 1, 4))
testSF$year = as.factor(substr(testSF$Dates, 1, 4))
trainSF$month = as.factor(substr(trainSF$Dates, 6, 7))
testSF$month = as.factor(substr(testSF$Dates, 6, 7))
trainSF$month = as.factor(substr(trainSF$Dates, 6, 7))
testSF$month = as.factor(substr(testSF$Dates, 6, 7))
trainSF$iswkend = ifelse(trainSF$DayOfWeek =="Saturday" | trainSF$DayOfWeek =="Sunday" | trainSF$DayOfWeek =="Friday", 1,0)
testSF$iswkend = ifelse(testSF$DayOfWeek =="Saturday" | testSF$DayOfWeek =="Sunday" | testSF$DayOfWeek =="Friday", 1,0)

testSF$iswkend = as.factor(substr(testSF$Dates, 6, 7))
trainSF=trainSF[,-1]
testSF=testSF[,-1]
##############

#### Plot Violent crimes ###

viocrime <-
  trainSF %>%
  filter(Category %in% c("ASSAULT", "ROBBERY", "SEX OFFENSES FORCIBLE"))
viocrime1 <- as.data.frame(cbind(viocrime$X,viocrime$Y))

contours <- stat_density2d(
  aes(x = X, y = Y, fill = ..level.., alpha=..level..),
  size = 0.1, data = viocrime, n=200,
  geom = "polygon")

ggmap(get_map('San Francisco, California',
             zoom=12,
             source='google',
             maptype='terrain')) + contours +
  scale_alpha_continuous(range=c(0.25,0.4), guide='none') +
  scale_fill_distiller(palette= "Spectral", direction=-1) +
  
  ggtitle('Violent Crime in San Francisco')

#######

#1. number of crimes by day of week
options(scipen=999)
ggplot(trainSF,aes(x = DayOfWeek)) + geom_bar(colour = "black", fill = "skyblue")

#2. crimes by year

g <- ggplot(trainSF,aes(x=Category, y= year, fill = Category)) + 
  geom_bar(stat = "Identity") + coord_flip() + facet_grid(.~year) +theme(legend.position = "none")






coltypes <-list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))

train <-train1 %>% mutate(Year  = factor(year(Dates), levels=2003:2015),
                            Month = factor(month(Dates), levels=1:12),
                            Day   = day(Dates),
                            Hour  = factor(hour(Dates), levels=0:23),
                            dayDate = as.POSIXct(Dates, units = "days"),
                            DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                                   "Tuesday",
                                                                   "Wednesday",
                                                                   "Thursday",
                                                                   "Friday",
                                                                   "Saturday",
                                                                   "Sunday")))

test <-test1 %>% mutate(Year  = factor(year(Dates), levels=2003:2015), Month = factor(month(Dates), levels=1:12),
                          Day = day(Dates),Hour  = factor(hour(Dates), levels=0:23),dayDate = as.POSIXct(Dates, units = "days"),
                          DayOfWeek = factor(DayOfWeek, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))


# crimes by year
ggplot(data=train,aes(x=train$Year)) + geom_bar() + ggtitle("Year-wise distribution of crimes in various districts")

# crimes by month
ggplot(data=train,aes(x=train$Month)) + geom_bar(fill="brown") + ggtitle("Monthly distribution of crimes in various districts")

# crimes by day of week
ggplot(data=train,aes(x=train$Day)) + geom_bar(fill="blue") + ggtitle("Day-wise distribution of crimes in various districts")

# crime by hour of day 
ggplot(data=train,aes(x=train$Hour)) + geom_bar(fill=rainbow(24)) + ggtitle("Hourly distribution of crimes in various districts")




##### model based data analysis #####

# Define model.
model = Category ~ DayOfWeek + hour +month +  PdDistrict

# Set seed for reproducibility.
set.seed(1)
library(randomForest)
library(MLmetrics)
# Create random forest.
index1 <- sample(1:nrow(trainSF), 530557)
rf = randomForest(model, data = trainSF[index1,], ntree = 100, importance = T)
# View feature importance.
varImpPlot(rf)

# Compute model performance on training data.
train_pred =predict(rf,trainSF[-index1,-2])
test_pred =predict(rf,testSF,type="prob")
 print(test_pred)

x <- table(test_pred,trainSF[,2])


table(trainSF[-index1,2]==train_pred)

# Add training set predictions to 'train'.
trainSF$pred = trainSF_pred$V1
mtry <- 3
control <- trainControl(method='cv', 
                        number=3)
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(model, 
                    data=trainSF, 
                    method='rf', 
                    metric='accuracy', 
                    tuneGrid=tunegrid, 
                    trControl=control)
print(rf_default)


library(data.table)
library(rfUtilities)
test_pred = data.table(test_pred)
test_pred = cbind(testSF$Id, test_pred)
setnames(test_pred, names(test_pred), c('Id', names(test_pred)[2:ncol(test_pred)]))
# Create csv file of test predictions.
# This is commented out for now, since I don't actually want to create a csv.
write.csv(test_pred, 'rf_result.csv', row.names = F)


#####
# CROSS VALIDATION.
library(BioMedR)
# Conduct cross validation.
cv = print.rf.cv(model, 
           data = trainSF, 
           kcv = 3, 
           scale = T)
trainSF <- as.data.frame(trainSF)
cv1 = rf.crossValidation(rf,train1, n = 3,  ntree = 50,
           mtrysize = 3)
# View cross validation accuracy.
cv = data.table(cv[[1]])
print('Cross Validation Accuracy')
print(table(cv$y == cv$yhat))
print(prop.table(table(cv$y == cv$yhat)))

#####


###CREATE MODEL AND PREDICTIONS.
library(e1071)
# Create model and generate predictions for training set.
nb = naiveBayes(model, 
                data = trainSF)

# Generate predictions for training and test data.
# For the training data, I only want to compute accuracy.
# For the test data, I need to put predictions in a specific format for submission, as specified by Kaggle.com.
train1SF <- trainSF
train1SF$pred = data.table(predict(nb, newdata = trainSF, type = 'class'))


#
test_pred = data.table(predict(nb, 
                               newdata = testSF, 
                               type = 'raw'))
test_pred = data.table(test_pred)
test_pred = cbind(testSF$Id, test_pred)
setnames(test_pred, names(test_pred), c('Id', names(test_pred)[2:ncol(test_pred)]))
# Create csv file of test predictions.
write.csv(test_pred, 'nb_result.csv', row.names = F)

#####
# CHECK TRAINING SET ACCURACY.

# View training accuracy.
print('Training Accuracy:')
table(train$Category == train$pred)
prop.table(table(train$Category == train$pred))
library(party)
cforest(model, data=trainSF, controls=cforest_control(mtry=3, mincriterion=0))

#########################knn

install.packages("kknn")
library(kknn)

library(caret)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
knn_fit <- cv.kknn(model, data = trainSF,  kcv=3)
knn_fit


