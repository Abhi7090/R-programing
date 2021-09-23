
library(data.table)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(pROC)
library(glmnet)
library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(DataExplorer)



##  Importing the Dataset
df=read.csv("D:/Abhi reddy/NMIT M.tech/creditcard.csv")
head(df)
nrow(df)
ncol(df)
tail(df)
summary(df)
dim(df)
table(df$Class)
names(df)
var(df$Amount)
sd(df$Amount)
sum(is.na(df))
df<-as.data.frame(df)
##plotting graphs
boxplot(df)
smoothScatter(df)
sunflowerplot(df)
plot_density(df)
plot_correlation(df, type = 'continuous','Date')
plot_missing(df)
plot_histogram(df)




## Data Manipulation

df$Amount=scale(df$Amount)
NewData=df[,-c(1)]
head(NewData)
##Data Modeling


data_sample = sample.split(NewData$Class,SplitRatio=0.80)
train_data = subset(NewData,data_sample==TRUE)
test_data = subset(NewData,data_sample==FALSE)
dim(train_data)
dim(test_data)


##Fitting Model

Logistic_Model=glm(Class~.,test_data,family=binomial())
summary(Logistic_Model)
plot(Logistic_Model)

## Fitting a Decision Tree Model

decisionTree_model <- rpart(Class ~ . , df, method = 'class')
summary(decisionTree_model)
predicted_val <- predict(decisionTree_model, df, type = 'class')
probability <- predict(decisionTree_model, df, type = 'prob')
rpart.plot(decisionTree_model)

