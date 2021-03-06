# Regularized Regression
library(ElemStatLearn)
data(prostate)
str(prostate)
# Add more predictors, the training set error always decrease
# Approach: divide data into training/test/validation, treat validation as test data
small = prostate[1:5,]
lm(lpsa~.,data=small)
# Ridge Regression
# Lasso
# relaxo

# Combining Predictors
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage = subset(Wage,select=-c(logwage))

inBuild = createDataPartition(y=Wage$wage,p=0.7,list=F)
validation = Wage[-inBuild,]
buildData = Wage[inBuild,]

inTrain = createDataPartition(y=buildData$wage,p=0.7,list=F)
training = buildData[inTrain,]
testing = buildData[-inTrain,]
dim(validation)
dim(training)
dim(testing)

mod1 = train(wage~.,method="glm",data=training)
mod2 = train(wage~.,method="rf",data=training,trControl=trainControl(method='cv'),number=3)

pred1 = predict(mod1,testing)
pred2 = predict(mod2,testing)

qplot(pred1,pred2,colour=wage,data=testing)

# fit a model that combines predictors
predDF = data.frame(pred1,pred2,wage=testing$wage)
combModFit = train(wage~.,method="gam",data=predDF)
combPred = predict(combModFit,predDF)

sqrt(sum(pred1-testing$wage)^2)
sqrt(sum(pred2-testing$wage)^2)
sqrt(sum(combPred-testing$wage)^2)

# Fit Validation
pred1V = predict(mod1,validation)
pred2V = predict(mod2,validation)
predVDF = data.frame(pred1=pred1V,pred2=pred2V)
combPredV = predict(combModFit,predDF)

sqrt(sum(pred1V-validation$wage)^2)
sqrt(sum(pred2V-validation$wage)^2)
sqrt(sum(combPredV-validation$wage)^2)

# Forecasting
library(quantmod)
from.dat = as.Date("01/01/08",format="%m/%d/%y")
to.dat = as.Date("12/31/13",format="%m/%d/%y")
getSymbols("GOOG",src="google",from=from.dat,to=to.dat)







