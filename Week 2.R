library(caret)
# PreProcess
# Data Splitting
# Train/Test Function
# Model Comparison
library(kernlab)
# Data Splitting
inTrain = createDataPartition(y=spam$type,p=0.75,list=F)
training = spam[inTrain,]
testing = spam[-inTrain,]
dim(training)
# Fit a Model
modelFit = train(type~.,data=training,method="glm")
modelFit # see Accuracy and Kappa
modelFit$finalModel
# Prediction
predictions = predict(modelFit, newdata=testing)
predictions
confusionMatrix(predictions,testing$type)

# k-fold
set.seed(32323)
folds = createFolds(y=spam$type,k=10,list=T,returnTrain=T) # returnTrain = T, returns to train set
sapply(folds,length)
folds[[1]][1:10]

# Resample
folds = createResample(y=spam$type,times=10,list=T)
sapply(folds,length)

# Time Slices
set.seed(32323)
tme = 1:1000
folds = createTimeSlices(y=tme,initialWindow = 20, horizon = 10)
names(folds)

# Metric options
# continous outcomes: RMSE, Rsquare
# categorical outcomes: Accuracy, Kappa

set.seed(1235)
modelFit2 = train(type~.,data=training,method="glm")
modelFit2

# Plotting Predictors
library(ISLR)
library(ggplot2)
data(Wage)
summary(Wage)
# Get training/test sets
inTrain = createDataPartition(y=Wage$wage,p=0.7,list=F)
training = Wage[inTrain,]
testing = Wage[-inTrain,]
dim(training)
dim(testing)

# Feature Plot
featurePlot(x=training[,c("age","education","jobclass")],
            y=training$wage,
            plot="pairs")
# Qplot
qplot(age,wage,data=training)
qplot(age,wage,data=training,colour=jobclass)
qq = qplot(age,wage,colour=education,data=training)
qq + geom_smooth(method="lm",formula=y~x)

# cut2, making factors
library(Hmisc)
cutWage = cut2(training$wage,g=3)
table(cutWage)
p1 = qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))
p1
# 
p2 = qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

# Tables
t1 = table(cutWage,training$jobclass)
t1
prop.table(t1,1)
prop.table(t1,2)

# Density
qplot(wage,colour=education,data=training,geom="density")

# Preprocessing
hist(training$capitalAve,main="",xlab="ave.capital run length")
# Standardizing test value using train mean and sd

# Impute
set.seed(13343)
training$region_na = training$region
selectNA = rbinom(dim(training)[1],size=1,prob=0.05)==1
training$region_na[selectNA]=NA
preObj = preProcess(training[,-58],method="knnImpute")
region_na = predict(preObj,training[,-58])$region_na


# Covariate Creation
library(kernlab)
data(spam)
# transforming tidy covariates
spam$capitalAveSq = spam$capitalAve^2

library(ISLR)
library(caret)
data(Wage)
inTrain = createDataPartition(y=Wage$wage,p=0.7,list=F)
training = Wage[inTrain,]
testing = Wage[-inTrain,]

table(training$jobclass)
dummies = dummyVars(wage~jobclass,data=training)
head(predict(dummies,newdata=training))

# remove zero covariate
nsv = nearZeroVar(training,saveMetrics=T)
nsv

# splines
library(splines)
bsBasis = bs(training$age,df=3) # pylonomial
head(bsBasis)

# fit curve with splines
lm1 = lm(wage~bsBasis, data=training)
plot(training$age,training$wage,pch=19,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5)
