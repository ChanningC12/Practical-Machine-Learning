# Tree
data(iris)
library(ggplot2)
library(rpart)
names(iris)
table(iris$Species)
inTrain = createDataPartition(y=iris$Species,p=0.7,list=F)
training = iris[inTrain,]
testing = iris[-inTrain,]
dim(training)
dim(testing)

qplot(Petal.Width,Sepal.Width,colour=Species,data=training)
modFit = train(Species~.,method="rpart",data=training)
print(modFit$finalModel)
plot(modFit$finalModel,uniform=T,main="Classification Tree")
text(modFit$finalModel,use.n=T,all=T,cex=.8)
library(rattle)
fancyRpartPlot(modFit$finalModel)
predict(modFit,newdata=testing)

# classification tree is not linear model

# Bagging: bluestraps aggregating
# Estimating models, sometimes you average them together
# Basic idea: resample cases and recalculate predictions; average or majority vote
library(ElemStatLearn)
data(ozone,package="ElemStatLearn")
ozone = ozone[order(ozone$ozone),]

ll = matrix(NA,nrow=10,ncol=155)
for (i in 1:10){
    ss = sample(1:dim(ozone)[1],replace=T)
    ozone0 = ozone[ss,]
    ozone0 = ozone0[order(ozone0$ozone),]
    loess0 = loess(temperature~ozone,data=ozone0,span=0.2)
    ll[i,] = predict(loess0,newdata=data.frame(ozone=1:155))
}

plot(ozone$ozone,ozone$temperature,pch=19,cex=0.5)
for (i in 1:10){
    lines(1:155,ll[i,],col="grey",lwd=2)
}
lines(1:155,apply(ll,2,mean),col="red",lwd=2)

# caret bagging
predictors = data.frame(ozone=ozone$ozone)
temperature = ozone$temperature
treebag = bag(predictors,temperature,B=10, bagControl = bagControl(fit=ctreeBag$fit,predict=ctreeBag$pred, aggregrate = ctreeBag$aggregate))

# Random Forest
# at each split, bootstrap variables
data(iris)
library(ggplot2)
inTrain = createDataPartition(y=iris$Species ,p=0.7 ,list=F)
training = iris[inTrain,]
testing = iris[-inTrain,]
library(caret)
modFit = train(Species~.,data=training,method="rf",prox=T)
modFit
getTree(modFit$finalModel,k=2)
irisP = classCenter(training[,c(3,4)],training$Species,modFit$finalModel$prox)
irisP = as.data.frame(irisP)
irisP$Species = rownames(irisP)
p = qplot(Petal.Width,Petal.Length,col=Species,data=training)
p + geom_point(aes(x=Petal.Width,y=Petal.Length,col=Species),size=5,shape=4,data=irisP)

# Predicting new values
pred = predict(modFit,testing)
testing$predRight = pred==testing$Species
table(pred,testing$Species)

qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")


# Boosting
# Take lots of weak predictors, weight them and add them up, get a stronger predictor
library(ISLR)
data(Wage)
library(ggplot2)
library(caret)
Wage = subset(Wage,select=-c(logwage))
inTrain = createDataPartition(y=Wage$wage,p=0.7,list=F)
training = Wage[inTrain,]
testing = Wage[-inTrain,]
modFit = train(wage~.,method="gbm",data=training,verbose=F)
print(modFit)
qplot(predict(modFit,testing),wage,data=testing)

# Model Based Prediction
data(iris)
library(ggplot2)
inTrain = createDataPartition(y=iris$Species ,p=0.7 ,list=F)
training = iris[inTrain,]
testing = iris[-inTrain,]
library(caret)
modlda = train(Species~.,data=training,method="lda")
modnb = train(Species~.,data=training,method="nb")
plda = predict(modlda,testing)
pnb = predict(modnb,testing)
table(plda,pnb)
equalPredictions = (plda==pnb)
qplot(Petal.Width,Petal.Length,colour=equalPredictions,data=testing)
