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