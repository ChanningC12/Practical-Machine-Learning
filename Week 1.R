# Question(What to predict) - Input data - Features - Algorithm - Parameters - Evaluation
library(kernlab)
data(spam)
head(spam)
str(spam)
# Look at frequency of "your"
plot(density(spam$your[spam$type=="nonspam"]),col="blue",main="",xlab="Frequency of 'Your'")
lines(density(spam$your[spam$type=="spam"]),col="red")
# Find a value "C", cutoff to predict spam
abline(v=0.5,col="black")
# Evaluate precision
prediction = ifelse(spam$your>0.5,"spam","nonspam")
table(prediction,spam$type)
table(prediction,spam$type)/length(spam$type)

