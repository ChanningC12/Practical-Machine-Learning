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

# Interpretable, Simple, Accurate, Fast (train and test), Scalable (apply to large datasets)

# In Sample Error
# Out of Sample Error
set.seed(333)
smallSpam = spam[sample(dim(spam)[1],size=10),]
spamLabel = (smallSpam$type=="spam")*1+1
plot(smallSpam$capitalAve,col=spamLabel)
# Prediction Rule 1: capitalAve > 2.7 = "Spam"
# capitalAve > 2.4 & <2.45 = "nonSpam"
# capitalAve >2.45 & <2.7 = "Spam"

Rule1 = function(x){
    prediction=rep(NA,length(x))
    prediction[x>2.7] = "spam"
    prediction[x<2.4] = "nonspam"
    prediction[x>=2.4 & x<=2.45] = "spam"
    prediction[x>2.45 & x<=2.7] = "nonspam"
    return(prediction)
}
table(Rule1(smallSpam$capitalAve),smallSpam$type)

Rule2 = function(x){
    prediction=rep(NA,length(x))
    prediction[x>2.8] = "spam"
    prediction[x<2.8] = "nonspam"
    return(prediction)
}
table(Rule2(smallSpam$capitalAve),smallSpam$type)

# apply to spam
table(Rule1(spam$capitalAve),spam$type)
table(Rule2(spam$capitalAve),spam$type)

# Look at Accuracy
sum(Rule1(spam$capitalAve)==spam$type)
sum(Rule2(spam$capitalAve)==spam$type,na.rm=T)
# rule2 performs better than rule1, overfitting

# 1. Define error rate
# 2. Train, Test, Validation
# 3. Cross-validation to pick features
# 4. Cross-validation to pick prediction function
# 5. Apply 1x to test set
# 6. apply to test set and refine, apply 1x to validation

# Rule of thumb 
# Large: 60% Training, 20% Test, 20% Validation
# Medium: 60% Training, 40% Test
# Small: Cross Validation

# 









