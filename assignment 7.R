#Question 1
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\Train.csv")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\Test.csv')
test$Age[is.na(test$Age)] = mean(test$Age, na.rm=TRUE)
train$Age[is.na(train$Age)] = mean(train$Age, na.rm=TRUE)
levels(train$Embarked)
levels(train$Embarked)[1] = 'S'
train$Embarked <- sapply(as.character(train$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)
test$Embarked <- sapply(as.character(test$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)
train$Sex <- ifelse(train$Sex == 'male', 1, 0)
test$Sex <- ifelse(test$Sex == 'male', 1, 0)
#fix(train)

#logistic regression
attach(train)
LR = glm( factor(Survived)~factor(Pclass) + Sex+ Age+SibSp+ Parch+factor(Embarked), data= train, family= 'binomial')
LR.probs = predict(LR, type= 'response', newdata= test)
LR.probs
LR.probs = ifelse(LR.probs > 0.5, 1, 0)
table(LR.probs,test$Survived)
mean(LR.probs == test$Survived)

 #LDA
library(MASS)
LDA = lda(factor(Survived)~ factor(Pclass) + Sex+ Age+SibSp+ Parch+factor(Embarked) , data= train)
LDA.pred = predict(LDA , newdata= test)
LDA.class = LDA.pred$class
LDA.class
table(LDA.class, test$Survived )
mean(LDA.class != test$Survived)

#QDA
library(MASS)
QDA = qda(factor(Survived)~factor(Pclass) + Sex+ Age+SibSp+ Parch + factor(Embarked), data= train, family= binomial)
QDA.pred = predict(QDA , newdata= test)
QDA.class = QDA.pred$class
QDA.class
table(QDA.class, test$Survived )
mean(QDA.class != test$Survived)

.#KNN
library(FNN) 
#train$Age <- ifelse(train$Age<18, 1, 0)
#test$Age <- ifelse(test$Age<18, 1, 0)
train1= train[, -c(1,2,4,9,10,11)]
test1= test[, -c(1,2,4,9,10,11)]
train.Y = train$Survived
set.seed(12)
knn1 <- knn(train1, test1, train.Y, k=1)
table(knn1, test$Survived)
mean(knn1 != test$Survived)
knn2 = knn(train1, test1, train.Y, k=2)
table(knn2, test$Survived)
mean(knn2 != test$Survived)
knn3 = knn(train1, test1, train.Y, k=3)
table(knn3, test$Survived)
mean(knn3 != test$Survived)
knn4 = knn(train1, test1, train.Y, k=4)
table(knn4, test$Survived)
mean(knn4 != test$Survived)
knn5 = knn(train1, test1, train.Y, k=5)
table(knn5, test$Survived)
mean(knn5 != test$Survived)

#question 2
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\Train.csv")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\Test.csv')
test$Age[is.na(test$Age)] = mean(test$Age, na.rm=TRUE)
train$Age[is.na(train$Age)] = mean(train$Age, na.rm=TRUE)
levels(train$Embarked)
levels(train$Embarked)[1] = 'S'
install.packages('naniar')
library(naniar)
levels(test$Cabin)
levels(test$Cabin)[1] = 'NA'
attach(test)
formula1 = as.formula('factor(Survived)~factor(Pclass) + Sex+ Age+SibSp+ Parch+factor(Embarked)+Cabin')
LR_C = glm(formula1,data= test, family='binomial')
LR_C.probs = predict(LR_C)
LR_C.probs = ifelse(LR_C.probs > 0.5, 1, 0)
mean(LR_C.probs!= test$Survived)

LR_C1 = glm(factor(Survived)~factor(Pclass) + Sex+ Age+SibSp+ Parch+factor(Embarked),data= test, family= 'binomial')
LR_C1.probs = predict(LR_C1)
LR_C1.probs = ifelse(LR_C1.probs > 0.5, 1, 0)
mean(LR_C1.probs!= test$Survived)


#question3
install.packages('vcd')
library(vcd)
attach(train)
#fix(train)
od = glm(factor(Survived)~factor(Pclass) + Sex+ factor(Embarked), data= train, family = 'binomial')
x= od$coefficients
or = exp(x)
or

#question 4
formula = as.formula('factor(Survived)~factor(Pclass) + Sex+ Age+SibSp+ Parch+factor(Embarked)')
LR_0.5 = glm(formula, data= train, family= 'binomial')
LR.probs_0.5 = predict(LR_0.5, type= 'response', newdata= test)
LR.probs_0.5 = ifelse(LR.probs_0.5 > 0.5, 1, 0)
table(LR.probs_0.5,test$Survived)
mean(LR.probs_0.5!= test$Survived)

LR_0.2 = glm(formula, data= train, family= 'binomial')
LR.probs_0.2 = predict(LR_0.2, type= 'response', newdata= test)
LR.probs_0.2 = ifelse(LR.probs_0.2> 0.2, 1, 0)
table(LR.probs_0.2,test$Survived)
mean(LR.probs_0.2!= test$Survived)


LR_0.8= glm(formula, data= train, family= 'binomial')
LR.probs_0.8 = predict(LR_0.8, type= 'response', newdata= test)
LR.probs_0.8 = ifelse(LR.probs_0.8 > 0.8, 1, 0)
table(LR.probs_0.8,test$Survived)
mean(LR.probs_0.8!= test$Survived)

#question 5
install.packages('ROCR')
library(ROCR)
pred= predict(LR, type= 'response', newdata= test)
pr = prediction(pred,test$Survived)
prf<- performance(pr, measure='tpr', x.measure='fpr')
plot(prf)
abline(0,1)
prf
auc_ROCR <- performance(pr, measure = "auc")
auc<- auc_ROCR@y.values[[1]]
auc


#question 6
library(FNN)
train = read.csv("C:\\Users\\KIRAN KONDISETTI\\Desktop\\Train.csv")
test = read.csv('C:\\Users\\KIRAN KONDISETTI\\Desktop\\Test.csv')
test$Age[is.na(test$Age)] = mean(test$Age, na.rm=TRUE)
train$Age[is.na(train$Age)] = mean(train$Age, na.rm=TRUE)
levels(train$Embarked)
levels(train$Embarked)[1] = 'S'
train$Embarked <- sapply(as.character(train$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)
test$Embarked <- sapply(as.character(test$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)
train$Sex <- ifelse(train$Sex == 'male', 1, 0)
test$Sex <- ifelse(test$Sex == 'male', 1, 0)
LR_7 = glm(factor(Survived)~factor(Pclass) + Sex+ Age+SibSp+ Parch+factor(Embarked)+Parch+Fare,data = train, family= 'binomial')
summary(LR_7)
train2= train[, -c(1,2,4,8,9,10,11,12)]
test2= test[, -c(1,2,4,8,9,10,11,12)]
train.Y1= train$Survived
train1
set.seed(143)
knn11 <- knn(train2, test2, train.Y1, k=1)
table(knn11, test$Survived)
mean(knn11 != test$Survived)
knn21 = knn(train2, test2, train.Y1, k=2)
table(knn21, test$Survived)
mean(knn21 != test$Survived)
knn31 = knn(train2, test2, train.Y1, k=3)
table(knn31, test$Survived)
mean(knn31 != test$Survived)
knn41 = knn(train2, test2, train.Y1, k=4)
table(knn41, test$Survived)
mean(knn41 != test$Survived)
knn51 = knn(train2, test2, train.Y1, k=5)
table(knn51, test$Survived)
mean(knn51 != test$Survived)

