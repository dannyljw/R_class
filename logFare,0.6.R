library(titanic)
library(dplyr)
library(caret)
library(e1071)


##### Train Data set ####
titanic_train2 <- titanic_train
##### Test Data set ####

#A. Feature Selection(All): Pclass,Age,Sex,SibSp,Parch,Embarked
titanic_train2 <- select(titanic_train2, PassengerId,Survived, Pclass, Age, Sex, SibSp,Fare,
                         Parch, Embarked)
summary(titanic_test)
test <- select(titanic_test, PassengerId, Pclass, Age, Sex, SibSp,Fare,
               Parch, Embarked)


#titanic_train2 <- select(titanic_train2, PassengerId,Survived, Pclass, Age, Sex,
#                        Parch, Embarked)
head(titanic_train2)

summary(titanic_train2)
summary(test)
#B. NAs => mean


age.mean.male <- mean(titanic_train[which(titanic_train$Sex=='male'),'Age'],na.rm=T)
age.mean.male
age.mean.female <- mean(titanic_train[which(titanic_train$Sex=='female'),'Age'],na.rm=T)
age.mean.female
for(i in 1:nrow(titanic_train2)){
  if(titanic_train2[i,'Sex']=='male' && is.na(titanic_train2[i,'Age'])){
    titanic_train2[i,'Age']=age.mean.male
  }
  else if(titanic_train2[i,'Sex']=='female' && is.na(titanic_train2[i,'Age'])){
    titanic_train2[i,'Age']=age.mean.female
  }
}
summary(titanic_train2)


#c. Factorization / Numeric Conversion
titanic_train2$Survived <-as.factor(titanic_train2$Survived)
#titanic_train2$Survived

titanic_train2$Pclass <-as.factor(titanic_train2$Pclass)
#titanic_train2$Pclass
titanic_train2$Pclass <-as.numeric(titanic_train2$Pclass)
#titanic_train2$Pclass

titanic_train2$Sex <-as.factor(titanic_train2$Sex)
#titanic_train2$Sex
titanic_train2$Sex<-as.numeric(titanic_train2$Sex)
#titanic_train2$Sex


titanic_train2$SibSp<-as.numeric(titanic_train2$SibSp)
#titanic_train2$SibSp
#titanic_train2$Parch
titanic_train2$Parch<-as.numeric(titanic_train2$Parch)


titanic_train2$Embarked <-as.factor(titanic_train2$Embarked)
#titanic_train2$Embarked
titanic_train2$Embarked<-as.numeric(titanic_train2$Embarked)
#titanic_train2$Embarked
#titanic_train2$Fare <-as.factor(titanic_train2$Fare)
#titanic_train2$Sex
#titanic_train2$Fare<-as.numeric(titanic_train2$Fare)
titanic_train2$Fare <- log(titanic_train2$Fare + 1)
#c. TEST
titanic_test2$Fare <- log(titanic_test2$Fare + 1)
#titanic_test2$Survived <-as.factor(titanic_test2$Survived)
#titanic_train2$Survived

titanic_test2$Pclass <-as.factor(titanic_test2$Pclass)
#titanic_train2$Pclass
titanic_test2$Pclass <-as.numeric(titanic_test2$Pclass)
#titanic_train2$Pclass

titanic_test2$Sex <-as.factor(titanic_test2$Sex)
#titanic_train2$Sex
titanic_test2$Sex<-as.numeric(titanic_test2$Sex)
#titanic_train2$Sex


titanic_test2$SibSp<-as.numeric(titanic_test2$SibSp)
#titanic_train2$SibSp
#titanic_train2$Parch
titanic_test2$Parch<-as.numeric(titanic_test2$Parch)


titanic_test2$Embarked <-as.factor(titanic_test2$Embarked)
#titanic_train2$Embarked
titanic_test2$Embarked<-as.numeric(titanic_test2$Embarked)
#titanic_train2$Embarked
#titanic_test2$Fare <-as.factor(titanic_test2$Fare)
#titanic_train2$Sex
#titanic_test2$Fare<-as.numeric(titanic_test2$Fare)


#Use FSelector for best feature selection
#install.packages("FSelector")
library(FSelector) # Feature Selection Library
attach(titanic_train2)
head(titanic_train2)
weights <- symmetrical.uncertainty(Survived~., titanic_train2[, c(-1,-2)]) # feature Importance
print(weights)



#$$$$$$$$$$$$$$$$$$$
#titanic_train2 <- titanic_train2[, c(-6,-7)]
summary(titanic_train2)

#Classification
ds.train <- as.matrix(titanic_train2[, c(-1,-2)])
ds.train
cl.train <- titanic_train2$Survived
model.svmC <- svm(ds.train, cl.train,type='C-classification') #classification model
#model.svmC <- svm(ds.train, cl.train,type='eps-regression') #classification model
pred.train <- predict(model.svmC, ds.train) #prediction on train data themselves
confusionMatrix(cl.train,pred.train)

# Submit


ds.test <- as.matrix(test[, c(-1)])
ds.test
sum(is.na(ds.test)) # all NAs in the data set
ds.test[is.na(ds.test)] <- 0
Survived <- predict(model.svmC, ds.test)
summary(Survived)
PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
summary(output.df)
write.csv(output.df,"untitle13_baselinecode.csv",row.names = FALSE)

