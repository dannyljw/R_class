library(titanic)
library(dplyr)
library(caret)
library(e1071) 
library(class)

titanic_train2 <- titanic_train

# Data Preprocessing #
#A. Feature Selection(All): Pclass,Age,Sex,SibSp,Parch,Embarked
titanic_train2 <- select(titanic_train2, PassengerId,Survived, Pclass, Age, Sex, SibSp, Parch, Embarked)
head(titanic_train2)

#B. NAs => mean
age.mean.male <- mean(titanic_train[which(titanic_train$Sex=='male'),'Age'],na.rm=T)
age.mean.male

age.mean.female <- mean(titanic_train[which(titanic_train$Sex=='female'),'Age'],na.rm=T)
age.mean.female

for(i in 1:nrow(titanic_train2)){
  if(titanic_train2[i,'Sex']=='male' && is.na(titanic_train2[i,'Age'])){
    titanic_train2[i,'Age']=age.mean.male
  }
  else  if(titanic_train2[i,'Sex']=='female' && is.na(titanic_train2[i,'Age'])){
    titanic_train2[i,'Age']=age.mean.female
  }
}
summary(titanic_train2)

#c. Factorization / Numeric Conversion
titanic_train2$Survived <-as.factor(titanic_train2$Survived)
titanic_train2$Pclass <-as.factor(titanic_train2$Pclass)
titanic_train2$Pclass <-as.numeric(titanic_train2$Pclass)
titanic_train2$Sex <-as.factor(titanic_train2$Sex)
titanic_train2$Sex<-as.numeric(titanic_train2$Sex)
titanic_train2$Embarked <-as.factor(titanic_train2$Embarked)
titanic_train2$Embarked<-as.numeric(titanic_train2$Embarked)
titanic_train2$SibSp<-as.numeric(titanic_train2$SibSp)
titanic_train2$Parch<-as.numeric(titanic_train2$Parch)
#End of Data Preprocessing # 

ds.tr=titanic_train2[,c(-1,-2)]
cl.tr=titanic_train2[,2]

# training acc summary
pred <-knn(ds.tr, ds.tr, cl.tr, k=3, prob=F) # which k yields the best acc?
pred

acc <-mean(pred==cl.tr) # accuracy
acc

confusionMatrix(cl.tr,pred)


#### Test Data set ####
ds.test <- titanic_test
#A. Feature Selection(All): Pclass,Age,Sex,SibSp,Parch,Embarked
ds.test <- select(ds.test, PassengerId,Pclass, Age, Sex, SibSp, Parch, Embarked)
head(ds.test)

#B. NAs => mean
for(i in 1:nrow(ds.test)){
  if(ds.test[i,'Sex']=='male' && is.na(ds.test[i,'Age'])){
    ds.test[i,'Age']=age.mean.male
  }
  else  if(ds.test[i,'Sex']=='female' && is.na(ds.test[i,'Age'])){
    ds.test[i,'Age']=age.mean.female
  }
}


#c. Factorization / Numeric Conversion
ds.test$Pclass <-as.factor(ds.test$Pclass)
ds.test$Pclass <-as.numeric(ds.test$Pclass)
ds.test$Sex <-as.factor(ds.test$Sex)
ds.test$Sex<-as.numeric(ds.test$Sex)
ds.test$Embarked <-as.factor(ds.test$Embarked)
ds.test$Embarked<-as.numeric(ds.test$Embarked)
ds.test$SibSp<-as.numeric(ds.test$SibSp)
ds.test$Parch<-as.numeric(ds.test$Parch)

summary(ds.test)


pred.test <-knn(ds.tr, ds.test[,-1], cl.tr, k=3, prob=F) 
pred.test

df<-as.data.frame(bind_cols (titanic_test$PassengerId,pred.test))
colnames(df) <-c("PassengerId","Survived")


write.csv(df,'titanic_KNN.csv',row.names = F)
############ End KNN on Titanic ############


# 2
install.packages("treemap")
install.packages("carData")
library(treemap)
library(carData)

data(UN98)
df <- UN98[,c('region','lifeFemale', 'illiteracyFemale')]
df <- df[complete.cases(df)]
df$country <- rownames(df)

treemap(df,
        index =c('region', 'country'),
        vSize = 'lifeFemale',
        vColor = 'illiteracyFemale',
        type = 'value',
        bg.labels = 'yellow',
        title = "World's Women")



#3
install.packages("fmsb")
library(fmsb)
data(iris)
iris.data <- c(mean(iris$Sepal.Length),mean(iris$Sepal.Width),mean(iris$Petal.Length),mean(iris$Petal.Width))
# 평균
iris.data

max.iris <- rep(6,4)
min.iris <- rep(1,4)

# dataFrame
ds.iris <- rbind(max.iris,min.iris,iris.data)
ds.iris <- data.frame(ds.iris)
ds.iris
colnames(ds.iris) <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width')
# 방사형차트
radarchart(ds.iris)

