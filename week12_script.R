#HW4-2
# 2. Ch8-Exercise 5 [10]
ds <- table(mtcars$cyl, mtcars$gear)       # 행 = cyl, 열 = gear
color <- c('tomato', 'salmon', 'peachpuff')

barplot(ds,
        main = 'Distribution of carburetors',
        col = color,
        xlab = 'Number of gear',
        ylab = 'frequency',
        legend.text = c('cyl 4', 'cyl 6', 'cyl 8'),
        args.legend = list(x ='topright', bty = 'n', inset = c(0,0)),
        beside = T)


#HW4-3
library(DAAG)
data(greatLakes)
ds <- data.frame(greatLakes)
# (1) Erie호와 michHuron호의 수위 분포를 산점도로 나타내시오.
plot(ds$Erie, ds$michHuron)
# 또는
plot(ds[,c('Erie','michHuron')])

# (2) 4개 호수의 수위를 다중 산점도로 나타내시오.
plot(ds)



#multi-variable Data Analysis
# (1) 데이터 확인
head(pressure)

# (2) 산점도 작성
plot(pressure$temperature, # x축 데이터
     pressure$pressure, # y축 데이터
     main='Temp vs. pressure', # 그래프 제목
     xlab='Temp(F)', # x축 레이블
     ylab='Pressure', # y축 레이블
)


#Correlation Coefficient eg1. Car speed vs. Stopping distance

#(1) 데이터의 확인
head(cars)

#(2) 산점도의 작성
plot(cars$speed, # x축 데이터
     cars$dist, # y축 데이터
     main='Speed vs. Stop. Dist.', # 그래프 제목
     xlab='Speed',     # x축 레이블
     ylab='Stop Distance', # y축 레이블
)

#(3) 상관계수
cor(cars$speed, cars$dist)


#Multi-variable Correlation Coefficient
st <- data.frame(state.x77)
head(st)

# multi-scatter plots
plot(st)

# multi-correlation coefficients
cor(st)


#Lab 1
str(longley)
head(longley)

df <- longley[,c('GNP', 'Unemployed', 'Armed.Forces', 'Population','Employed')]
df
plot(df)
cor(df)


#Sampling
x <- 1:100
y <- sample(x, size=20, replace=F) # 비복원 추출
y
z <- sample(x, size=20, replace=T) # 복원 추출
z

idx <- sample(1:nrow(iris), size=50, replace=F)
iris.50 <- iris[idx,] # 50개의 행 추출
dim(iris.50) # 행과 열의 개수 확인
head(iris.50)


#Lab2
# KosteckiDillon 데이터셋을 불러와서 내용을 확인합니다.
library(carData)
str(KosteckiDillon)

#전체 데이터에 대한 평균 치료일수(dos)를 구합니다.
tot.mean <- mean(KosteckiDillon$dos)
tot.mean

# 샘플링 비율을 달리하면서 평균 치료일수(dos)를 구하여 tot.mean과의 차이를 확인합니다.
for (rate in (1:5)*0.1) {
  set.seed(100)
  idx <- sample(nrow(KosteckiDillon), nrow(KosteckiDillon)*rate)
  sam.data <- KosteckiDillon[idx,'dos'] # 샘플링 데이터 추출
  tmp.mean <- mean(sam.data) # 샘플링 데이터 평균
  cat('Diff:', rate, tot.mean-tmp.mean, '\n')
}


############ KNN on IRIS ############

library(class)
#1. Let's train and test on the whole iris data set
ds.trAll <-iris[, 1:4] #training data set
cl.trAll <- factor(iris[, 5])

predAll <-knn(ds.trAll, ds.trAll, cl.trAll, k=3,prob=T)
predAll

accAll <-mean(predAll==cl.trAll) # accuracy
accAll

table(predAll,cl.trAll)


#2. Dividing training and testing data set
tr.idx <-c(1:25,51:75, 101:125)
ds.tr <-iris[tr.idx, 1:4] #training data set
ds.ts <-iris[-tr.idx, 1:4] #testing data set
cl.tr <- factor(iris[tr.idx, 5])
cl.ts <- factor(iris[-tr.idx, 5])

pred <-knn(ds.tr, ds.ts, cl.tr, k=2,prob=T)
pred

acc <-mean(pred==cl.ts) # accuracy
acc

table(pred,cl.ts)

############ End KNN on IRIS ############




############ KNN on Titanic ############
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


pred <-knn(ds.tr, ds.tr, cl.tr, k=3, prob=T) # which k yields the best acc?
pred

acc <-mean(pred==cl.tr) # accuracy
acc

confusionMatrix(cl.tr,pred)

############ End of KNN on Titanic ############