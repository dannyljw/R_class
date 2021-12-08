#Data Preprocessing
#Getting how many NAs
rowSums(is.na(titanic_train)) # NAs of each row
sum(rowSums(is.na(titanic_train))>0) # Number of rows w/ NA
sum(is.na(titanic_train)) # all NAs in the data set


# Omitting NAs
summary(titanic_train)
titanic_train.noNa <- na.omit(titanic_train)
summary(titanic_train.noNa)

summary(titanic_test)
titanic_train.noNa <- na.omit(titanic_train)
summary(titanic_train.noNa)


# Titanic
library(titanic)
library(dplyr)
library(caret)
library(e1071) 

#Gender model on titanic_train
tr_predict.Sex<-as.factor(titanic_train$Sex)
tr_predict.Sex<-as.numeric(tr_predict.Sex)
tr_predict.Sex<-as.factor(tr_predict.Sex%%2) # male:2,female:1 => male:0, female:1

confusionMatrix(as.factor(titanic_train$Survived),tr_predict.Sex)


#Gender_class_age model on titanic_train: Sam's 1st try
tr_predict.Sam <-c()
for(i in 1:nrow(titanic_train)){
  if(!is.na(titanic_train[i,"Age"]) && titanic_train[i,"Age"]<10){
    tr_predict.Sam[i]=1
  } else if(titanic_train[i,"Sex"]=="female" && titanic_train[i,"Pclass"]<=2) {
    tr_predict.Sam[i]=1
  } else if(titanic_train[i,"Sex"]=="male" && titanic_train[i,"Pclass"]==1){
    tr_predict.Sam[i]=1
  } else tr_predict.Sam[i]=0
}


tr_predict.Sam<-as.factor(tr_predict.Sam)
confusionMatrix(as.factor(titanic_train$Survived),tr_predict.Sam)

df<-as.data.frame(cbind(titanic_test$PassengerId,ts_predict.Sam))
colnames(df) <-c("PassengerId","Survived")

setwd("D:\\SamA\\Documents\\R\\ds")
write.csv(df,'titanic_sam.csv',row.names = F)


# 2. Single variable Data Analysis: 

# [1] Titanic_survival
library(carData)

# (1) 데이터 준비
room.class <- TitanicSurvival$passengerClass   # 선실 정보
room.class

# (2) 도수분포 계산
tbl <- table(room.class)
tbl
sum(tbl) 				    # 전체 탑승객수

nrow(titanic_train)+nrow(titanic_test) # see if it equls to the sum of train and test data

# (3) 막대그래프 작성
barplot(tbl, main='선실별 탑승객',
        xlab='선실 등급',
        ylab='탑승객수',
        col=c('blue', 'green', 'yellow')) 

#(4) 원그래프 작성
tbl/sum(tbl)   # 선실별 탑승객 비율
par(mar=c(1,1,4,1))
pie(tbl, main='선실별 탑승객',
    col=c('blue', 'green', 'yellow'))


# [2] 미국의 주별 고등학교 졸업률 분석하기: State.x77
# (1) 데이터 준비
grad <- state.x77[,'HS Grad'] 	# 주별 고등학교 졸업률
grad

# (2) 사분위수
summary(grad)
var(grad) 	# 분산
sd(grad) 	# 표준 편차

# (3) 히스토그램
hist(grad, main='주별 졸업률',
     xlab='졸업률',
     ylab='주의 개수',
     col='orange')


# (4) 상자그림
boxplot(grad, main='주별 졸업률',
        col='orange')

# (5) 졸업률이 가장 낮은 주
idx <- which(grad==min(grad))
grad[idx]

# (6) 졸업률이 가장 높은 주
idx <- which(grad==max(grad))
grad[idx]

# (7) 졸업률이 평균 이하인 주
idx <- which(grad<mean(grad))
grad[idx] 

#Lab2
library(DAAG)
str(carprice)
carprice$Price 		# 자동차 가격

range(carprice$Price) 	# 값의 범위
mean(carprice$Price) 	# 값의 평균

#가격에 대한 히스토그램을 작성하여 데이터의 분포를 확인합니다.
hist(carprice$Price, main='자동차 가격',
     xlab='가격대',
     ylab='빈도',
     col='green')

#자동차 타입(Type)별로 데이터의 빈도수를 계산하여 막대그래프로 확인합니다.
tbl <- table(carprice$Type)
barplot(tbl, main='자동차 타입별 빈도수',
        xlab='타입',
        ylab='빈도수',
        col=rainbow(length(tbl))) 

# 가격이 가장 비싼 자동차는 어떤 타입인지 알아봅니다.
idx <- which(carprice$Price==max(carprice$Price))
carprice[idx, c('Type','Price')]
