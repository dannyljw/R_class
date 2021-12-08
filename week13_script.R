#HW 5-2 (2)히스토토그램, (3)원그래프프
#HW 5-3
library(Ecdat)
summary(Hdma)

#(1)
tbl <- table(Hdma$self)
barplot(tbl)

#(2)
tbl <- table(Hdma$single)
pie(tbl)

#(3-1)
boxplot(Hdma$uria)

#uria: 대출 신청자가 속한 직업군의 실업률 데이터터
#(3-2)  2.0~4.3
boxplot.stats(Hdma$uria) 


#(3-3) 457개
length(boxplot.stats(Hdma$uria)$out)

#hir: housing expenses to income ratio
#(4) 승인 거절: 0.2506052 0.2902124 
approve.hir <- mean(Hdma$hir[Hdma$deny=='no'])
deny.hir <- mean(Hdma$hir[Hdma$deny=='yes'])
cat('승인 거절:', approve.hir, deny.hir, '\n')

#(5) 수입대비 주택유지비용 비율이 높으면 대출에 불리하다. 



############ Treemap ############
# GNI2014
install.packages('treemap')
library(treemap) 		# treemap 패키지 불러오기
data(GNI2014) 			# 데이터 불러오기
head(GNI2014) 			# 데이터 내용 보기
treemap(GNI2014,
        index=c('continent','iso3'), # 계층 구조 설정(대륙-국가)
        vSize='population', 	# 타일의 크기
        vColor='GNI', 		# 타일의 컬러
        type='value', 		# 타일 컬러링 방법
        bg.labels='yellow', 	# 레이블의 배경색
        title="World's GNI") 	# 나무지도 제목 


# state.x77
st <- data.frame(state.x77) 	# 매트릭스를 데이터프레임으로 변환

# 주의 이름 열 stname을 추가
st <- data.frame(st, stname=rownames(st))

treemap(st,
        index=c('stname'), 	  # 타일에 주 이름 표기
        vSize='Area', 		  # 타일의 크기
        vColor='Income', 	  # 타일의 컬러
        type='value', 		  # 타일 컬러링 방법
        title='USA states area and income' ) # 나무그림의 제목

############ End of Treemap ############

install.packages('fmsb')
library(fmsb)


############ Radar Chart ############
install.packages('fmsb')
library(fmsb)
# (1) 데이터 준비
score <- c(80,60,95,85,40)
max.score <- rep(100,5) 	# 100을 5회 반복
min.score <- rep(0,5) 	# 0을 5회 반복
ds <- rbind(max.score,min.score, score)
ds <- data.frame(ds) 		# 매트릭스를 데이터프레임으로
colnames(ds) <- c('국어','영어','수학','물리','음악')
ds

# (2) 방사형 차트
radarchart(ds)

# LAB. 종교 유무를 조사한 데이터 분석하기 
library(carData)
library(fmsb)
head(WVS)

pop <- table(WVS$country) 		# 국가별 응답자수
tmp <- subset(WVS, religion=='yes')
rel <- table(tmp$country) 	# 국가별 종교가 있는 응답자수
stat <- rel/pop 			# 국가별 종교가 있는 응답자수 비율
stat

max.score <- rep(1,4) 		# 최댓값
min.score <- rep(0,4) 		# 최솟값
ds <- rbind(max.score,min.score, stat)
ds <- data.frame(ds) 		# 매트릭스를 데이터프레임으로 변환 

radarchart(ds, 				# 데이터프레임
           pcol='dark green',		# 다각형 선의 색
           pfcol=rgb(0.2,0.5,0.5,0.5), 	# 다각형 내부색
           plwd=3, 				# 다각형 선의 두께
           cglcol='grey', 			# 거미줄의 색
           cglty=1, 			# 거미줄의 타입
           axistype=1, 			# 축의 레이블 표시
           axislabcol='grey', 		# 축의 레이블 색
           caxislabels=seq(0,1,0.25), 	# 축의 레이블 값
           title='국가별 종교인 비율' 	# 그래프 제목
)


############ end of Radar Chart ############



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


# Testing the acc w/ k=1 to 10
for(n in 1:10){
  pred <-knn(ds.tr, ds.ts, cl.tr, k=n, prob=T) # which k yields the best acc?
  acc <-mean(pred==cl.ts) # accuracy
  cat(n,":",acc,"\n")
}

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

setwd("D:\\SamA\\Documents\\R\\ds")
write.csv(df,'titanic_sam_knn(k3).csv',row.names = F)
############ End KNN on Titanic ############


############ kfold cross validation ############ 
library(caret)
library(e1071)  # svm
library(class) # knn

ds <- iris[,-5] # 
cl <- factor(iris$Species)

kfold <- function(classifier, ds, cl, fold) {
  acc <- c()
  for (i in 1:length(fold)) {
    ds.train <- ds[-fold[[i]], ]
    ds.test <-  ds[fold[[i]], ]
    cl.train <- cl[-fold[[i]]]
    cl.test <- cl[fold[[i]]]
    
    if (classifier == 'svm') {
      model <- svm(ds.train, cl.train)
      pred <- predict(model, ds.test)
    } else if (classifier == 'knn') {
      pred <-knn(ds.train, ds.test, cl.train, k=2, prob=F) 
      pred<-factor(pred)
    }
    
    acc[i] <- mean(pred==cl.test)
  }
  
  return(mean(acc))
}

set.seed(100) # For the same result
fold <- createFolds(cl, k=3, list=TRUE, returnTrain = FALSE)

## SVM
acc <- kfold("svm", ds, cl, fold)
cat("svm :", acc, "\n")

## knn
acc <- kfold("knn", ds, cl, fold)
cat("knn :", acc, "\n")


############ k-means Clustering on Iris ############ 
# 모두를 위한 R데이터 분석 입문  (오세종)

mydata <- iris[,-5]
fit <-kmeans(x=mydata, centers=3)
fit$cluster  #각 데이터에 대한 군집 번호
fit$centers   # 각 군집의 중심점 좌표표

# 차원 축소 후 군집 시각화
library(cluster)
clusplot(mydata,fit$cluster, color=T, shade=T,labels=2, lines=0)

# 데이터에서 두 번째 군집의 데이터만 추출
subset(mydata, fit$cluster ==2)

############ End of k-means Clustering on Iris ############ 