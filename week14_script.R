#Quiz #2

# write the code to draw a scatter plot of all pairs
# write the code to get the correlation coefficients of all pairs.
# What pairs are the ones with more than 0.8 Correlation Coefficient?

library(ggplot2)
data(mtcars)
head(mtcars)
plot(mtcars)
res<-cor(mtcars)

coln <- colnames(mtcars)
n <- dim(res)[1]
cor.08 <- which(res>0.8)
row <- coln[ceiling(cor.08/n)]
col <- coln[cor.08%%n]

for(i in 1:n){
  if(row[i]!=col[i]) cat(row[i],":",col[i],"\n")
}

# cyl vs. disp
# cyl vs. hp
# disp vs wt


#multiple plot on iris
par(mfrow=c(2,2), mar=c(3,3,4,2)) 	# Screen Division (2x2)

color=c("red","green","blue")
plot(Petal.Length~Species,data=iris,col=color,main='Petal.Length')
plot(Petal.Width~Species,data=iris,col=color,main='Petal.Width')
plot(Sepal.Length~Species,data=iris,col=color,main='Sepal.Length')
plot(Sepal.Width~Species,data=iris,col=color,main='Sepal.Width')


#HW6-1 KNN on Titanic

#HW6-2  Ch12-실전분석 3 Treemap(lifeFemale & illiteracyFemale) (Page 459)
library(treemap)
library(carData)
data(UN98)
head(UN98)
df <-UN98[,c('region','lifeFemale','illiteracyFemale')]
df <- df[complete.cases(df),]
df$country <- rownames(df)

treemap(df,
        index=c('region','country'),
        vSize='lifeFemale',
        vColor='illiteracyFemale',
        type='value',
        bg.labels='yellow',
        title="World's Women")

#HW6-3 Ch12-Exercise 3 Radar Chart (iris data) (Page 462)
#iris 데이터셋의 4개 측정값열에 대해 평균을 구하고 이를 방사형 차트로 작성하시오(1~6)
library(fmsb)
iris.mean <- colMeans(iris[,-5])
max.score <-rep(6,4)
min.score <-rep(1,4)
ds <-rbind(max.score,min.score, iris.mean)
ds <- data.frame(ds)
colnames(ds) <- colnames(iris[,-5])

radarchart(ds)


############ Forbes2000 ############
install.packages('HSAUR')
library(tools)
library(HSAUR)

data("Forbes2000")
help(Forbes2000)
ds <- Forbes2000
ds[!complete.cases(ds),] 	# 결측값 확인

str(ds)
head(ds)

table(ds$country)
tmp <- sort(table(ds$country), decreasing=T)
top.10.contry <- tmp[1:10]
top.10.contry 	# 상위 10개국

par(mar=c(8,4,2,2))  # 그래프 여백 조정
barplot(top.10.contry,
        main='기업수 상위 10개국',
        col=rainbow(10),	 # 레인보우 팔레트
        las=2
)
par(mar=c(4,4,4,4))


# 업종별 기업 분포
table(ds$category)
tmp <- sort(table(ds$category), decreasing=T)
top.10.category <- tmp[1:10]
top.10.category 	# 상위 10개 업종

par(mar=c(10,4,4,2))
barplot(top.10.category,
        main='기업수 상위 10개 업종',
        col='pink',
        las=2)
par(mar=c(4,4,4,2))


# 업종별 기업자산 분포
tmp <- ds[ds$category %in% names(top.10.category),]
levels(tmp$category)
tmp$category <- factor(tmp$category)
levels(tmp$category)

par(mar=c(10,4,4,2))   # 그래프 여백 조정
boxplot(assets~category, data=tmp,
        ylim=c(0,100),
        xlab='',
        las=2)
par(mar=c(5,4,4,2))


# 기업 가치 상위 10대 기업
tmp <- ds[order(ds$marketvalue, decreasing=T),]
tmp[1:10,c('name', 'country','category','marketvalue')]

# 한국 기업 정보
korea <- subset(ds, country=='South Korea')
korea[,c('rank','name','category','marketvalue')]

# 기업 가치와 타 변수와의 상관관계
tmp <- ds[,5:8]
tmp <- tmp[complete.cases(tmp),] 	# 결측값 제거
plot(tmp, lower.panel=NULL) 	# 산점도
cor(tmp)				# 상관계수