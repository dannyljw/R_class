# Titanic
library(titanic)
str(titanic_train)
str(titanic_test)
str(titanic_gender_model)
str(titanic_gender_class_model)

Titanic

#Let's Try submission result to the competition in Kaggle
# https://www.kaggle.com/c/titanic/data
setwd("D:\\SamA\\Documents\\R\\ds")
write.csv(titanic_gender_class_model,'titanic_sam_gen_class.csv',row.names = F)
write.csv(titanic_gender_model,'titanic_sam_gen.csv',row.names = F)


#Pie Graph
favorite <- c('WINTER', 'SUMMER', 'SPRING', 'SUMMER', 'SUMMER',
              'FALL', 'FALL', 'SUMMER', 'SPRING', 'SPRING')
ds <- table(favorite) 	# frequency Table
ds
pie(ds, main='Favorite season', 	# Pie graph
    radius=1) 


pie(ds, main='Favorite season',
    col=c('brown','green','red','black'),   # Specify Colors
    radius=1 ) 	


#3D Pie 
install.packages('plotrix')
library(plotrix)

pie3D(ds, main='Favorite Season',
      labels=names(ds),                      # labels
      labelcex=1.0,                          # font
      explode=0.1,                           # space between pies
      radius=1.5,                            # pie size
      col=c('brown','green','red','yellow')) # colors	

#Line Graph
month = 1:12 			 # Data
late = c(5,8,7,9,4,6,12,13,8,6,6,4)    	 # Data
plot(month, 			 # x data
     late, 			 # y data
     main='Tradiness', 		 # title
     type='l', 			 # Type (alphabet)
     lty=1, 			 # line type
     lwd=1, 			 # line thickness
     xlab='Month', 		 # x label
     ylab='Late cnt' 		 # y label
)


#Multiple Line Plots
month = 1:12
late1 = c(5,8,7,9,4,6,12,13,8,6,6,4)
late2 = c(4,6,5,8,7,8,10,11,6,5,7,3)
plot(month, # x data
     late1, # y data
     main='Late students',
     type='b', #graph type
     lty=1,  #line type
     col='red', 		
     xlab='Month', 		
     ylab='Late cnt'		
)
lines(month, 	# x data
      late2, 	# y data
      type='b', 	# line type
      col='blue') # line color


#Lab1
install.packages('DAAG')
library(DAAG)

ds <- table(science$like)
pie(ds, main='선호 점수별 비율',
    col=rainbow(length(ds)), 		 # 파이의 색 지정
    radius=1) 			         # 파이의 크기 지정

install.packages('plotrix')
library(plotrix)

ds <- table(science$State)
pie3D(ds, main='주별 학생 비율',
      labels=names(ds), 		        	# 파이별 레이블 지정
      labelcex=1.0, 		        	# 레이블의 폰트 크기
      explode=0.1, 		      	# 파이 간 간격
      radius=1.5, 			# 파이의 크기
      col=c('brown','green')) 		# 파이의 색 지정

year = 1875:1972
ds <- data.frame(year, LakeHuron)

plot(ds$year, 			# x data
     ds$LakeHuron, 	        # y data
     main='수위 변화',
     type='b',
     lty=1,
     col='blue',
     xlab='연도',
     ylab='수위')


#Box plot
dist <- cars[,2] 		# Stopping distance (unit:feet)
boxplot(dist, main='Stopping distance')
boxplot.stats(dist)

# Scatter plot
wt <- mtcars$wt 			# weight
mpg <- mtcars$mpg		# miles per gallon
plot(wt, mpg, 			# x,y
     main='weight-mpg', 		# title
     xlab='Weight', 		# x-label
     ylab='MPG', 		# y-label
     col='red', 			# point color
     pch=19) 			# point type


#Scatter plot over multiple variables
vars <- c('mpg','disp','drat','wt') 	# target variables
target <- mtcars[,vars] 		        # data
head(target)
plot(target, 			        # plot
     main='Multi-plots') 