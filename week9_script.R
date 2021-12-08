favorite <- c('WINTER','SUMMER','SPRING','SUMMER','SUMMER','FALL','FALL','SUMMER','SPRING','SPRING') 
ds <- table(favorite) 	
barplot(ds, main='favorite season')
barplot(ds, main='favorite season',col='blue')
colors()
barplot(ds, main='favorite season',col=c('blue','red','green','yellow'))
barplot(ds, main='favorite season',col=rainbow(4))

barplot(ds, main='favorite season',col=rainbow(4), horiz=T)
barplot(ds, main='favorite season',col=rainbow(4), names=c('FA','SP','SU','WI'))
barplot(ds, main='favorite season',col=rainbow(4), las=2)

# x-label, y-label
barplot(ds, main='favorite season',
        col='green', 		
        xlab='Season', 		# x-label
        ylab='Freq' ) 		# y-label


#Multiple Graphs
age.A <- c(13709, 10974, 7979, 5000, 4250)
age.B <- c(17540, 29701, 36209, 33947, 24487)
age.C <- c(991, 2195, 5366, 12980, 19007)

ds <- rbind(age.A, age.B, age.C)
colnames(ds) <- c('1970','1990','2010','2030','2050')
ds
barplot(ds, main='Population Est') 

barplot(ds, main='Population Est',
        col=c('green','blue','yellow'))


barplot(ds, main='Population Est',
        col=c('green','blue','yellow'),
        beside=TRUE)


#Adding Legend
par(mfrow=c(1, 1), mar=c(5, 5, 5, 7))
barplot(ds, main='Population Est',
        col=c('green','blue','yellow'),
        beside=F,
        legend.text=T,
        args.legend = list(x='topright', bty='n', inset=c(-0.25,0)))



# LAB 1. 사업부문별 매출액 그래프 그리기
ha <- c(54659, 61028, 53307, 46161, 54180)
he <- c(31215, 29863, 32098, 39684, 29707)
mc <- c(15104, 16133, 15222, 13208, 9986)
vs <- c(13470, 14231, 13401, 13552, 13193)
bs <- c(16513, 14947, 15112, 14392, 17091)

ds <- rbind(ha, he, mc, vs, bs)
colnames(ds) <- c('19.1Q', '19.2Q', '19.3Q', '19.4Q', '20.1Q')

barplot(ds, main = '사업부문별 매출액')

barplot(ds, main = '사업부문별 매출액',
        col = c('#003f5c', '#58508d', '#bc5090', '#ff6361', '#ffa600'))

barplot(ds, main = '사업부문별 매출액',
        col = c('#003f5c', '#58508d', '#bc5090', '#ff6361', 
                '#ffa600'), 
        horiz = T, las = 1)

barplot(ds, main = '사업부문별 매출액',
        col = c('#003f5c', '#58508d', '#bc5090', '#ff6361', '#ffa600'),
        horiz = T, las = 1,
        xlab = '억 원', beside = T)

par(mfrow = c(1,1), mar = c(5, 5, 5, 10))

barplot(ds, main = '사업부문별 매출액',
        col = c('#003f5c', '#58508d', '#bc5090', '#ff6361', '#ffa600'),
        horiz = T, las = 1,
        xlab = '억 원', beside = T,
        legend.text = c('H&A','HE','MC','VS','BS'),
        args.legend = list(x='topright', bty='n', inset=c(-0.25,0)))

par(mfrow = c(1,1), mar = c(5,4,4,2)+.1)




#Histogram
head(cars)
dist <- cars[,2] 		# Stopping distance
dist
hist(dist, 			# data
     main='Histogram for Stop Dist', 	# title
     xlab ='Distance', 		# x-label
     ylab='Freq', 		# y-label
     border='blue', 		# border color of the bars
     col='green', 		# bar color
     las=2, 			# x-label direction (0~3)
     breaks=5) 			# number of bars


#Multiple plots
par(mfrow=c(2,2), mar=c(3,3,4,2)) 	# Screen Division (2x2)

hist(iris$Sepal.Length, 		# graph 1
     main='Sepal.Length',
     col='orange')

barplot(table(mtcars$cyl), 	# graph 2
        main='mtcars',
        col=c('red','green','blue'))

barplot(table(mtcars$gear), 	# graph 3
        main='mtcars',
        col=rainbow(3), 		# Rainbow palette
        horiz=TRUE)

pie(table(mtcars$cyl), 		# graph 4
    main='mtcars',
    col=topo.colors(3), 		# topo palette
    radius=2)

par(mfrow=c(1,1), mar=c(5,4,4,2)+.1) # cancel Screen Division


# LAB 2. 정책 지지도 그래프 그리기 
install.packages('carData')
library(carData)
ds <- Chile
colors <- rainbow(20) 	# 레인보우 팔레트에서 20색 선택

par(mfrow = c(2,3))

barplot(table(ds$region), main = '지역별 분포', col=colors[1:5])
barplot(table(ds$sex), main = '성별 분포', col=colors[6:7])
barplot(table(ds$education), main = '교육수준별 분포', col=colors[8:10])

hist(ds$age, breaks = 6, main = '연령', xlab = 'age', col=colors[1:6])
hist(ds$income, breaks = 4, main = '수입', xlab = 'income',
     col=colors[11:14])
hist(ds$statusquo, breaks = 9, main = '정책 지지도', xlab = 'support',
     col=colors[15:20]) 

par(mfrow = c(1,1))

install.packages("titanic")
library(titanic)
data("Titanic")