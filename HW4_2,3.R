

#======================================================#
ds <- table(mtcars$cyl, mtcars$gear)
color <- c('tomato','salmon','peachpuff')
barplot(ds,main='Distribution of carburetors',col=color,xlab = 'Number of gear',
        ylab = 'frequency',beside=T)
legend("topright",legend=c("cyl 4","cyl 6","cyl 8"),fill=color,border="black",box.lty=0,cex=1)




#======================================================#
install.packages("DAAG")
library(DAAG)
ds <- data.frame(greatLakes)
ds

plot(data = greatLakes,x=ds$Erie, y =ds$michHuron, main = "Scatter plot", xlab = "Erie", ylab = "michHuron")

vars<-c("Erie","michHuron","Ontario","StClair")
target <- ds[,vars]
head(target)
pairs(target,main="Multi Plots")



