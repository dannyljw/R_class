color <- c('cyan', 'magenta', 'yellow', 'black')


sales <- c(20,15,21,19,33,42,10)
sales
names(sales) <- c('M', 'T', 'W', 'Th', 'F', 'Sa', 'Su' )
sales

#

good_sales_days <- function(sales,n){
  days <- sales[sales>=n]
  
  return(names(days))
}
good_sales_days(sales, 30)

