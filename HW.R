n = 5
ifelse ( n<0, -n,n)

#question 2-a
fibo <- function(n){
  if (n==1 || n==2){
    return(1)
  }
  return(fibo(n-1) + fibo(n-2))
}

print(fibo(10))


#question 2-b
#in document

#question 3-a
rand <- sample(x=200:500, size = 9, replace = FALSE)
print(rand)


#question 3-b
rand[10] <- 10000
print(rand[10])
print(rand)

#question 3-c
my_mean <- function(rand){
  return(mean(rand))
}
print(my_mean(rand))

my_median <- function(rand){
  return(median(rand))
}
print(my_median(rand))

#question 3-d
#thinking


#minmax <- function(x){
# return((x-min(x)) / (max(x) - min(x)))
#}

#question 4
getwd()
df_girl <- read.csv(file = "rawdata_height_girl.csv",header = TRUE, sep =",",encoding = "UTF-8")
head(df_girl)

df_boy <- read.csv(file = "rawdata_height_boy.csv",header = TRUE, sep =",",encoding = "UTF-8")
head(df_boy)


class(df_girl)

num_girl <- as.numeric(unlist(df_girl))
num_boy <- as.numeric(unlist(df_boy))


hist(num_girl, breaks = 50)
hist(num_boy, breaks = 50)

plot(num_girl )
plot(num_boy,  add = T)


t.test(df_girl$height_girl, df_boy$height_boy, var.equal = TRUE)
var.test(df_girl$height_girl, df_boy$height_boy, var.equal = FALSE)
#------------------------------------------------------------------
log_girl = transform(num_girl, num_girl = log(num_girl+1))
print(log_girl)
log_girl <- as.numeric((unlist(log_girl)))
log_girl_hist = hist(log_girl, freq = TRUE, breaks = 10)
