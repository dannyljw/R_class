#option(max.print=100000000)
#score <- rtruncnorm(n=100000000, a = 0, b=100, mean = 80,sd = 15)
#print(score)
my_sum <- function(data){
  result=0 # initialization
  for(i in 1:length(data)){
    result=result+data[i]
  }  
  return(result)
}
print(my_sum(score))





my_mean <- function(data, precision){
  result = 0
  for(i in 1:length(data)){
    result = result + data[i]
  }
  return(round(result/length(data), precision))
}

print(my_mean(score,2))

avg <- my_mean(score,2)



r_avg <- function(score){return (mean(score))}
print(r_avg(score))


my_sd <- function(data){
  sum_err = 0
  for(i in 1: length(data)){
    sum_err = sum_err + (data[i] - avg)^2
  }
  return(sqrt(sum_err/length(data)))
}

print(my_sd(score))

r_sd <- function(score){return(sd(score))}

print(r_sd(score))

