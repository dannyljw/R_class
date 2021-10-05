my_mean <- function(data,precision){
  result=0 # initialization
  for(i in 1:length(data)){
    result=result+data[i]
  }  
  return(round(result/length(data),precision))
}

print(my_mean(scores,2))

