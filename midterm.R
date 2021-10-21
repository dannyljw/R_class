student_space <- c()

for(i in 1:50){
  student_space <- c(student_space, paste('S' , i , sep = ''))
}


math.bf <- rnorm(50 , mean = 73 , sd = 10)
math.af <- rnorm(50 , mean = 78 , sd = 10)

df = data.frame('student' = student_space , 
                'math.bf' = math.bf,
                'math.af' = math.af)
df

mean(df$math.bf)
median(df$math.bf)
sd(df$math.bf)

mean(df$math.af)
median(df$math.af)
sd(df$math.af)




hist(df$math.bf, col = 'red')
hist(df$math.af, col = 'blue', add =TRUE)

t.test(df$math.bf, df$math.af, var.equal = TRUE, conf.level = 0.95)
var.test(df$math.bf, df$math.af, var.equal = FALSE)

write.csv(df,file = 'ex.csv')
