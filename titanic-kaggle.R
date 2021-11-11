train.tit <- read.csv("titanic_train.csv",stringsAsFactors = F)
test.tit <- read.csv("titanic_test.csv", stringsAsFactors = F)
test.tit$Survived <-NA
full_titanic <- rbind(train.tit, test.tit)

##Check the structure
str(full_titanic)

###is there any Missing obesrvation

colSums(is.na(full_titanic))

####Empty data
colSums(full_titanic=='')
##Summary shows, Age missing 263 value, Cabin too having lot of missing value and embarked just 2

###Lets replace Embarked by most frequest observation 

table(full_titanic$Embarked)
full_titanic$Embarked[full_titanic$Embarked==""]="S"
table(full_titanic$Embarked)
apply(full_titanic,2, function(x) length(unique(x)))
cols=c("Survived","Pclass","Sex","Embarked")
for (i in cols){
  full_titanic[,i]=as.factor(full_titanic[,i])
}
install.packages("ggplot")
library(ggplot2)
ggplot(full_titanic[1:891,],aes(x = Pclass,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Pclass v/s Survival Rate")+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")  
ggplot(full_titanic[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("3D view of sex, pclass, and survival") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")


head(full_titanic$Name)

names <- full_titanic$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)

full_titanic$title <- title

table(title)

full_titanic$title[full_titanic$title == 'Mlle']        <- 'Miss' 
full_titanic$title[full_titanic$title == 'Ms']          <- 'Miss'
full_titanic$title[full_titanic$title == 'Mme']         <- 'Mrs' 
full_titanic$title[full_titanic$title == 'Lady']          <- 'Miss'
full_titanic$title[full_titanic$title == 'Dona']          <- 'Miss'

## I am afraid creating a new varible with small data can causes a overfit
## However, My thinking is that combining below feauter into original variable may loss some predictive power as they are all army folks, doctor and nobel peoples 

full_titanic$title[full_titanic$title == 'Capt']        <- 'Officer' 
full_titanic$title[full_titanic$title == 'Col']        <- 'Officer' 
full_titanic$title[full_titanic$title == 'Major']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Dr']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Rev']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Don']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Sir']   <- 'Officer'
full_titanic$title[full_titanic$title == 'the Countess']   <- 'Officer'
full_titanic$title[full_titanic$title == 'Jonkheer']   <- 'Officer'


# Lets check who among Mr, Master, Miss having a better survival rate
ggplot(full_titanic[1:891,],aes(x = title,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Title V/S Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 


full_titanic$ticket.size <- as.factor(full_titanic$ticket.size)
full_titanic$title <- as.factor(full_titanic$title)
