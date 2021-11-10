# The gender model achieves a 76.6% leaderboard score
test <- read.csv("titanic_test.csv",stringsAsFactors=F)
test$Survived[test$Sex=='male'] <- 0
test$Survived[test$Sex=='female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit,"genderModel.csv",row.names=F)

train <- read.csv("titanic_train.csv",stringsAsFactors=F)
table(train$Survived[train$Sex=='male' & train$Age<16])
table(train$Survived[train$Sex=='female' & train$Pclass==3])



# engineer titles in training dataset
train$Title <- substring(train$Name,regexpr(",",train$Name)+2,regexpr("\\.",train$Name)-1)
train$Title[train$Title %in% c("Capt","Don","Major","Col","Rev","Dr","Sir","Mr","Jonkheer")] <- "man"
train$Title[train$Title %in% c("Dona","the Countess","Mme","Mlle","Ms","Miss","Lady","Mrs")] <- "woman"
train$Title[train$Title %in% c("Master")] <- "boy"
# engineer "woman-child-groups" for training dataset
train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
train$Surname[train$Title=='man'] <- 'noGroup'
train$SurnameFreq <- ave(1:891,train$Surname,FUN=length)
train$Surname[train$SurnameFreq<=1] <- 'noGroup'
# calculate "woman-child-group" survival rates
train$SurnameSurvival <- ave(train$Survived,train$Surname)
table(train$SurnameSurvival[train$Surname!='noGroup'])

# the following "woman-child-groups" all perish
x=train[train$SurnameSurvival==0,c("Surname")]; unique(x[order(x)])

# the following "woman-child-groups" all survive
x=train[train$SurnameSurvival==1,c("Surname")]; unique(x[order(x)])

# the following "woman-child-groups" have mixed survival
train[train$SurnameSurvival==1/7,c("Surname","Title","Survived")]
train[train$SurnameSurvival==1/3,c("Surname","Title","Survived")]
train[train$SurnameSurvival==3/4,c("Surname","Title","Survived")]


library(ggplot2)

# adjust survival rates for use on training set
train$AdjustedSurvival <- (train$SurnameSurvival * train$SurnameFreq - train$Survived) / (train$SurnameFreq-1)
# apply gender model plus new predictor to training set
train$predict <- 0
train$predict[train$Title=='woman'] <- 1
train$predict[train$Title=='boy' & train$AdjustedSurvival==1] <- 1
train$predict[train$Title=='woman' & train$AdjustedSurvival==0] <- 0
# plot how new predictor changes gender model


# Perform 25 trials of 10-fold cross validation
trials = 25; sum = 0
for (j in 1:trials){
  x = sample(1:890); s = 0
  for (i in 0:9){
    # Engineer "woman-child-groups" from training subset
    train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
    train$Surname[train$Title=='man'] <- 'noGroup'
    train$SurnameFreq <- ave(1:891,train$Surname,FUN=length)
    train$Surname[train$SurnameFreq<=1] <- 'noGroup'
    train$SurnameSurvival <- NA
    # calculate training subset's surname survival rate
    train$SurnameSurvival[-x[1:89+i*89]] <- ave(train$Survived[-x[1:89+i*89]],train$Surname[-x[1:89+i*89]])
    # calculate testing subset's surname survival rate from training set's rate
    for (k in x[1:89+i*89]) 
      train$SurnameSurvival[k] <- train$SurnameSurvival[which(!is.na(train$SurnameSurvival) & train$Surname==train$Surname[k])[1]]
    # apply gender model plus new predictor
    train$predict <- 0
    train$predict[train$Title=='woman'] <- 1
    train$predict[train$Title=='boy' & train$SurnameSurvival==1] <- 1
    train$predict[train$Title=='woman' & train$SurnameSurvival==0] <- 0
    c = sum(abs(train$predict[x[1:89+i*89]] - train$Survived[x[1:89+i*89]]))
    s = s + c
  }
  cat( sprintf("Trial %d has 10-fold CV accuracy = %f\n",j,1-s/890))
  sum = sum + 1-s/890
}
cat(sprintf("Average 10-fold CV accuracy from %d trials = %f\n",trials,sum/trials))


# engineer titles in test dataset
test$Title <- substring(test$Name,regexpr(",",test$Name)+2,regexpr("\\.",test$Name)-1)
test$Title[test$Title %in% c("Capt","Don","Major","Col","Rev","Dr","Sir","Mr","Jonkheer")] <- "man"
test$Title[test$Title %in% c("Dona","the Countess","Mme","Mlle","Ms","Miss","Lady","Mrs")] <- "woman"
test$Title[test$Title %in% c("Master")] <- "boy"
# engineer "woman-child-groups" for entire dataset
test$Survived <- NA; test$predict <- NA; train$AdjustedSurvival <- NULL
train$Surname <- NULL; train$SurnameFreq <- NULL; train$SurnameSurvival <- NULL
allData <- rbind(train,test)
allData$Surname <- substring(allData$Name,0,regexpr(",",allData$Name)-1)
allData$Surname[allData$Title=='man'] <- 'noGroup'
allData$SurnameFreq <- ave(1:1309,allData$Surname,FUN=length)
allData$Surname[allData$SurnameFreq<=1] <- 'noGroup'
# using only "Name" scores 0.81818, correcting surname groups with "Ticket" scores 0.82296
# search single woman and children and correct surname groups using Ticket
for (i in which(allData$Title!='man' & allData$Surname=='noGroup'))
  allData$Surname[i] = allData$Surname[allData$Ticket==allData$Ticket[i]][1]
allData$Surname[is.na(allData$Surname)] <- 'noGroup'
# calculate "woman-child-group" survival rates
allData$SurnameSurvival <- NA
allData$SurnameSurvival[1:891] <- ave(allData$Survived[1:891],allData$Surname[1:891])
for (i in 892:1309) allData$SurnameSurvival[i] <- allData$SurnameSurvival[which(allData$Surname==allData$Surname[i])[1]]
# apply gender model plus new predictor to test dataset
allData$predict <- 0
allData$predict[allData$Title=='woman'] <- 1
allData$predict[allData$Title=='boy' & allData$SurnameSurvival==1] <- 1
allData$predict[allData$Title=='woman' & allData$SurnameSurvival==0] <- 0
# plot predictions

# create CSV file to submit
submit <- data.frame(PassengerId = allData$PassengerId[892:1309], Survived = allData$predict[892:1309])
write.csv(submit,"genderSurnameModel.csv",row.names=F)

