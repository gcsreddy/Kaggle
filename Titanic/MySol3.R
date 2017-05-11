setwd("~/workspace/Datascience/Kaggle/Titanic")
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)

test$Survived <- NA
train$Survived <- as.factor(train$Survived)
test$Survived <- as.factor(test$Survived)

train$IsTrainingSet <- TRUE
test$IsTrainingSet <- FALSE

full <- rbind(train,test)
full[full$Embarked=='',"Embarked"]<-'S'

full$Child <- 0
full$Child[full$Age<19] <- 1

medianAge <- median(full$Age, na.rm = TRUE)
full[is.na(full$Age),"Age"] <- medianAge

#medianFare <- median(full$Fare, na.rm = TRUE)
#full[is.na(full$Fare),"Fare"] <- medianFare
aggrFarePerClassDf <- aggregate(Fare ~ Pclass, data = full, FUN = function(x){sum(x)/length(x)})
full$Fare[is.na(full$Fare) && full$Pclass == 1] <- aggrFarePerClassDf$Fare[1]
full$Fare[is.na(full$Fare) && full$Pclass == 2] <- aggrFarePerClassDf$Fare[2]
full$Fare[is.na(full$Fare) && full$Pclass == 3] <- aggrFarePerClassDf$Fare[3]

full$Fare2 <- "35+"
full$Fare2[full$Fare<15] <- "15"
full$Fare2[full$Fare>=15 & full$Fare < 25] <- "25"
full$Fare2[full$Fare>=25 & full$Fare < 35] <- "35"

full$Pclass <- as.factor(full$Pclass)
full$Sex <- as.factor(full$Sex)
full$Embarked <- as.factor(full$Embarked)
full$Fare2 <- as.factor(full$Fare2)

train <- full[full$IsTrainingSet == TRUE,]
test <- full[full$IsTrainingSet == FALSE,]

train$Survived <- as.factor(train$Survived)

equation <- "Survived ~ Child + Sex + Fare2 + Pclass"
SurvivedFormula  <- as.formula(equation)

#install.packages("randomForest")
library(randomForest)

SurviedModel <- randomForest(formula = SurvivedFormula, data = train)

featuresEquation <- "Child + Sex + Fare2 + Pclass"

Survived <- predict(SurviedModel, newdata = test)


PassengerId <- test$PassengerId

outputData <- as.data.frame(PassengerId)
outputData <- cbind(outputData,Survived)

write.csv(outputData,"kaggle_submission3.csv",row.names = FALSE)

#aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x){sum(x)/length(x)})