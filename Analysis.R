#read the file
mydata <- read.xlsx("data/CleanedData.xlsx",header = T, sheetIndex =1)
print(str(mydata))


#change the type into factor type
mydata$Employed <- as.factor(mydata$Employed)
mydata$Mental.Illness <- as.factor(mydata$Mental.Illness)
mydata$Own.Computer <- as.factor(mydata$Own.Computer)
mydata$Disabled <- as.factor(mydata$Disabled)
mydata$Internet.Access <- as.factor(mydata$Internet.Access)
mydata$Live.With.Parents <- as.factor(mydata$Live.With.Parents)
mydata$Study <- as.factor(mydata$Study)
mydata$Receive.Food.Stamps <- as.factor(mydata$Receive.Food.Stamps)
mydata$Section.8.Housing <- as.factor(mydata$Section.8.Housing)
mydata$Lack.of.Concentration <- as.factor(mydata$Lack.of.Concentration)
mydata$Anxiety <- as.factor(mydata$Anxiety)
mydata$Depression <- as.factor(mydata$Depression)
mydata$Obsessive.Thinking <- as.factor(mydata$Obsessive.Thinking)
mydata$Mood.Swings <- as.factor(mydata$Mood.Swings)
mydata$Panic.Attacks <- as.factor(mydata$Panic.Attacks)
mydata$Compulsive.Behavior <- as.factor(mydata$Compulsive.Behavior)
mydata$Tiredness <- as.factor(mydata$Tiredness)
mydata$Gender <- as.factor(mydata$Gender)
mydata$Education<- as.factor(mydata$Education)
mydata$Age<- as.factor(mydata$Age)
mydata$Household.Income<- as.factor(mydata$Household.Income)
mydata$Device.Type<- as.factor(mydata$Device.Type)
print(str(mydata))

mydata[is.na(mydata$Study)|is.na(mydata$Depression),]
nrow(mydata)

xtabs(~ Employed + Study, data=mydata)       

unique(mydata$Education)
unique(mydata$Employed)

set.seed(10) 
split_index <- sample(2,nrow(mydata),replace = T , prob = c(0.8,0.2))
train_set <- mydata[split_index == 1,]
test_set <- mydata[split_index == 2,]

regression_function <- Employed ~ .
logistic_model <- glm(regression_function , data = train_set, family ='binomial')
summary(logistic_model)



regression <- Employed ~  Internet.Access + `Gaps.In.Resume(in.a.Month)` + Age + Disabled + Mental.Illness
logistic_model <- glm(regression , data = train_set, family ='binomial')
summary(logistic_model)

train_predict <- predict(logistic_model, train_set, type = 'response')
train_predict <- ifelse(train_predict >=0.5 , "TRUE" , "FALSE")
train_matrix <- table(Prediction = train_predict , Actual = train_set$Employed)
train_accuracy <- sum(train_predict == train_set$Employed)/nrow(train_set)*100

test_predict <- predict(logistic_model, test_set, type = 'response')
test_predict <- ifelse(test_predict >=0.5 ,  "TRUE" , "FALSE")
test_matrix <- table(Prediction = test_predict , Actual = test_set$Employed)
test_accuracy <- sum(test_predict == test_set$Employed)/nrow(test_set)*100

print(train_matrix)
print(train_accuracy)
print(test_matrix)
print(test_accuracy)














