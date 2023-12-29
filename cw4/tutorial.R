data = data('titanic')
View(data)
library(gbm)
data("Titanic")

setwd("D:/AGH-studia/Semestr_7/Data_Mininig/cw4")

dane<-read.csv("titanic.csv")
dane <- subset(dane, select=c('X.pclass.', 'X.sex.', 'X.age.', 'X.survived.', 'X.parch.', 'X.sibsp.'))
colnames(dane) <- c('pclass', 'sex', 'age', 'survived', 'parch', 'sibsp')
dane$sex <- factor(dane$sex, levels=c("female", "male"))
dane$age <- as.integer(dane$age)
dane <- na.omit(dane)
dane <- subset(dane, select=c('pclass', 'sex', 'age', 'survived', 'parch', 'sibsp'))

indices <- sample(1:nrow(dane), 0.7*nrow(dane))
train_data <- dane[indices,]
test_data <- dane[-indices,]

#Trenowanie modelu GBM
library(gbm)
gbm_model <- gbm(survived ~ ., data = train_data, distribution = "bernoulli", n.trees = 1000, interaction.depth = 3, shrinkage = 0.01)
summary(gbm_model)

predict_data <- predict(gbm_model, newdata = test_data) 
predictBinaries <- as.factor(ifelse(predict_data>0.7, 1, 0))
test_data$survived <- as.factor(test_data$survived)
library(confusionMatrix)
confusionMatrix(predictBinaries, test_data$survived)

