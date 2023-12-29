
#Przyklad 1
table(iris$Species)
cor(iris[-5])

library(psych)
pairs.panels(iris[-5])

boxplot(Sepal.Length~Species, iris)

#własna funkcja do wykonania standaryzacji lub inaczej normalizacji z-score
stand <-function(x) { (x - mean(x))/(sd(x)) }
iris_std<- as.data.frame(lapply(iris[,c(1,2,3,4)],stand ))
iris_std_sp<-data.frame(iris_std, iris$Species)

#podzbiory
set.seed(123)
sets <- sample(1:nrow(iris), 0.75 * nrow(iris)) 
train_ir<-iris_std_sp[sets,]
test_ir<-iris_std_sp[-sets,]

#klasy poszczególnych obserwacji w podzbiorach
train_ir_class<-iris[sets,5]
test_ir_class<-iris[-sets,5]

#pakiet do modeli klasyfikacyjnych kNN
library(class)

#model 1a
model_ir3 <- knn(train = train_ir[, 1:4], test = test_ir[,1:4], cl = train_ir_class, k=3)
t_ir3<-table(Species=test_ir_class, Prediction=model_ir3)
acc_ir3<-mean(test_ir_class == model_ir3)

#kNN model regresyjny dla danych Boston z pakiety MASS
set.seed(123)
library(MASS)
library(caret)

#funkcja standaryzująca, zamiast niej może być np. scale()
stand <-function(x) { (x -mean(x))/(sd(x)) }
data("Boston")

sets <- sample(1:nrow(Boston), 0.75 * nrow(Boston)) 
train<-Boston[sets,]
train_std<- as.data.frame(lapply(train[,c(1:13)],stand ))
train_y<-train[,14]
test<-Boston[-sets,]
test_std<- as.data.frame(lapply(test[,c(1:13)],stand ))
test_y<-test[,14]
knnm1<- knnreg(train_std, train_y, k=5)
str(knnm1)

pred_y = predict(knnm1, data.frame(test_std))
mse = mean((test_y - pred_y)^2)
rmse = caret::RMSE(test_y, pred_y)
R2=(cor(test_y, pred_y))^2

#Zadanie 2
setwd("D:/AGH-studia/Semestr_7/Data_Mininig/cw5")

real_estate <- read.csv('Real_estate.csv')
summary(real_estate)
str(real_estate)

library(corrplot)
library(caret)
corr_matrix <- cor(real_estate)
corrplot(corr_matrix)

set.seed(123)
sample <- sample(1:nrow(real_estate), 0.95*nrow(real_estate))
train <- real_estate[sample, ]
test <- real_estate[-sample, ]

#model liniowy
plot(Y.house.price.of.unit.area ~ X3.distance.to.the.nearest.MRT.station, data = train)
linear_model <- lm(Y.house.price.of.unit.area ~ X3.distance.to.the.nearest.MRT.station, data = train)
linear_predict <- predict(linear_model, test)
linear_mse <- mean((test$Y.house.price.of.unit.area - linear_predict)^2)
linear_mse
linear_rmse <- RMSE(test$Y.house.price.of.unit.area, linear_predict)
linear_rmse
linear_R2 <- cor(test$Y.house.price.of.unit.area, linear_predict)^2
linear_R2

#model wieloraki
multi_model <- lm(Y.house.price.of.unit.area ~ X3.distance.to.the.nearest.MRT.station + X2.house.age, data = train)
multi_predict <- predict(multi_model, test)
multi_mse<- mean((test$Y.house.price.of.unit.area - multi_predict)^2)
multi_mse
multi_rmse <- RMSE(test$Y.house.price.of.unit.area, multi_predict)
multi_rmse
multi_R2 <- cor(test$Y.house.price.of.unit.area, multi_predict)^2
multi_R2

#model nieliniowy
nonlinear_model <- lm(Y.house.price.of.unit.area ~ X3.distance.to.the.nearest.MRT.station ^ 2, data = train)
nonlinear_predict <- predict(nonlinear_model, test)
nonlinear_mse<- mean((test$Y.house.price.of.unit.area - nonlinear_predict)^2)
nonlinear_mse
nonlinear_rmse <- RMSE(test$Y.house.price.of.unit.area, nonlinear_predict)
nonlinear_rmse
nonlinear_R2 <- cor(test$Y.house.price.of.unit.area, nonlinear_predict)^2
nonlinear_R2

#model knn
knn_model <- knnreg(train, train$Y.house.price.of.unit.area, k = 5)
knn_predict <- predict(knn_model, test)
knn_mse<- mean((test$Y.house.price.of.unit.area - knn_predict)^2)
knn_mse
knn_rmse <- RMSE(test$Y.house.price.of.unit.area, knn_predict)
knn_rmse
knn_R2 <- cor(test$Y.house.price.of.unit.area, knn_predict)^2
knn_R2


#Zadanie 3
names <- c("number_of_pregnant", "glucose", "blood_pressure", "triceps", "insulin", "BMI", "pedigree", "age", "class")
pima_indians <- read.delim("pima-indians-diabetes.txt", col.names = names, sep = ",", header = F)
summary(pima_indians)
str(pima_indians)

filtered_col <- c("number_of_pregnant", "glucose", "blood_pressure", "triceps", "insulin", "BMI", "pedigree", "age")
for (col in filtered_col){
  mean <- mean(pima_indians[[col]], na.rm = TRUE)
  std <- sd(pima_indians[[col]], na.rm = TRUE)
  lower <- mean - 3 * std
  upper <- mean + 3 * std
  pima_indians <- subset(pima_indians, pima_indians[[col]] >= lower & pima_indians[[col]] <= upper)
}
summary(pima_indians)

sample <- sample(1:nrow(pima_indians), 0.7 * nrow(pima_indians))
train <- pima_indians[sample, ]
test <- pima_indians[-sample,]

k_value <- as.integer(sqrt(length(train$class)))

knn_model <- knn(train = train, test = test, cl = train$class, k = k_value)

table <- table(original = test$class, Prediction = knn_model)
confusionMatrix(table)

TP <- sum(knn_model == "1" & test$class == "1")
TN <- sum(knn_model == "0" & test$class == "0")
FP <- sum(knn_model == "1" & test$class == "0")
FN <- sum(knn_model == "0" & test$class == "1")

accuracy <- (TP + TN) / (TP + FP + TN + FN)
accuracy

precision <- TP / (TP + FP)
precision

recall <- TP / (TP + FN)
recall
