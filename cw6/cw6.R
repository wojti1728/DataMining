library(rpart)
#dla wykonanych podzbiorów danych:
data(iris)

set.seed(123)
sets <- sample(1:nrow(iris), 0.75 * nrow(iris)) 
stand <-function(x) { (x -mean(x))/(sd(x)) }
iris_std<- as.data.frame(lapply(iris[,c(1,2,3,4)],stand ))
iris_std_sp<-data.frame(iris_std, iris$Species)
train_ir<-iris_std_sp[sets,]
test_ir<-iris_std_sp[-sets,]
ir_cl1<-rpart(iris.Species~.,data=train_ir,method = 'class')
ir_cl1
summary(ir_cl1)

#wizualizacja drzewa
rpart.plot(ir_cl1, box.col=c("red", "green"))
#przewidywanie na zbiór testowy i ocena jakości
ir_cl1_pred<-predict(ir_cl1,newdata=test_ir[-5],type = 'class')
#zamiast funkcji table, można też wykonać macierz pomyłek za pomocą funkcji z pakietu caret
library(caret)

t_ir3<-table(Species=test_ir$iris.Species, Prediction=ir_cl1_pred)
t_ir3
confusionMatrix(ir_cl1_pred,test_ir$iris.Species)
         
printcp(ir_cl1)

opt <- which.min(ir_cl1$cptable[,'xerror'])
cp <- ir_cl1$cptable[opt, 'CP']
pruned_ir <- prune(ir_cl1,cp)
rpart.plot(pruned_ir, box.col=c("red", "green"))
ir_prun_pred<- predict(pruned_ir,newdata=test_ir[-5],type = 'class')
confusionMatrix(ir_prun_pred,test_ir$iris.Species)

# Drzewa klasyfikacyjne
library(rpart)
#dla wykonanych podzbiorów danych:
ir_cl1<-rpart(iris.Species~.,data=train_ir,method = 'class')
ir_cl1
print(ir_cl1)
summary(ir_cl1)                

#wizualizacja drzewa
rpart.plot(ir_cl1, box.col=c("red", "green"))
#przewidywanie na zbiór testowy i ocena jakości
ir_cl1_pred<- predict(ir_cl1,newdata=test_ir[-5],type = 'class')
#zamiast funkcji table, można też wykonać macierz pomyłek za pomocą funkcji z pakietu caret
library(caret)
confusionMatrix(ir_cl1_pred,test_ir$iris.Species)


#przycinanie drzewa, sprawdzenie czy nie zmieniły się wyniki klasyfikacji
printcp(ir_cl1)

opt <- which.min(ir_cl1$cptable[,'xerror'])
cp <- ir_cl1$cptable[opt, 'CP']
pruned_ir <- prune(ir_cl1,cp)
rpart.plot(pruned_ir, box.col=c("red", "green"))

ir_prun_pred<- predict(pruned_ir,newdata=test_ir[-5],type = 'class')
confusionMatrix(ir_prun_pred,test_ir$iris.Species)

#Drzewa regresyjne z rpart

library(rpart)
library(rpart.plot)
library(caret)
library(MASS)
bos<-Boston
dim(bos)
names(bos)

#Wykonanie podzbiorów
sets <- sample(1:nrow(bos), 0.90 * nrow(bos))
train_bos<-bos[sets,]
test_bos<-bos[-sets,]


#model
set.seed(10)
rt_bos <- rpart(medv~., data = train_bos, control = rpart.control(cp = 0.00001))
printcp(rt_bos)
                
#wykres drzewa przed przycinaniem
plot(rt_bos)
text(rt_bos, cex = 0.9, xpd = TRUE)

#przycinanie drzewa i jego wizualizacja
plotcp(rt_bos)

#chcemy uzyskać 8 węzłów
#cp: 0.0085790
rt_bos_pr<-prune(rt_bos, cp = 0.0085790)
plot(rt_bos_pr)
text(rt_bos_pr, cex = 0.9, xpd = TRUE)

rpart.plot(rt_bos_pr)

#wygenerowany model i reszty
rt_bos_pr_mod <-predict(rt_bos_pr)
R2<-cor(train_bos$medv, rt_bos_pr_mod)^2
rt_bos_pr_res<-train_bos$medv-rt_bos_pr_mod
#oceniamy założenia odnośnie reszt
#przewidywanie na zbiór testowy
rt_bos_pr_pred<- predict(rt_bos_pr, test_bos[,-14])
#oceniamy MAPE, THEIL, Janussowy      

# drzewa decyzyjne Random Forest
library(MASS)
library(randomForest)
#dane i ich przygotowanie jak w drzewie regresyjnym
#Model
set.seed(100)
?randomForest
rf_bos1 <- randomForest(medv~., data=train_bos, ntree=10, importance=TRUE, nodesize= 25)
#wyświetlenie informacji o drzewie
print(rf_bos1)

summary(rf_bos1)
#wizualizacja drzewa
plot(rf_bos1)

which.min(rf_bos1$mse)
## [1] 10
#uzyskanie informacji o wybranym drzewie
tree10<-getTree(rf_bos1, 10, labelVar=TRUE)
#przewidywanie na zbiór testowy na podstawie uśrednionego drzewa
rf_bos1_mod<-rf_bos1$predicted
#ocena jakości modelu:
#R^2,błąd standardowy estymacji
plot(train_bos$medv, type="p", col="red")
lines(rf_bos1_mod, type="p", col="blue")

#ocena jakości reszt:
#ocena czy reszty spełniają założenia procesu białoszumowego
#prognozy
pred_rf_bos1 <- predict(rf_bos1, test_bos[,1:13])
pred_rf_bos1

#ZADANIE 1

# Wczytaj dane
setwd("D:/AGH-studia/Semestr_7/Data_Mininig/cw6")
cols<-c('num_preg', 'gluc', 'diast_bp', 'tric_st', 'ins', 'bmi', 'ped', 'age', 'class')
pima_data<-read.delim("pima-indians-diabetes.txt", sep=',', header=F, col.names = cols)

# Podzial anych na zbir treningowy i testowy
set.seed(123)
sample_index <- sample(1:nrow(pima_data), 0.7 * nrow(pima_data))
train_pima <- pima_data[sample_index, ]
test_pima <- pima_data[-sample_index, ]

# Model drzewa klasyfikacyjnego
library(rpart)

pima_clf <- rpart(class ~ ., data = train_pima, method = 'class')

# Wyswietl informacje o drzewie
print(pima_clf)

library(rpart.plot)

# Wizualizacja drzewa
rpart.plot(pima_clf, box.col = c("red", "green"))

# Przewidywanie na zbiorze testowym
pima_pred <- predict(pima_clf, newdata = test_pima, type = 'class')

test_pima$class <- as.factor(test_pima$class)
levels(test_pima$class)

# Ocena jakosci modelu

library(caret)
# Przyklad konwersji na faktor, jezeli to konieczne
conf_matrix <- confusionMatrix(pima_pred, test_pima$class)
print(conf_matrix)


#ZADANIE 2

library(rpart)
library(rpart.plot)
library(caret)
library(MASS)
library(MLmetrics)
library(dplyr)

dates<-read.delim("Randki1.txt",skip=1, sep='\t', header=T)
dates <- na.omit(dates)
dates
dates <- dates %>%
  mutate(Weekend = ifelse(Dzien_tyg %in% c(6, 7), 1, 0))
dates

set.seed(123)

#Wykonanie podzbior�w
sets <- sample(1:nrow(dates), 0.8 * nrow(dates)) 
train_dates<-dates[sets,]
test_dates<-dates[-sets,]

rf <- randomForest(Liczba_logowan~., data=train_dates, ntree=100, importance=TRUE, nodesize= 1)
print(rf)
plot(rf)
min <- which.min(rf$mse)
min
tree <- getTree(rf, min, labelVar=TRUE)
rf_pred <- tree$prediction
rf_pred

plot(dates$Liczba_logowan, type="p", col="red")
lines(rf_pred, type="p", col="blue")

pred <- predict(rf, test_dates)

mae <- MAE(pred, test_dates$Liczba_logowan)
cat("Mean Absolute Error (MAE):", mae, "\n")

rmse <- RMSE(pred, test_dates$Liczba_logowan)
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

rsquared <- R2_Score(pred, test_dates$Liczba_logowan)
cat("R-squared:", rsquared, "\n")

summary(rf_pred)

############

