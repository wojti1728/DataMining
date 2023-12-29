
library(liver)
data("cereal")

library(corrplot)
cor_matrixp<-round(cor(cereal[4:16]),2)

boxplot(cereal$rating)
boxplot(cereal$rating~cereal$type)


# Analiza rozkładu danych
hist(cereal$rating, main="Rozkład ocen", xlab="Ocena")

# Wykresy ramka-wąsy ze zmienną grupującą
boxplot(cereal$rating ~ cereal$type, main="Oceny w zależności od typu", xlab="Typ", ylab="Ocena")

# Macierze korelacji
cor_matrix_linear <- cor(cereal[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", "potass", "vitamins", "shelf", "weight", "cups", "rating")])
cor_matrix_nonparametric <- cor(cereal[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", "potass", "vitamins", "shelf", "weight", "cups", "rating")], method = "spearman")

# Wykresy korelacyjne
library(corrplot)
corrplot(cor_matrix_linear, method="color")
corrplot(cor_matrix_nonparametric, method="color")

# Wykresy rozrzutu
pairs(cereal[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", "potass", "vitamins", "shelf", "weight", "cups", "rating")])

# Wybór zmiennych do modelu
# Aby wybrać zmienne do modelu regresyjnego, możesz użyć różnych metod, takich jak analiza korelacji, selekcja zmiennych za pomocą LASSO lub regularyzacja grzbietowa. Wybór zależy od celu analizy.

# Przykład regresji liniowej
# Jeśli chcesz przeprowadzić regresję liniową na ocenach, możesz wybrać zmienne, które wykazują silne korelacje z ocenami. Na przykład:
model <- lm(rating ~ protein + fiber + vitamins, data=cereal)
summary(model)

# Możesz eksperymentować z różnymi zestawami zmiennych i metodami modelowania, aby znaleźć najlepszy model regresyjny.

# Oblicz macierz korelacji rang Spearmana
cor_matrix_spearman <- cor(cereal[, c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", "potass", "vitamins", "shelf", "weight", "cups", "rating")], method = "spearman")

# Wykres korelacyjny dla macierzy korelacji rang Spearmana
library(corrplot)
corrplot(cor_matrix_spearman, method="color", type="upper", tl.col="black", tl.srt=45)


# Zadanie 1

setwd("D:/AGH-studia/Semestr_7/Data_Mininig/cw1")
data <- read.csv("Real_estate.csv")
str(data)
summary(data)
View(data)

#Preprocessing
data <- data[, -which(names(data) == "No")]
hist(data$Y.house.price.of.unit.area)
boxplot(data$X4.number.of.convenience.stores, data$Y.house.price.of.unit.area)
cor(data$X2.house.age, data$Y.house.price.of.unit.area, method='pearson')

plot(data$X2.house.age, data$Y.house.price.of.unit.area)

cor_mat_s<-round(cor(data, method="spearman"),2)
cor_mat_p<-round(cor(data),2)
corrplot(cor_mat_s)
