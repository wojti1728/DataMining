library(liver)
data("cereal")


Wybór celowy
train<-cereal[-c(5, 15, 25, 35, 55),]
test<-cereal[c(5, 15, 25, 35, 55),]


sets <- sample(1:nrow(cereal), 0.9 * nrow(cereal))
train2<- cereal[sets,]
test2<- cereal[-sets,]

#model 1: Regresja liniowa
m1<-lm(rating~sugars, train2)
summary(m1)

# zwracamy uwage na poprawnosc modelu (czyli jest istotny statystyczny)
# Od coeffitians:
# nasz model jest istotny statystycznie to patrzmym na ilosc 
# gwiazdek na koniec suggars (jesli jedna to juz git, im wiecej tym lepiej)
# jesli musimy usunac jakies zmienne, to wtedy usuwamy z modelu te z najwiekszym z p
# Rating = 59.34-2.07 * sugars (do przewidywania w przyszlosci)
# RSE (o ile średnio się mylimy)
# R-squared - wspołczynnik determinacji który tłumaczy nam jak dobry jest nasz model
# 
plot(train2$rating, type="l", col="red")
lines(m1$fitted.values, type="l", col="blue")

#kryterium porównawcze z innymi modelami
aic_m1<-AIC(m1)
bic_m1<-BIC(m1)
#reszty
hist(m1$residuals)

acf(m1$residuals)

pacf(m1$residuals)

#prognozy
library(MLmetrics)
##
## Dołączanie pakietu: 'MLmetrics'
## Następujący obiekt został zakryty z 'package:base':
##
## Recall
pred_m1<-predict(m1, newdata = test2)
mape_m1<-MAPE(pred_m1, test2$rating)
mape_m1

aic_m1<-AIC(m1)
aic_m1
bic_m1<-BIC(m1)
bic_m1
# w kolejnych krokach wyliczane są współczynniki Theila i wsp. Janussowy

plot(m1$fitted.values,m1$residuals)

aic_m1<-AIC(model)
aic_m1



pred_model<-predict(model, newdata = test_data)
pred_model
mape_model<-MAPE(pred_model, test_data$rating)
mape_model

# zad 2
# analiza danych
# czy tam jest red czy jest tam sezonowoac, jakie dane mozna dodac
# obczaic funkcje lag()


# zad 1
View(cereal)

n_test<-5
sets<-sample(1:nrow(cereal), n_test)

test_data<-cereal[sets,]
train_data<-cereal[-sets,]

v<-cereal[,4:ncol(cereal)]
cor_matrix<-cor(v)
corrplot(cor_matrix)

# model
model<-lm(rating ~ sugars + fiber + fat + sodium, data = train_data)

# ocena jakosci modelu
summary(model)

par(mfrow = c(1,1))
plot(train_data$ratin, type='l', col='red')
lines(model$fitted.values, type='l', col='blue')

par(mfrow=c(2,2))
hist(model$residuals)
acf(model$residuals)
pacf(model$residuals)


plot(model$residuals~model$fitted.values)
par(mfrow = c(1, 1))


# zad 2
setwd("D:/AGH-studia/Semestr_7/Data_Mininig/cw3/")

data <- read.csv('Series_G.csv')
air_passangers<-read.table("Series_G.csv",header = FALSE, sep = "\t")
air_passangers

szereg_czasowy<-ts(data = air_passangers$V1, frequency = 12, start=c(1949,1), end = c(1960,12))
szereg_czasowy

# wspolczynniki od nachylenia
# zmienna decyduje o kształcie

