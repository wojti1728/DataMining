data("mtcars")

# w drugim kroku usuwamy tą zmienną która jest nie istotna statystycznie
# potem znowu sprawdzamy czy znowy cos odrzucic

# zadanie 2
# nls - dowolna postac funkcyjna modelu
# sami mamay dodac wspólczynniki itp
# musimy wskazac punkt startu
# jesli przedstawiamy zaleznosc, wykres rozrzutu, macierz korelacji
# za pomocja funkcji nls poprawic r^2 mpg


# wykonac zad 1 - poprawic model regresji liniowej


# zad 2 - bez zmiennych zewnetrznych - model edogenny


# zad 3 - wymodelowanie cene za jednostki powierzchi domu


#przyklad 1

model1<-lm(mpg~cyl+disp+wt+vs, data=mtcars)
summary(model1)

model1<-lm(mpg~cyl+disp+wt, data=mtcars)
summary(model1)

model1<-lm(mpg~cyl+wt, data=mtcars)
summary(model1)

#przyklad 2
library(corrplot)
library(dplyr)

corrplot(cor(mtcars))
mtcars = mtcars %>%
  mutate(cyl4 = (cyl==4))
  
#model2 <- nls(mpg ~ a0 + b1*disp^2+cyl4*b2, data = mtcars, start = list(a0 = 5, b1 = 5, b2=3))


max <- A <- B1 <- B2 <- B3 <- 0

for (i in -2:0.2:2)
{
  for (j in -2:0.2:2)
  {
    for (k in -2:0.2:2)
    {
      for (l in -2:0.2:2)
        model2 <- nls(mpg ~ a0 + b1*disp^2+b2*log(wt)+b3*cyl4, data = mtcars, start = list(a0 = i, b1 = j, b2=k, b3=l))

        a0 = summary(model2)$parameters[1][1]
        b1 = summary(model2)$parameters[2][1]
        b2 = summary(model2)$parameters[3][1]
        b3 = summary(model2)$parameters[4][1]

        model2_results<-data.frame(pred=a0+b1*mtcars$disp*mtcars$disp+b2*log(mtcars$wt)+mtcars$cyl4*b3,
                                   real=mtcars$mpg)

        model2_results<-model2_results %>%
          mutate(res=real-pred)
        model2_R2<-(cor(model2_results$real, model2_results$pred))^2
        
        if (model2_R2 > max)
          {
            max = model2_R2
            print("max")
            A = a0
            B1 = b1
            B2 = b2
            B3 = b3
        }
    }
  }
}
          
model2_results<-data.frame(pred=A+B1*mtcars$disp*mtcars$disp+B2*log(mtcars$wt)+mtcars$cyl4*B3,
                           real=mtcars$mpg)
model2_results<-model2_results %>%
  mutate(res=real-pred)
model2_R2<-(cor(model2_results$real, model2_results$pred))^2
A
B1
B2
B3
model2_R2


# zadanie 2

series_g <- read.csv('Series_G.csv', sep='\t',header = FALSE)
series_g
colnames(series_g) <- c('x','data')
series_g$data <- as.Date(series_g$data, format = "%d.%m.%Y")
series_g

# Sprawd� wyst�powanie trendu i sezonowo�ci
plot(series_g, type = 'l', ylab = 'Data', xlab = 'Liczba pasa�er�w')

# Dodaj zmienne informuj�ce o trendzie i sezonowo�ci
series_g$trend <- 1:length(series_g)
series_g$month <- as.numeric(format(series_g$data, "%m"))

# Utw�rz model regresji endogenicznej
model_lm <- lm(x ~ trend + month, data = series_g)

# Utw�rz model nieliniowy (przyk�adowa nieliniowa funkcja)
model_nonlinear <- nls(x ~ a * trend + b * sin(c * month), 
                       data = series_g, 
                       start = list(a = 1, b = 1, c = 1))

# Wykonaj predykcj�
n_obs <- nrow(series_g)
test_set <- series_g[(n_obs - 2):n_obs, ]

# Predykcja modelu regresji endogenicznej
predictions_lm <- predict(model_lm, newdata = test_set)

# Predykcja modelu nieliniowego
predictions_nonlinear <- predict(model_nonlinear, newdata = test_set)

# Oce� jako�� modelu
rmse_lm <- sqrt(mean((test_set$x - predictions_lm)^2))
rmse_nonlinear <- sqrt(mean((test_set$x - predictions_nonlinear)^2))

# Oce� jako�� reszt modelu
residuals_lm <- residuals(model_lm)
residuals_nonlinear <- residuals(model_nonlinear)

# Oce� jako�� predykcji
par(mfrow = c(2, 2))
plot(x = test_set$data, y = test_set$x, type = 'l', col = 'blue', lwd = 2, xlab = 'Data', ylab = 'Liczba pasa�er�w', )
legend('top', legend = c('Rzeczywiste'), col = c('blue'), lwd = 2)
plot(x = test_set$data, y = predictions_lm, type = 'l', col = 'red', lwd = 2,  xlab = 'Data', ylab = 'Liczba pasa�er�w')
legend('top', legend = c('Endogenna'), col = c('red'), lwd = 2)
plot(x = test_set$data, y = predictions_nonlinear, type = 'l', col = 'green', lwd = 2,  xlab = 'Data', ylab = 'Liczba pasa�er�w')
legend('top', legend = c('Nieliniowy'), col = c('green'), lwd = 2)

# Na koniec mo�esz wy�wietli� wyniki i metryki jako�ci
print(paste("RMSE dla modelu regresji endogenicznej: ", rmse_lm))
print(paste("RMSE dla modelu nieliniowego: ", rmse_nonlinear))

