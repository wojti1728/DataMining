# Zadanie 1

setwd("D:/AGH-studia/Semestr_7/Data_Mininig/cw2")
wastewater <- read.table("wastewater.txt",header = TRUE, sep = "\t")
# Ręczna konwersja pierwszej kolumny na datę
wastewater$X <- as.Date(wastewater$X, format = "%Y.%m")

str(wastewater)
summary(wastewater)
View(wastewater)

szereg_czasowy<-ts(data = wastewater$Sandomierz, frequency = 12, start=c(2002,1), end = c(2007,06))
szereg_czasowy
str(szereg_czasowy)
summary(szereg_czasowy)
# parzsyty indeks to bardziej rygorystyczny 0-7.5 (wartosci prawidłowe)
View(szereg_czasowy)
boxplot(wastewater$Sandomierz)
plot(wastewater$Sandomierz, type="l")
hist(wastewater$Sandomierz, breaks=8)

library(dplyr)
wastewater <- wastewater %>% 
  mutate(a_Sandomierz = ifelse(Sandomierz < 0 | Sandomierz > 7.5, NA, Sandomierz))
wastewater

boxplot(wastewater$a_Sandomierz)
plot(wastewater$a_Sandomierz, type="l")
hist(wastewater$a_Sandomierz, breaks=8)

par(mfrow=c(2,1))
acf(szereg_czasowy, na.action = na.pass)

# zad 4
cars <- read.table("cars.txt",header = TRUE, sep = ",")
cars$cubicinches <- as.numeric(cars$cubicinches) 
str(cars)
summary(cars)
View(cars)

#histogramy
par(mfrow=c(3,3))
hist(cars$cylinders)
hist(cars$hp)
hist(cars$mpg)
hist(cars$cubicinches)
hist(cars$weightlbs)
hist(cars$time.to.60)
hist(cars$year)

# na podstawie własnej wiedzy oraz danych znalezionych w internecie przeprowadzamy westępny preprocessing wraz z usunieciem 
# wartości odstających

cars = cars %>%
  mutate(mpg2 = ifelse(cars$mpg < 0 | cars$mpg > 70, NA, cars$mpg),
         cylinders2 = ifelse(cars$cylinders > 8, NA, cars$cylinders),
         cubicinches2 = ifelse(cars$cubicinches > 400, NA, cars$cubicinches),
         hp2 = ifelse(cars$hp > 300, NA, cars$hp))
cars<-distinct(cars)

cars$mpg2 = na.approx(cars$mpg2, rule=2)
cars$cylinders2 = na.approx(cars$cylinders2, rule=2)
cars$cubicinches2 = na.approx(cars$cubicinches2, rule = 2)
cars$hp2 = na.approx(cars$hp2, rule = 2)


View(cars)

par(mfrow=c(3,3))
hist(cars$cylinders2)
hist(cars$hp2)
hist(cars$mpg2)
hist(cars$cubicinches2)

sum_sd<-cars %>%
  group_by(cars$brand)
summary(sum_sd)
