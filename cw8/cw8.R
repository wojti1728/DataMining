setwd("C:/Users/msuro/Desktop/Studia/S7/Data Mining")
library(neuralnet)
library(corrplot)
library(pdp)

data <- pima
colSums(is.na(data))
data <- subset(data)
data <- data[!is.na(data$insulin),]
data <- data[!is.na(data$glucose),]
data <- data[!is.na(data$mass),]

stand <- function(x) { (x - mean(x) / sd(x) )  }
data_std <- as.data.frame(lapply(data[,c(2,3,4,5,6,7,8)], stand))
data_std$pregnant <- data$pregnant
data_std$diabetes <- data$diabetes


data_std$diabetes_bin <- ifelse(data_std$diabetes == "neg", 0, 1)

corrplot(cor(data_std[,c(1,2,3,4,5,6,7,8)]))

sets <- sample(1:nrow(data_std), 0.75 * nrow(data_std))
train_data <- data_std[sets,]
test_data <- data_std[-sets,]

model <- neuralnet(diabetes_bin ~ age + glucose, data = data_std, hidden = 3, linear.output = FALSE, rep = 5,act.fct = "logistic", threshold=0.005, err.fct="ce")

plot(model, rep="best")

pred <- predict(model, test_data)
rd_pred <- round(pred)

conf_matrix <- table(rd_pred, test_data$diabetes_bin)
print(conf_matrix)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

##
?read.csv()


data <- read.delim("Randki1.txt",header=TRUE, skip=1, sep='\t')
colSums(is.na(data))

data <- data[!is.na(data$Dzien_tyg),]
data <- data[!is.na(data$Liczba_logowan),]

data$Dzien_tyg <- as.factor(data$Dzien_tyg)
index <- createDataPartition(data$Liczba_logowan, p = 0.8, list = FALSE)
train_data <- data[index, ]
test_data <- data[-index, ]

model <- neuralnet(Liczba_logowan ~ Dzien_tyg, data = train_data, hidden = c(5, 3), linear.output = TRUE)