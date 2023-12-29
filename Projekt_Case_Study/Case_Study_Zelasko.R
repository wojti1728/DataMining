# wstepne biblioteki
library(tidyverse)
library(caret)
library(gbm)

# Import danych
setwd("D:/AGH-studia/Semestr_7/Data_Mininig/Projekt_Case_Study/education_data")
edu_data <- read.csv("Global_Education.csv")

# Analiza zawartości danych
dim(edu_data)
head(edu_data)
summary(edu_data)

# Sprawdzenie wystepowania wartości NA ( brak NA, za to wiele "0")
sum(is.na(edu_data))
colSums(is.na(edu_data))
dim(edu_data)
colSums(edu_data == 0, na.rm = TRUE) / nrow(edu_data) * 100


# Podwójna klasteryzacja danych w celu potwierdzenia niepoprawności danych w wielu kolumnach.
selected_columns<-c(12:25)
subset_edu_data<-edu_data[, selected_columns]
subset_edu_data
set.seed(123)
km.res <- kmeans(subset_edu_data, 4, nstart = 50)
km.res
dd <- cbind(edu_data, cluster_com = km.res$cluster)
head(dd)
dd[,c(1,29,30)]

# Próba klasteryzacji naszych danych
selected_columns<-c(4:11)
subset_edu_data<-edu_data[, selected_columns]
subset_edu_data
set.seed(123)
km.res <- kmeans(subset_edu_data, 4, nstart = 50)
km.res
dd <- cbind(dd, cluster_oos = km.res$cluster)
head(dd)
dd[,c(1,30)]

library(dplyr)
dd <- dd %>%
  mutate(cluster_level_up = case_when(
    cluster_com == 1 ~ "Bardzo Słaby",
    cluster_com == 2 ~ "Słaby",
    cluster_com == 3 ~ "Dobry",
    cluster_com == 4 ~ "Bardzo Dobry",
    TRUE ~ "Inny"
  ))

dd <- dd %>%
  mutate(cluster_level_low = case_when(
    cluster_oos == 1 ~ "Bardzo Słaby",
    cluster_oos == 2 ~ "Słaby",
    cluster_oos == 3 ~ "Dobry",
    cluster_oos == 4 ~ "Bardzo Dobry",
    TRUE ~ "Inny"
  )) 

head(dd)

# Odrzucenie niepoprawnych kolumn
selected_edu_data <- edu_data[, c(1,2,3,26,27,28,29)]

summary(selected_edu_data)
dim(selected_edu_data)
colSums(selected_edu_data == 0, na.rm = TRUE) / nrow(selected_edu_data) * 100

# wybór tylko znaczących kolumn do wykonania naszej klasteryzacji
cluster_colum<-c(1,4,5,6,7)
subset_edu_data<-selected_edu_data[, cluster_colum]
head(subset_edu_data)

# Pozbycie się wierszy które zaiwierały więcje niż jedną zerową wartość
zero_condition <- rowSums(selected_edu_data[, cluster_colum] == 0) <= 1
subset_edu_data_without_zeros <- selected_edu_data[zero_condition, ]
dim(subset_edu_data_without_zeros)
subset_edu_data_without_zeros

# Przeskalowanie danych
subset_edu_data_without_zeros[,c(4,5,6,7)] = scale(subset_edu_data_without_zeros[,c(4,5,6,7)])
subset_edu_data_without_zeros

edu_data_4cols <- subset_edu_data_without_zeros[,c(4,5,6,7)]
set.seed(123)
km.res <- kmeans(edu_data_4cols, 3, nstart = 20)
km.res

n_clusters <- 10
wss <- numeric(n_clusters)

set.seed(123)

for (i in 1:n_clusters) {
  km.out <- kmeans(edu_data_4cols, centers = i, nstart = 20)
  wss[i] <- km.out$tot.withinss
}

wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters') +
  ylab('Withinss')
scree_plot


scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )


k <- 5
set.seed(123)
km.out <- kmeans(edu_data_4cols, centers = k, nstart = 20)
km.out


clustered_edu_data <- cbind(subset_edu_data_without_zeros, economic_level = km.out$cluster)
clustered_edu_data[c(1,8)]

##################################################################################
# GBM model 1

set.seed(123)
train_index <- sample(seq_len(nrow(clustered_edu_data)), 0.8 * nrow(clustered_edu_data))
train_data <- clustered_edu_data[train_index, ]
test_data <- clustered_edu_data[-train_index, ]
test_data

gbm_model <- gbm(economic_level ~  Birth_Rate+Gross_Primary_Education_Enrollment+Gross_Tertiary_Education_Enrollment+Unemployment_Rate, data = train_data, n.trees = 500, interaction.depth = 3, shrinkage = 0.01, distribution = "gaussian")

summary(gbm_model)

predictions <- predict(gbm_model, newdata = test_data, n.trees = 500)
predictions

# Konwertuj predykcje na faktory (jeśli nie są)
test_data$economic_level <- as.factor(test_data$economic_level)

# sprawdzmy czy zgadzają się klastry
levels(predictions)
levels(test_data$economic_level)

predictions <- factor(round(predictions), levels = levels(test_data$economic_level))
levels(predictions)

# Utwórz macierz pomyłek
conf_matrix <- confusionMatrix(predictions, test_data$economic_level)
conf_matrix


##################################################################################
#kNN
#pakiet do modeli klasyfikacyjnych kNN
library(class)

head(train_data)
head(test_data)

model_3 <- knn(train = train_data[,4:7], test =test_data[,4:7], cl = train_data[,8], k=3)
summary(model_3)

confusionMatrix(model_3, as.factor(test_data$economic_level))

# próba wyboru najlepszej liczby sąsiadów
# Empty variables
KnnTestPrediction <- list()
accuracyKNN <- numeric()

for(k in 1:20){
  KnnTestPrediction[[k]] <- knn(train = train_data[,4:7], test =test_data[,4:7], cl = train_data[,8], k)
  accuracyKNN[k] <- sum(KnnTestPrediction[[k]]==test_data[,8])/length(test_data[,8])*100
}

plot(accuracyKNN, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")


abline(v=which(accuracyKNN==max(accuracyKNN)), col="darkorange", lwd=1.5)
abline(h=max(accuracyKNN), col="grey", lty=2)
abline(h=min(accuracyKNN), col="grey", lty=2)

# wykres wskazuje k=9 jako najlepszą liczbe klastrów do problemu

model_3 <- knn(train = train_data[,4:7], test =test_data[,4:7], cl = train_data[,8], k=9)
summary(model_3)

confusionMatrix(model_3, as.factor(test_data$economic_level))



