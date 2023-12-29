library(rpart)
library(rpart.plot)
library(caret)
library(MASS)

setwd("D:/AGH-studia/Semestr_7/Data_Mininig/cw7")
diamonda <- read.csv('diamonds/diamonds.csv', sep='\t',header = FALSE)
diamonds
View(diamonds)

set.seed(123)

subset <- diamonds[sample(1:nrow(diamonds), size = 5200, replace = FALSE), ]

training <- subset[sample(1:nrow(subset), size = 5000, replace = FALSE), ]

test <- subset[sample(1:nrow(subset), size = 200, replace = FALSE), ]

selected_columns <- c("carat", "cut", "color", "clarity", "depth", "table", "price")

num_clusters <- 3

# bottom-up (hierarchiczna)

data_for_bottomup <- training[selected_columns]

data_for_bottomup$cut <- as.numeric(factor(data_for_bottomup$cut))

data_for_bottomup$color <- as.numeric(factor(data_for_bottomup$color))

data_for_bottomup$clarity <- as.numeric(factor(data_for_bottomup$clarity))

bottomup_model <- hclust(dist(data_for_bottomup))

clusters_bottomup <- cutree(bottomup_model, k = num_clusters)

table(clusters_bottomup)

# agglomerative (k-means)

data_for_agglomerative <- training[selected_columns]

data_for_agglomerative$cut <- as.numeric(factor(data_for_agglomerative$cut))

data_for_agglomerative$color <- as.numeric(factor(data_for_agglomerative$color))

data_for_agglomerative$clarity <- as.numeric(factor(data_for_agglomerative$clarity))

agglomerative_model <- pam(data_for_agglomerative, k = num_clusters)

clusters_agglomerative <- agglomerative_model$cluster

table(clusters_agglomerative)

# divisive

data_for_divisive <- training[selected_columns]

data_for_divisive$cut <- as.numeric(factor(data_for_divisive$cut))

data_for_divisive$color <- as.numeric(factor(data_for_divisive$color))

data_for_divisive$clarity <- as.numeric(factor(data_for_divisive$clarity))

divisive_model <- diana(data_for_divisive)

clusters_divisive <- cutree(divisive_model, k = num_clusters)

table(clusters_divisive)

# top-down

data_for_topdown <- training[selected_columns]

data_for_topdown$cut <- as.numeric(factor(data_for_topdown$cut))

data_for_topdown$color <- as.numeric(factor(data_for_topdown$color))

data_for_topdown$clarity <- as.numeric(factor(data_for_topdown$clarity))

topdown_model <- agnes(data_for_topdown)

clusters_topdown <- cutree(topdown_model, k = num_clusters)

table(clusters_topdown)

# Sprawdzenie bottomup

silhouette_bottomup <- silhouette(clusters_bottomup, dist(data_for_bottomup))

mean_silhouette_bottomup <- mean(silhouette_bottomup[, "sil_width"])

mean_silhouette_bottomup

# Sprawdzenie bottomup

silhouette_agglomerative <- silhouette(clusters_agglomerative, dist(data_for_agglomerative))

mean_silhouette_agglomerative <- mean(silhouette_agglomerative[, "sil_width"])

mean_silhouette_agglomerative

# Sprawdzenie divisive

silhouette_divisive <- silhouette(clusters_divisive, dist(data_for_divisive))

mean_silhouette_divisive <- mean(silhouette_divisive[, "sil_width"])

mean_silhouette_divisive

# Sprawdzenie bottomup

silhouette_topdown <- silhouette(clusters_topdown, dist(data_for_topdown))

mean_silhouette_topdown <- mean(silhouette_topdown[, "sil_width"])

mean_silhouette_topdown

#najlepszy jest agglomerative

#kmeans

kmeans_model_comparison <- kmeans(data_for_agglomerative, centers = num_clusters, nstart = 25)

kmeans_model_comparison

# Sprawdzenie kmeans

silhouette_kmeans <- silhouette(kmeans_model_comparison$cluster, dist(data_for_agglomerative))

mean_silhouette_kmeans <- mean(silhouette_kmeans[, "sil_width"])

mean_silhouette_kmeans

clusters_kmeans_comparison <- kmeans_model_comparison$cluster

table(clusters_kmeans_comparison)

