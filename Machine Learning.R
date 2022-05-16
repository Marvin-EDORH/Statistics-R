rm(list = ls());
setwd("C:/Users/marvin.edorh_artefac/Desktop/Formation R")
library(dplyr)
install.packages("rpart.plot")
################################################ REGRESSION LINEAIRE ################################################ 

DATA <- read.csv("quanti.csv", sep=",",header = TRUE)

id <- DATA[,1]

DATA <-  DATA[,2:11]

DATA_ML <- data.frame( DATA$Users, DATA$Page_views, DATA$Campaign, DATA$Transactions)

rownames(DATA_ML) <- id

#install.packages("tidyverse")
library(tidyverse)

glimpse(DATA_ML)

plot(DATA_ML)

model <- lm( DATA.Transactions ~ DATA.Users , data = DATA_ML)
#Ce mod?le est-il bon ?
summary(model)$r.squared 
## [1] 0.9419583

new_data <- data.frame(DATA.Users = c(100, 1000, 5000, 10000))

prediction <- predict(model, new_data) %>%
  as.data.frame() %>%
  cbind(new_data, .)

prediction

##################################################### CLASSIFICATION ################################################ 

CLASSI <- read.csv("classification.csv", sep=",",header = TRUE)

#train <- CLASSI %>% sample_frac(0.6)
#test <- anti_join(CLASSI, train)

test <- read.csv("test_classi.csv", sep=",",header = TRUE)

library(rpart)
library(caret)
library(e1071)

#+ v2ProductCategory
#+ v2ProductCategory

tree <- rpart(Transactions ~ operatingSystem + visits + medium ,  
              data = CLASSI, method = "class", control=rpart.control(minsplit=0,cp=0))
            
test$prediction <- predict(tree, test, type = "class")
conf <- confusionMatrix(data = factor(test$prediction), reference = factor(test$Transactions))

#sensibilit? est la capacit? du mod?le ? pr?dire un positif quand la donn?e est r?ellement positive, 
#sp?cificit? est la capacit? du mod?le ? pr?dire un n?gatif lorsqu'il y a vraiment un n?gatif.

conf$byClass["Sensitivity"]
conf$byClass["Specificity"]


library(rpart.plot) # Pour la repr?sentation de l'arbre de d?cision

prp(tree, extra = 1)

#Elagage de l'arbre avec le cp optimal
tree_Opt <- prune(tree,cp=tree$cptable[which.min(tree$cptable[,4]),1])

#Repr?sentation graphique de l'arbre optimal
prp(tree_Opt, extra = 1)


################################################### K-MEANS ######################################################## 

DATA <- read.csv("test.csv", sep=",",header = TRUE)

test_kmeans <- DATA[,2:7]

id <- DATA[,1]

rownames(test_kmeans) <- id

ratio_ss <- data.frame(cluster = seq(from = 1, to = 10, by = 1)) 
for (k in 1:10) {
  km_model <- kmeans(test_kmeans, k, nstart = 1000)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss 
}

ggplot(ratio_ss, aes(cluster, ratio)) +  geom_line() + geom_point()

k_m <- kmeans (test_kmeans, 3, nstart = 1000)

id_cluster <- data.frame(k_m$cluster)
cluster_center <- data.frame(k_m$centers)
cluster_size <- data.frame(k_m$size)

test_kmeans$cluster <- k_m$cluster

ggplot(test_kmeans, aes(Transactions,Revenue, col = factor(cluster)))  + geom_point(size = 2, alpha = 0.8, position = "jitter")

k_m <- kmeans (test_kmeans, 3, nstart = 1000)

id_cluster <- data.frame(k_m$cluster)
cluster_center <- data.frame(k_m$centers)
cluster_size <- data.frame(k_m$size)

test_kmeans$cluster <- k_m$cluster

DATA$cluster <- k_m$cluster

write.csv(DATA,"data.csv", row.names = FALSE)

ggplot(test_kmeans, aes(Transactions,Revenue, col = factor(cluster)))  + geom_point(size = 2, alpha = 0.8, position = "jitter")

ggplot(test_kmeans, aes(x = Transactions)) + geom_histogram() + facet_wrap(~cluster)

ggplot(DATA, aes(x = Revenue, fill=cluster)) + geom_bar() 

############################################## RANDOM FOREST ####################################################### 


FOREST <- read.csv("ML.csv", sep=",",header = TRUE) 

install.packages("rpart.plot")

library(rpart)# Pour l'arbre de d?cision
library(rpart.plot) # Pour la repr?sentation de l'arbre de d?cision

#Cr?ation d'un dataset d'apprentissage et d'un dataset de validation
train <- CLASSI %>% sample_frac(0.8)
test <- anti_join(CLASSI, train)

#Construction de l'arbre
forest.Tree <- rpart(Transactions_2 ~ Device + medium +
                                      campaign + Page_views, 
                                      data=train,method='class', 
                                      control=rpart.control(minsplit=1,cp=0))

############################################# RANDOM FOREST ####################################################### 

lycee <- read.csv2("https://www.data.gouv.fr/s/resources/indicateurs-de-resultat-des-lycees-denseignement-general-et-technologique/20160401-163749/MEN-DEPP-indicateurs-de-resultats-des-LEGT-2015.csv", 
                   sep = ";", header = TRUE, fileEncoding = "ISO-8859-15", na.strings = c(" ", 
                                                                                          "."))

