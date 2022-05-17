rm(list = ls());
setwd("C:/Users/marvin.edorh_artefac/Desktop/Formation R")

###################################################################### fonction aggrégée  ###################################################

#install.packages("readxl")
library(readxl)
DATA_1 <- as.data.frame(read_excel("projet_Airline_small pour WA.xlsx"))

median(DATA_1$Age)
min(DATA_1$Age)
sum(DATA_1$Age)
mean(DATA_1$Age)

DATA_groupby <- aggregate(Age ~ Gender + `Satisfaction Top2`, DATA_1, FUN=min)

#install.packages("plyr")
library(plyr)
DATA_summarize <- ddply(.data = DATA_1, .variables = .(Gender), .fun = summarize, min = min(Age), moy = mean(Age), max=max(Age))

DATA_mean <- aggregate(DATA_1$Age, by=list(Category=DATA_1$Gender), FUN=mean)
DATA_min <- aggregate(Age ~ Gender, DATA_1, FUN=min)
DATA_max <- aggregate(Age ~ Gender, DATA_1, FUN=max)
DATA_sum <- aggregate(Age ~ Gender, DATA_1, FUN=sum)
DATA_median <- aggregate(Age ~ Gender, DATA_1, FUN=median)

colnames(DATA_mean)[2] <- "mean"
colnames(DATA_min)[2] <- "min"
colnames(DATA_max)[2] <- "max"
colnames(DATA_sum)[2] <- "sum"
colnames(DATA_median)[2] <- "median"

DATA_DATA_summarize_2 <- merge(merge(merge(merge(DATA_mean,DATA_min), DATA_max),DATA_sum),DATA_median)

###################################################################### graphiques ##########################################################

barplot(table(DATA_1$Gender))
pie(table(DATA_1$Gender))
hist(table(DATA_1$Age))
boxplot(DATA_1$Age~DATA_1$Gender)
plot(DATA_1$Satisfaction~DATA_1$Age)

#install.packages("ggplot2")
library(ggplot2)

# synthaxe ggplot2 : 
# - fonction ggplot
# - nom de la table
# - virgule
# - parametre aes 
# - + fonction geom correspondant au type de graphique souhaité

graph_1 <- ggplot(DATA_1, aes(x=Gender, y=Age)) 

#box plot
box_plot <- graph_1 + geom_boxplot () #on ajoute à la fonction ggplot le graphique souhaité ici box plot
box_plot + labs (x = "sexe", y = "âge", title = "box plot") #on ajoute maintenant titre axe et graphique
graph_1 + geom_boxplot (outlier.colour="red", outlier.shape=8, outlier.size=4) + stat_summary(fun=mean, shape=18, size=1)
graph_1 + geom_boxplot (color = 'blue') #on ajoute de la couleur
graph_1 + geom_boxplot (aes(color = Gender)) + stat_summary(fun=mean) #on modifie la couleur en foncftion de la légende
graph_1 + geom_boxplot (shape = 1) #on modifie la forme
graph_1 + geom_boxplot (size = 1) #on modifie la taille
graph_1 + geom_boxplot (size = 1, width = 0.1) #on modifie l'écart
graph_1 + geom_boxplot () + scale_y_continuous(breaks = seq(0, 100, by = 10)) #on modifie l'echelle de l'axe

#nuage de points 
graph_2 <- ggplot(DATA_1, aes(x=Age, y=Satisfaction, color = Gender)) 
graph_2 <- ggplot(DATA_1, aes(x=Age, y=`Flight Distance`)) 
graph_2 + geom_point()
graph_2 + geom_point(shape = 2)
graph_2 + geom_jitter(position = position_jitter(width = 0.4)) 
graph_2 + geom_point() + scale_x_continuous(breaks = seq(min(DATA_1$Age), max(DATA_1$Age), by = 10))
graph_2 + geom_point() + geom_smooth(linetype ='dashed') #on ajoute courbe de regression
graph_2 + geom_point() + geom_smooth(method = 'lm',se = FALSE)  #modele linéaire, courbe droite
graph_2 + geom_point() + geom_smooth(method = 'lm', se = FALSE) + facet_grid(.~Gender, scales ='free_x') #on split en 2 graphe
graph_2 + geom_point() + geom_smooth(method = 'lm',se = FALSE) + facet_grid(Gender~., scales ='free_y') #on split en 2 graphe
graph_2 + geom_point() + geom_smooth(method = 'lm',se = FALSE) + facet_grid(Gender~., scales ='free_y') + theme_bw()
ggplot(DATA_1, aes(x = `Arrival Delay in Minutes`, y = Satisfaction )) +geom_point() + geom_smooth(method = "lm", se = FALSE)

#graphique en barre
graph_3 <- ggplot(DATA_1, aes(x=Gender,color = Gender)) #couleur contour
graph_3 + geom_bar(fill ='black') #couleur remplissage
ggplot(DATA_1, aes(x = Class)) + geom_bar() + facet_wrap(~ Gender)

#histogrammme
graph_4 <- ggplot(DATA_1, aes(x=Age, color = Gender))
graph_4 + geom_histogram(fill ='black', bins = 10) #25 barres
graph_4 + geom_histogram(fill ='black', bins = 10, position = 'dodge') #25 barres
graph_4 + geom_histogram(fill ='white', bins = 10) + facet_grid(Gender~.) + geom_vline(aes(xintercept = mean (Age, na.rm = TRUE)))

#densité
graph_5 <- ggplot(DATA_1, aes(x=Age))
graph_5 + geom_histogram(aes(color =Gender), fill ='white') + geom_density(color='blue')

#diagramme circulaire
graph_6 <- ggplot(DATA_1, aes(x='', y = Age, fill = Gender)) 
graph_6 + geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) #numerique
ggplot(DATA_1, aes(x=factor(1), fill=Gender)) + geom_bar(width = 1) + coord_polar("y") #compte observatiion

########################################################## KHI-2 ##################################################################

library(sqldf)
x <- sqldf("SELECT Gender, `Airline status`, COUNT (*) as count FROM DATA_1 GROUP BY Gender, `Airline status`")
x_1 <- sqldf("SELECT Gender, COUNT (*) as count FROM DATA_1 GROUP BY Gender")
x_2 <- sqldf("SELECT x.Gender, `Airline status`,x.count as count_1 , x_1.count as count_2 FROM x LEFT JOIN x_1 USING (Gender)")
library(dplyr)
x_3 <- x_2 %>% mutate(percent_Gender = count_1/count_2)
graph_7 <- ggplot(x_3, aes(x=Gender, y = percent_Gender))
graph_7 + geom_col(aes(fill = `Airline status`))

table(DATA_1$Gender,DATA_1$`Airline status`)

prop.table(table(DATA_1$Gender,DATA_1$`Airline status`))

prop.table(table(DATA_1$Gender,DATA_1$`Airline status`),1) #1:ligne, 2:colonne

summary(table(DATA_1$Gender,DATA_1$`Airline status`))

###################################################### Corrélation ################################################################

Zagat <- read.table("zagat_all+Nation.txt", header = TRUE, sep = "\t", dec = ".") #sep =tabuation, decimale par un point
mcor <- cor(Zagat[,3:6])
mcor

# Significativité

#install.packages("Hmisc")
library(Hmisc)
rcorr(as.matrix(Zagat[,3:6]),type=c("pearson","spearman"))


# plot des correlations
#install.packages("corrplot")
library(corrplot)
corrplot(cor(Zagat[,3:6]), method = "circle")
corrplot(cor(Zagat[,3:6]), method = "ellipse",type="lower")

####################################################### Anova #####################################################################

nsw <- read.csv("nsw.csv", sep=";",header = TRUE)

summary(anovanswre75<-aov(nsw$re75 ~ nsw$treat))

#############################################  Regression linéaire ################################################################

library(readxl)

summary(lm (formula = DATA_1$`Satisfaction` ~ DATA_1$`Age` 
                                            + DATA_1$`Price Sensitivity`
                                            + DATA_1$`Flight time in minutes`
                                            + DATA_1$`Flight Distance`
                                            + DATA_1$`Arrival Delay in Minutes`
                                            + DATA_1$`Departure Delay in Minutes`))

#############################################  Modele linéaire généralisé #########################################################

#  Par défaut la réference est prise en fonction de l'ordere alphabétique

summary(glm(DATA_1$Satisfaction ~ DATA_1$Gender  
                                + DATA_1$Class
                                + DATA_1$`Flight cancelled`
                                + DATA_1$`Type of Travel`
                                + DATA_1$`Arrival Delay greater 5 Mins`))

############################################# Regression Logistique ##############################################################
#install.packages("dplyr")
library(dplyr)

DATA_1 <- DATA_1 %>% mutate(`Satisfaction Top2` = case_when(`Satisfaction Top2` == 'yes'~ 1,`Satisfaction Top2` == 'no' ~ 0))

reglog <- glm(`Satisfaction Top2` ~ Gender + Class + `Flight cancelled` + `Type of Travel` + `Arrival Delay greater 5 Mins`, DATA_1, family = binomial(logit))

summary(reglog)

########################################################## ACM ####################################################################

acm <- read.csv ("acm.csv", sep=";",header = TRUE)

active <- acm[,2:25] # fichier intégrale 

rownames(active) <- acm[,1]

install.packages("FactoMineR")
library(FactoMineR)

mca <- MCA (active, ncp = 6 , graph = FALSE)#Par defaut npc = 5, refaire tourner après analyse de l'inertie avec le bon nb d'axes

install.packages("factoextra")

library(factoextra)

info <- get_eigenvalue(mca) #SELECTIONNER LES AXES DONT L'INERTIE (info : eigenvalue) est superieure ? 1/NB VARIABLES ACTIVES

#part de l'inertie
fviz_screeplot (mca, addlabels = TRUE, ylim = c (0, 100))

var <- get_mca_var(mca)

# Coordonnées des variable
coordonnees <- var$coord

# Contributions aux axes des variables
Contributions <- var$contrib

#Interpreter les axes en regardant quel var contribue le plus, le signe de la var et sa contribution donne le sens de l'axe

fviz_mca_var (mca,
              repel = TRUE, 
              ggtheme = theme_minimal ())

# Contributions des variables à la dimension 1
fviz_contrib (mca, choice = "var", axes = 1, top = 24)
# Contributions des variables à la dimension 2
fviz_contrib (mca, choice = "var", axes = 2, top = 24)
# Contributions des variables à la dimension 3
fviz_contrib (mca, choice = "var", axes = 3, top = 24)
# Contributions des variables à la dimension 4
fviz_contrib (mca, choice = "var", axes = 4, top = 24)
# Contributions des variables à la dimension 5
fviz_contrib (mca, choice = "var", axes = 5, top = 24)
# Contributions des variables à la dimension 6
fviz_contrib (mca, choice = "var", axes = 6, top = 24)

ind <- get_mca_ind (mca)
ind

# Coordonnées
coordonnees_ind <- ind$coord

# Contributions
Contributions_ind <- ind$contrib

#fviz_mca_ind(mca, col.ind = "cos2", 
#gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
# repel = TRUE, 
# ggtheme = theme_minimal())

################################################## Classification-Mixte ############################################################

#####################################################  1 - K-Means #################################################################

k_m <- kmeans (coordonnees_ind, 100, nstart = 5)

library(factoextra)

print(k_m) 

Classes_k_m <- data.frame(k_m$cluster)
Barycentres_k_m <- data.frame(k_m$centers)
Taille_k_m <- data.frame(k_m$size)

######################################################2 - CAH ####################################################################

cah <- HCPC ( Barycentres )

Classes_cah <- data.frame(cah$data.clust)

cah_descvar <- cah$desc.var

cah_descvar

######################################################  3 - K-Means ###############################################################

k_m <- kmeans (coordonnees_ind, 5 , nstart = 5)

################################################ Préparation des données ##########################################

GRDF <- read.csv("GRDF_clustering_2.csv", sep=",",header = TRUE) #on crée le dataset "GRDF" ? partir du fichier CSV

GRDF_kmeans_MOIS <- data.frame(GRDF$clics_moyen_egazpar_mois,
                               GRDF$clics_moyen_suivi_conso_mois, GRDF$clics_moyen_ereleve_mois,
                               GRDF$clics_moyen_tel_conso_mois, GRDF$clics_moyen_seuil_mois)

#on crée un dataset à partir des KPI qui nous interesse pour le clustering (variables num?riques)

id <- GRDF[,1] #on stocke les éléments de la première colonne de la table GRDF dans l'objet "id"

rownames(GRDF_kmeans_MOIS) <- id #on donne comme identifiant de chaque ligne les éléments de l'objet "id"


#################################################### ACP ############################################################

#Le but de l'acp est transformer les variables num?riques en axe factoriels pour réunir sur le même  axe des varibles 
#corrélées

library("FactoMineR") #on charge les packages pour l'ACP

res.pca <- PCA(GRDF_kmeans_MOIS, graph = FALSE) #on effectue l'ACP 

#Valeur propres, selectionner les axe dont les valeurs propres sont superieurs à 1 (methode de kaiser)

library("factoextra") #on charge les packages pour obtenir les valeurs propre de l'ACP
eig.val <- data.frame(get_eigenvalue(res.pca)) #stocke les valeurs propres de chaque axes de l'acp

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) #on modélise la part de l'information de chaque dimension

eig.val #on visualise les valeurs propres, il faut conserver les dimmension dont la valeurs propres > 1 

#coordonnées des individus sur les axes

individus <- get_pca_ind(res.pca)

coord_ind_ACP <- individus$coord

################################################ Clustering #######################################################

GRDF_kmeans <- coord_ind_ACP[,1:5] #on charge dans une table les colonnes 1 ? 5 de la table coord_ind_ACP

########################################### choix du nombre de cluster ############################################

ratio_ss <- data.frame(cluster = seq(from = 1, to = 10, by = 1)) #on crée une table de 1 à 10 clusters

for (k in 1:10) {
  km_model <- kmeans(GRDF_kmeans, k, nstart = 100)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss 
} #on effectue 10 clustering à 100 itérations pour voir lequel est le plus optimal

library(ggplot2) #on charge le packages pour modéliser la courbe d'elbow 

ggplot(ratio_ss, aes(cluster, ratio)) +  geom_line() + geom_point() #on modélise la courbe d'elbow

##################################################### Clustering ##################################################

k_m <- kmeans (GRDF_kmeans, 6, nstart = 1000) #on effectue un clustering avec le nombre de clusters optimal

GRDF_cluster_size <- data.frame(k_m$size) #taille de chaque cluster

GRDF$cluster <- k_m$cluster #ajoute la variable cluster à la table GRDF d'origine

library(dplyr)

GRDF <- GRDF %>%
  mutate(
    cluster = case_when(
      cluster == 3 ~ "Cluster 1",
      cluster == 5 ~ "Cluster 2",
      cluster == 4 ~ "Cluster 3",
      cluster == 2 ~ "Cluster 4",
      cluster == 1 ~ "Cluster 5",
      cluster == 6 ~ "Cluster 6",
    )
  )

write.csv(GRDF,"GRDF_clusters.csv", row.names = FALSE) #on exporte la table GRDF pour l'analyser dans Data studio

############################################# Deuxieme clustering #################################################

GRDF <- read.csv("GRDF_clustering_2.csv", sep=",",header = TRUE) #on crée le dataset "GRDF" à partir du fichier CSV

GRDF_kmeans_MOIS_2 <- data.frame(GRDF$clics_egazpar,
                                 GRDF$clics_suivi_conso,
                                 GRDF$clics_ereleve,
                                 GRDF$clics_tel_conso,
                                 GRDF$clics_seuil)
#on crée un dataset à partir des KPI qui nous interesse pour le clustering (variables textuelles)

id <- GRDF[,1]


rownames(GRDF_kmeans_MOIS_2) <- id

######################################################### ACM ####################################################

library(FactoMineR) #On charge les packages pour l'acm

mca <- MCA (GRDF_kmeans_MOIS_2, ncp = 6, graph = FALSE)

#Par defaut npc = 5, refaire tourner après analyse des valeurs propres avec le bon nb d'axes

library(factoextra)

fviz_screeplot (mca, addlabels = TRUE, ylim = c (0, 100)) #on modélise la part de l'information de chaque dimension

vp <- get_eigenvalue(mca);vp #conserver les axes dont la valeur propre > 1/NB VARIABLES ACTIVES

#coordonnées des individus sur les axes

individus <- get_mca_ind (mca)

coord_ind_ACM <- individus$coord

################################################ Clustering #######################################################

GRDF_kmeans_2 <- coord_ind_ACM[,1:4] #on charge dans une table les colonnes 1 à 4 de la table coord_ind_ACM

########################################### choix du nombre de cluster ############################################

ratio_ss <- data.frame(cluster = seq(from = 1, to = 10, by = 1)) #on crée une table de 1 ? 10 clusters

for (k in 1:10) {
  km_model <- kmeans(GRDF_kmeans_2, k, nstart = 1000)
  ratio_ss$ratio[k] <- km_model$tot.withinss / km_model$totss 
} #on effectue 10 clustering à 100 itérations pour voir lequel est le plus optimal

library(ggplot2) #on charge le packages pour modéliser la courbe d'elbow 

ggplot(ratio_ss, aes(cluster, ratio)) +  geom_line() + geom_point() #on modélise la courbe d'elbow
warnings()

#################################################### Clustering ###################################################

k_m <- kmeans (GRDF_kmeans_2, 5, nstart = 1000) #on effectue un clustering avec le nombre de clusters optimal

GRDF_cluster_size <- data.frame(k_m$size) #taille de chaque cluster

GRDF$cluster_2 <- k_m$cluster #ajoute la variable cluster à la table GRDF d'origine

GRDF <- GRDF %>%
  mutate(
    cluster_2 = case_when(
      cluster_2 == 3 ~ "Cluster 1",
      cluster_2 == 2 ~ "Cluster 2",
      cluster_2 == 5 ~ "Cluster 3",
      cluster_2 == 4 ~ "Cluster 4",
      cluster_2 == 1 ~ "Cluster 5",
    )
  )

write.csv(GRDF,"GRDF_clustering.csv", row.names = FALSE) #on exporte la table GRDF pour l'analyser dans Data studio
