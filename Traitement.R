rm(list = ls());
setwd("C:/Users/marvin.edorh_artefac/Desktop/Formation R")

#opérateur
1 == 1
1 == 2
1 != 1
1 > 2
1 < 2
1 >= 2
1 <= 2
1 <= 2 & 1 <= 2 #et
1 <= 2 | 1 <= 2 #ou

#vecteurs
x1 <- c(1,20)
x1 <- c(1:20)
x1 <- seq(1,20,2)
x1 <- rep(seq(1,20,2),2)
x1 + 10 #On ajoute 10 à chaque valeur du vecteur
x1 + x1 # meme nombre de valeurs aux 2 vecteur
length(x1)
x1[1]
names(x1) <- x1
x1["1"]

#matrix
x2 <- matrix(1:12, byrow = TRUE, nrow = 4) ; x2
x2 <- matrix(1:12, byrow = FALSE, nrow = 4) ; x2
col <- c("col1", "col2", "col3")
colnames(x2) <- col
lin <- c("lin1", "lin2", "lin3", "lin4")
rownames(x2) <- lin
somme_col <- colSums(x2)
somme_lin <- rowSums(x2)
cbind(x2,somme_lin)
rbind(x2,somme_col)
x2[1]
x2[1,]
x2[,1]
x2[,-3]

#facteurs
sexe <- factor(c("H","F","H","H","F"))
levels(sexe)
table(sexe)
levels(sexe) <- c("Femme","Homme")
taille <- factor(c("petit","petit","moyen","grand","moyen"))
taille <- factor(c("petit","petit","moyen","grand","moyen"), ordered = TRUE, levels = c("petit","moyen","grand"))
taille[1]>taille[3]

#dataframe
library(readxl)
DATA <- as.data.frame(read_excel("kehr70fl.xlsx"))

dim(DATA) #ligne, colonne
str(DATA) #type varriable
head(DATA, 2) #2 premiere ligne
tail(DATA, 3) #2 derviere ligne
DATA[3,1] #troisieme ligne, premiere colonne ("nom de ligne ou colonne")
subset (DATA, subset = `Cluster number` > 3)
orderddata <- DATA[ordered(DATA$`Cluster number`),]


levels(factor(DATA$align))

#Creer une base à partir d'une autre

a <- data.frame(DATA$`Case Identification`)
b <- data.frame(DATA$`Household selected for Domestic Violence`)
c <- data.frame(DATA$`Size of agricultural land`)
d <- data.frame(a,b)
e <- data.frame(a,c)
f <- data.frame(a,b,c)
f <- data.frame(DATA$`Case Identification`, DATA$`Household selected for Domestic Violence`, DATA$`Size of agricultural land`)

name <- c("id","violence","size")
name_1 <- c("id","violence")
name_2 <- c("id","size")

colnames(f) <- name
colnames(d) <- name_1
colnames(e) <- name_2

#Modalité d'une variavle cat
levels(f$violence)

#Filtrer les sur une valeur
library(dplyr)
f_violence_0 <- f  %>%  filter(violence == 0 )
f_violence_na <- f  %>%  filter(is.na(violence))

#trier les sur une valeur
library(dplyr)
f_violence_0 <- f  %>%  arrange(desc(violence))

#Créer une variable conditionnelle "new"
f_new <- f %>% mutate( new = ifelse(violence == 1 , "vv", "bb" ),
                         new_2 = ifelse(is.na(violence) == TRUE, "vv", "bb" ))

# Recoder une variable
f_new_2 <- f  %>% #table f
  mutate(
    violence  = case_when( #variable 
      violence == 1  ~ "small",
      violence == 0 ~ "midsize",
      is.na(violence) == TRUE ~ "large"
    )
  )

# selectionner des variable
f_new_3 <- f  %>% select("id","violence")
#ou 
f_new_3 <- f  %>% select( -size )
#ou
f_new_3 <- f  %>% select(1:2)

# selectionner des ligne et renommer colonne
f_new_4 <- f  %>% slice(1:2) %>% rename("ident" = id)
f_new_4 <- f  %>% slice(n()) #dernier

#summarise
f_new_5 <- f  %>%  filter(violence == 0 ) %>% summarise(moyenne_size = mean(size,na.rm = TRUE))

#group_by group_by(var1,var2), ungroup(var1,var2)
f_new_6 <- f  %>%  group_by(violence) %>% summarise(moyenne_size = mean(size,na.rm = TRUE), mx = max(size,na.rm = TRUE))

#compter les lignes
f  %>%  count() 
f  %>%  count(violence) 
f  %>%  count(violence, sort = TRUE) 

#TOP
f  %>%  group_by(violence) %>%  top_n(1,id) #cb ligne (ici 1) pour chaque niveau de violence

#transmute
f  %>%  transmute("ident" = id,violence,size*2)

#echantillion
f  %>%  sample_f(10) #10 ligne
f  %>%  sample_frac(10) #10% de ligne

#echantillion
f  %>%  arrange(id) %>%  mutate(size_bis = size - lag(size)) #ligne précédente
f  %>%  arrange(id) %>%  mutate(size_bis = size - lead(size)) #ligne suivante

#Supprimer les valeurs manquantes
library(tidyr)
f_sans_na <- f %>% drop_na()

#REMPLACER VALEUR MANQUANTEPAR 0

f_violence_na [is.na(f_violence_na)] <- 0

#Croiser des tables
f_3 = merge(d,e, by.x = "id", by.y ="id", all.x = TRUE) # all.x left join
f_3 = merge(d,e, by.x = "id", by.y ="id", all.y = TRUE) # all.y right join
f_3 = merge(d,e, by.x = "id", by.y ="id", all = TRUE) # all full join

#liste
list(x1,X2,DATA)

#if
age <- 145

if (age >= 18 & age < 150) 
  {
    print("mageur")
  } else if (age > 120)  
    {
      print("immortel")
    } else
      {
        print("mineur")
      }

#boucle while
score <- 100

while (score > -30) 
{
  print(paste("vivant:",score))
  if (score < 50)
  {
    print("DANGER")
  }
  if (score == 0)
  {
    break
  }
  
  score <- score - 10
} 

#boucle for
id <- DATA$`Case Identification`
for (i in 1:length(DATA$`Case Identification`))
  {
  print(paste("id :",id[i]))
  }

#fonction 
vec <- c(1:20,NA) ; mean(vec)
mean(vec, na.rm = TRUE)

my_function <- function(x)
{
  mean(x, na.rm = TRUE)^2
}
my_function(vec)
my_function(c(1,6,7))

lapply(airquality, mean, na.rm = TRUE)
unlist(lapply(airquality, mean, na.rm = TRUE))
sapply(airquality, mean, na.rm = TRUE)


grepl("a", names(airquality)) #chercher index
grep("a", names(airquality))#chercher index
sub("a", "f" ,names(airquality)) # premier a remplacé
gsub("a", "f" ,names(airquality)) # tous les a remplacé

#date
jour_j  <- Sys.Date()
naissance <- as.Date("1990-12-25") #,format = %Y-%d-%m
round(as.numeric((jour_j - naissance) / 365))

write.csv(DATA,"data.csv", row.names = FALSE)
