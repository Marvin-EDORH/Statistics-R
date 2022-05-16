setwd("C:/Users/marvin.edorh_artefac/Desktop/Formation R")

library(readxl)
DATA_1 <- as.data.frame(read_excel("projet_Airline_small pour WA.xlsx"))
Zagat <- read.table("zagat_all+Nation.txt", header = TRUE, sep = "\t", dec = ".")
nsw <- read.csv("nsw.csv", sep=";",header = TRUE)

#kpi 1 répartition ville par pays

graph_1 <- ggplot(Zagat, aes(x=Nation))

graph_1 + geom_bar(aes(fill=City))

#kpi 2 restaurant par pays

library(dplyr)

restaurant_by_nation <- Zagat %>% group_by(Nation, City) %>% count(Restaurant) 
restaurant_by_nation <- as.data.frame(food_by_country)
restaurant_by_nation <- aggregate(n ~ Nation + City, food_by_country, FUN=sum)
restaurant_by_nation <- restaurant_by_nation %>% rename("restaurants" = n)

library(ggplot2)

graph_2 <- ggplot(restaurant_by_nation, aes(x=Nation, y=restaurants))
graph_2 + geom_col(aes(fill = City)) + geom_text(aes(label=restaurants), position = position_stack(vjust = 0.5), color="white", size=5) + labs (x = "Pays", y = "Nb Restos", title = "")

#kpi 3 part par pays
restaurant_by_nation_Nation <- restaurant_by_nation %>% group_by(Nation) %>% summarise(restaurants_nation = sum(restaurants))
install.packages("sqldf")

library(sqldf)

restaurant_by_nation_group <- sqldf("SELECT
                                        Nation, 
                                        City, 
                                        restaurants,
                                        restaurants_nation
                                     FROM 
                                        restaurant_by_nation
                                     LEFT JOIN 
                                        restaurant_by_nation_Nation 
                                        USING(Nation)")
  
restaurant_by_nation_2 <- restaurant_by_nation_group %>% mutate(percent_resto = round((restaurants/restaurants_nation)*100,2))

graph_3 <- ggplot(restaurant_by_nation_2, aes(x=Nation, y=percent_resto)) 
graph_3 + geom_col(aes(fill = City)) + geom_text(aes(label=percent_resto), position = position_stack(vjust = 0.5), color="white", size=5) 

library(scales)
restaurant_by_nation_2 <- restaurant_by_nation_group %>% mutate(percent_resto = restaurants/restaurants_nation)
graph_3 <- ggplot(restaurant_by_nation_2, aes(x=Nation, y=percent_resto)) 
graph_3 + geom_col(aes(fill = City)) + scale_y_continuous(labels = percent)


#kpi 4
food_by_country <- Zagat  %>%  group_by(Nation, City) %>% summarise(food = sum(Food))
graph_4 <- ggplot(food_by_country, aes(x=Nation, y=food))
graph_4 + geom_col(aes(fill = City)) + geom_text(aes(label=food), position = position_stack(vjust = 0.5), color="white", size=5)


#kpi 5 Prix moyen par pays
avg_cost_country <- aggregate(Cost ~ Nation, Zagat, FUN=mean)
graph_5 <- ggplot(avg_cost_country, aes(x=Nation, y=Cost)) 
graph_5 + geom_col(aes(fill = Nation)) 


#kpi 6 Prix en fonction du service
graph_6 <- ggplot(Zagat, aes(x=Cost, y=Service)) 
graph_6 + geom_point(aes(color = Nation)) + geom_smooth() 
graph_6 + geom_point(aes(color = Nation)) + geom_smooth(method = "lm", se = FALSE)


restaurant_by_nation <- Zagat %>% group_by(Nation,City) %>% count(Restaurant)
restaurant_by_nation <- as.data.frame(restaurant_by_nation)
restaurant_by_nation <- aggregate(n ~ Nation+City, restaurant_by_nation, FUN=sum)
restaurant_by_nation <- restaurant_by_nation %>% rename("restaurants" = n)

ggplot(restaurant_by_nation, aes(x="", y=restaurants, fill=City)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() 
#+ facet_grid(.~Nation)

restaurant_by_nation <- Zagat %>% group_by(Nation,City) %>% count(Restaurant)
restaurant_by_nation <- as.data.frame(restaurant_by_nation)
restaurant_by_nation <- aggregate(n ~ Nation+City, restaurant_by_nation, FUN=sum)
restaurant_by_nation <- restaurant_by_nation %>% rename("restaurants" = n)
restaurant_by_nation <- restaurant_by_nation %>% arrange(desc(City)) %>% mutate(prop = restaurants / sum(restaurant_by_nation$restaurants) *100) %>% mutate(ypos = cumsum(prop) - 0.5*prop)

ggplot(restaurant_by_nation, aes(x="", y=prop, fill=City)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  geom_text(aes( y = ypos, label = City), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

