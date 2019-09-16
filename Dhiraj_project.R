#Loading Dataset
read.csv("Pokemon.csv",sep = ",") -> pokemon
View(pokemon)

#Use Package to Remove Column
library(dplyr)
pokemon %>% select(-1) -> pokemon

#Renaming Of Column
colnames(pokemon)[colnames(pokemon)=="Type.1"] <- "Primary_Type"
colnames(pokemon)[colnames(pokemon)=="Type.2"] <- "Secondary_Type"
colnames(pokemon)[colnames(pokemon)=="HP"] <- "Health_Point"
colnames(pokemon)[colnames(pokemon)=="Sp..Atk"] <- "Special_Attack"
colnames(pokemon)[colnames(pokemon)=="Sp..Def"] <- "Special_Defence"

#Understanding Data
str(pokemon)

#Convert lengendary into Factor
#as.factor(pokemon$Legendary)->pokemon$Legendary

#Overview Of Type[18]
table(pokemon$Primary_Type)

#------------------------------------------------------------------
#Task_1 [As Biggnner Pokemon Trainer ....]
#Selecting Grass Pokemon
pokemon %>% filter(Primary_Type=="Grass")->Grass_pokemon
Grass_pokemon %>% filter(Secondary_Type=="Poison")->Grass_Poison_pokemon
range(Grass_Poison_pokemon$Speed)
Grass_Poison_pokemon %>% filter(Speed==90)->My_Grass_pokemon

#Selecting Water Pokemon
pokemon %>% filter(Primary_Type=="Water")->Water_pokemon
Water_pokemon %>% filter(Secondary_Type=="Psychic")->Water_Psychic_pokemon
range(Water_Psychic_pokemon$Defence)
Water_Psychic_pokemon %>% filter(Defense==110)->My_Water_pokemon

#Selecting Fire Pokemon
pokemon %>% filter(Primary_Type=="Fire")->Fire_pokemon
Fire_pokemon %>% filter(Secondary_Type=="Fighting")->Fire_Fighting_pokemon
range(Fire_Fighting_pokemon$Attack)
Fire_Fighting_pokemon %>% filter(Attack==160)->My_Fire_pokemon

#my three pokemons
rbind(My_Fire_pokemon,My_Grass_pokemon,My_Water_pokemon)->My_Pokemons

#--------------------------------------------------------------
#Task_2 [Understanding What influences a Pokemon's Attack]

#Splitting Tool
library(caTools)

#Splitting data
sample.split(pokemon$Attack,SplitRatio = 0.65)->split_index
subset(pokemon,split_index==T)-> train1 #65%
subset(pokemon,split_index==F)-> test1 #35%

#Building model 1 (Attack v/s Defence)
lm(Attack~Defense,data=train1)->mod_regress
predict(mod_regress,test1)->result_regress
cbind(Actual=test1$Attack,Predicted=result_regress)->final_data
as.data.frame(final_data)->Final_Data

#Finding Error
(Final_Data$Actual-Final_Data$Predicted)->Error
cbind(Final_Data,Error)->Final_Data
rmse1<-sqrt(mean(Final_Data$Error^2))
rmse1

#Building model 2 (Attack v/s Defence,speed,health_point)
lm(Attack~Defense+Speed+Health_Point,data=train1)->mod_regress2
predict(mod_regress2,test1)->result_regress2
cbind(Actual=test1$Attack,Predicted=result_regress2)->final_data2
as.data.frame(final_data2)->Final_Data2

#Finding Error[more better]
(Final_Data2$Actual-Final_Data2$Predicted)->Error2
cbind(Final_Data2,Error2)->Final_Data2
rmse2<-sqrt(mean(Final_Data2$Error2^2))
rmse2

#-----------------------------------------------------------
#Task_3 [Finding Lengendary Pokemon]
#Splitting Data
sample.split(pokemon$Legendary,SplitRatio = 0.65)->split_values
subset(pokemon,split_values==T)-> train_data
subset(pokemon,split_values==F)-> test1_data

#Total data in Each
nrow(train_data)
nrow(test1_data)

#Decision Tree
library(rpart)

#Building Model-1 [better]
rpart(Legendary~.,data = train_data)->mod1
predict(mod1,test1_data,type = "class")->result1
table(test1_data$Legendary,result1)

#Evaluting  Model-1
library(caret)
A1<-confusionMatrix(table(test1_data$Legendary,result1))

#Bulding Model-2
rpart(Legendary~Attack+Defense+Speed,data = train_data)->mod2
predict(mod2,test1_data,type = "class")->result2
table(test1_data$Legendary,result2)

#Evaluting Model-2
A2<-confusionMatrix(table(test1_data$Legendary,result2))



