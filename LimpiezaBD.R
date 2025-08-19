#Librerias

library(tidyverse)
library(corrplot)
library(ggplot2)
library(dplyr)


#Carga de datos
df <- World_University_Rankings_2025
df


#Inspección de la base de datos
str(df)
summary(df) #Todas nuestras variables son tipo character
head(df)


#Nuevo nombramiento de las variables

df <- df %>%
  rename(Rank = `Rank`, 
         University = `University Name`,
         Country = `CountryRegion`,
         Overall = `Overall`,
         Teaching = `Teaching`,
         Research_Environment = `Research_Environment`, 
         Research_Quality = `Research_Quality`,  
         Industry = `Industry`,
         International_Outlook = `International_Outlook`,
         About_university= `About_university`,
         About_university2= `About_university2`)
colnames(df)

#Tratamiento de la base de datos
datos <- df %>%
  filter(!is.na(University)) %>% #Filtramos los datos que no tengan nulos en la columna University
  filter(!is.na(Country))    %>% #Filtramos los datos que no tengan nulos en la columna Country
  filter(Rank != "Reporter") %>% #Filtramos los datos para que todos los Reporter desaparezcan
  
  select(-c(About_university,About_university2)) %>% #Eliminamos las columnas "About_university"
  
  mutate(Rank = str_replace(Rank, "=","")) %>% #Eliminamos el =
  mutate(Rank = str_replace(Rank, "\\+", ""))  #Eliminamos el +
datos


#Tratamiento de variables

datos <- datos %>%
  separate(Rank, c("MinRank", "MaxRank"), sep = "–", remove = F)  %>% #Separo Rank en Min y Max
  separate(Overall, c("MinOverall", "MaxOverall"), sep = "–", remove = F)
datos

datos$MinRank <- as.numeric(datos$MinRank) #Los convierto en numericos
datos$MaxRank <- as.numeric(datos$MaxRank)

datos$MinOverall <- as.numeric(datos$MinOverall) 
datos$MaxOverall <- as.numeric(datos$MaxOverall)


datos <- datos %>%
  mutate(Rank= if_else(is.na(MaxRank), MinRank, (MaxRank+MinRank)/2)) %>% #Aplico un if para dejar el Min o hacer promedio
  select(-c(MinRank, MaxRank)) %>% #Elimino las columnas una vez usadas
  
  mutate(Overall= if_else(is.na(MaxOverall), MinOverall, (MaxOverall+MinOverall)/2)) %>%
  select(-c(MinOverall, MaxOverall))

glimpse(datos)


#Creacion de variables categoricas

datos <- datos%>%
  mutate(Rank_category= case_when(
    Rank <=10 ~ "Top 10",
    Rank <=20 ~ "Top 20",
    Rank <=30 ~ "Top 30",
    Rank <=40 ~ "Top 40",
    Rank <=50 ~ "Top 50",
    Rank <=100 ~ "Top 100",
    Rank <=500 ~ "Top 500",
    Rank <=1000 ~ "Top 1000",
    Rank <=1500 ~ "Top 1500",
    Rank > 1500 ~ "Top 1500+"),
    
    Overall_category= case_when(
      Overall >= 90 ~ "Excelente (90+)",
      Overall >= 80 ~ "Muy alto (80-89.9)",
      Overall >= 70 ~ "Alto (70-79.9)",
      Overall >= 60 ~ "Bueno (60-69.9)",
      Overall >= 50 ~ "Medio (50-59.9)",
      Overall >= 40 ~ "Bajo (40-49.9)",
      Overall < 40 ~  "Muy bajo (40-)"))

# Variables numericas

datos$Teaching              <- as.numeric(datos$Teaching)
datos$Research_Environment  <- as.numeric(datos$Research_Environment) 
datos$Research_Quality      <- as.numeric(datos$Research_Quality)
datos$Industry              <- as.numeric(datos$Industry)
datos$International_Outlook <- as.numeric(datos$International_Outlook)

numeric_cols <- select_if(datos, is.numeric)


print(numeric_cols)

################################################################################

#ANÁLISIS ESTADISTICO


Table_category <- table(datos$Rank_category)        
barplot(Table_category,                   
        main="Rank Category",              
        xlab="Rank",                  
        ylab="Categorias",                   
        legend = rownames(Table_category),    
        ylim = c(0, 1500),
        xlim = c(0, 15))  



#Boxplot teaching y Reasearch_Enviroment
cor(datos$Teaching, datos$Research_Environment)

par(mfrow= c(1,2))
boxplot(datos$Teaching, main= "Teaching")
boxplot(datos$Research_Environment, main= "Research Environment")
par(mfrow= c(1,1))


#Boxplot Industry, International_Outlook y Research_Quality
par(mfrow= c(1,3))
boxplot(datos$Research_Quality, main= "Research Quality")
boxplot(datos$Industry, main= "Industry")
boxplot(datos$International_Outlook, main= "International Outlook")
par(mfrow= c(1,1))


#Segunda parte
OvCategory <- datos %>%
  group_by(Overall_category) %>%
  summarise(
    N_Universidades = n(),
    Teaching_Avg = mean(Teaching, na.rm = TRUE),
    Research_Env_Avg = mean(Research_Environment, na.rm = TRUE),
    Research_Quality_Avg = mean(Research_Quality, na.rm = TRUE),
    Industry_Avg = mean(Industry, na.rm = TRUE),
    International_Avg = mean(International_Outlook, na.rm = TRUE)
  ) %>%
  arrange(desc(Teaching_Avg)) # Ordenamos

print(OvCategory[,c(1,2,3,4)])
print(OvCategory[,c(5,6,7)])




numeric_datos <- datos %>%
  select(Overall,Teaching,Research_Environment,Research_Quality,Industry, International_Outlook)


cor_matrix <- cor(numeric_datos)
corrplot(cor_matrix, method = "number")

#Mejor decidí seguir haciendo las graficas en Quarto ✌

