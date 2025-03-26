#Load libraries
library(readxl)
library(readr)
library(tidyverse)
library(zoo)
library(chron)
library(lubridate)
library(ggplot2)
library(dplyr)

#load data from Thailand World Mountain and Trail Running Championships 2021
#results2021 <- read_excel("data/02ResultsResults.xlsx", 
#                          col_types = c("text", "numeric", "text", "text", "text","date", "date","text", "text"))

#write.csv(results2021, file = "data/WMTRC_Thailand.csv")
#save as *.csv

# Resultados WMTRC Thailand 2021 ####
results2021 <- read_csv("data/WMTRC_Thailand.csv", col_types = cols(...1 = col_skip()))
results2021$Date <- as.Date(results2021$Date)
results2021$Time <- (times(format(results2021$Time, "%H:%M:%S")))
results2021$Times <- paste(results2021$Date, results2021$Time )
results2021$Times <- as.POSIXct(results2021$Times, format='%Y-%m-%d %H:%M:%OS')
results2021 <- results2021 %>% select(-Date, -Time)

DNS <-  results2021 |> filter(Rank == "DNS")
DNF <-  results2021 |> filter(Rank == "DNF")

DNS
DNF

#Clean data
results2021 <- na.omit(results2021)
results2021$Rank <- as.numeric(results2021$Rank)

#Subset data
cl_2021 <- results2021 |> filter(Nation=="CHI")
Long <- results2021 |> filter(Race =="Long Trail Race")  
Short <- results2021 |> filter(Race =="Short Trail Race")  
Uphill <- results2021 |> filter(Race =="Uphill Mountain Race")  
Downhill <- results2021 |> filter(Race =="Up and Downhill Mountain Race")  

#Chilean results
knitr::kable(
cl_2021 |>
  filter(Sexo == "Male") |>
  select(Race, Rank, Bib, Name, Sexo, Times) |>
  arrange(Race, Rank))

knitr::kable(
cl_2021 |>
  filter(Sexo == "Female") |>
  select(Race, Rank, Bib, Name, Sexo, Times) |>
  arrange(Race, Rank))

# General results
ggplot(Long, aes(x = Times, y = Sexo)) +
  geom_boxplot() +
  geom_jitter(aes(x = Times, y = Sexo), color = "red", size = 2, data = cl_2021 |> filter(Race == "Long Trail Race") ) +  # Puntos resaltados
  scale_x_datetime(date_labels = "%H:%M:%S") +  # Formato eje X
  labs(title = "Long Trail Race - Distribución de Tiempos por Sexo", x = "Tiempo (HH:MM:SS)", y = "Sexo") +
  theme_minimal()
summary(Long)
knitr::kable(
results2021 |> filter(Race == "Long Trail Race" & Rank <=3 & Sexo == "Male"))
knitr::kable(
results2021 |> filter(Race == "Long Trail Race" & Rank <=3 & Sexo == "Female"))

ggplot(Short, aes(x = Times, y = Sexo)) +
  geom_boxplot() +
  geom_jitter(aes(x = Times, y = Sexo), color = "red", size = 2, data = cl_2021 |> filter(Race == "Short Trail Race") ) +  # Puntos resaltados
  scale_x_datetime(date_labels = "%H:%M:%S") +  # Formato eje X
  labs(title = "Short Trail Race - Distribución de Tiempos por Sexo", x = "Tiempo (HH:MM:SS)", y = "Sexo") +
  theme_minimal()
summary(Short)
knitr::kable(
results2021 |> filter(Race == "Short Trail Race" & Rank <=3 & Sexo == "Male"))
knitr::kable(
results2021 |> filter(Race == "Short Trail Race" & Rank <=3 & Sexo == "Female"))

ggplot(Uphill, aes(x = Times, y = Sexo)) +
  geom_boxplot() +
  geom_jitter(aes(x = Times, y = Sexo), color = "red", size = 2, data = cl_2021 |> filter(Race == "Uphill Mountain Race") ) +  # Puntos resaltados
  scale_x_datetime(date_labels = "%H:%M:%S") +  # Formato eje X
  labs(title = "Uphill Mountain Race - Distribución de Tiempos por Sexo", x = "Tiempo (HH:MM:SS)", y = "Sexo") +
  theme_minimal()
summary(Uphill)
knitr::kable(
results2021 |> filter(Race == "Uphill Mountain Race" & Rank <=3 & Sexo == "Male"))
knitr::kable(
results2021 |> filter(Race == "Uphill Mountain Race" & Rank <=3 & Sexo == "Female"))

ggplot(Downhill, aes(x = Times, y = Sexo)) +
  geom_boxplot() +
  geom_jitter(aes(x = Times, y = Sexo), color = "red", size = 2, data = cl_2021 |> filter(Race == "Up and Downhill Mountain Race") ) +  # Puntos resaltados
  scale_x_datetime(date_labels = "%H:%M:%S") +  # Formato eje X
  labs(title = "Up and Downhill Mountain Race - Distribución de Tiempos por Sexo", x = "Tiempo (HH:MM:SS)", y = "Sexo") +
  theme_minimal()
summary(Downhill)
knitr::kable(
results2021 |> filter(Race == "Up and Downhill Mountain Race" & Rank <=3 & Sexo == "Male"))
knitr::kable(
results2021 |> filter(Race == "Up and Downhill Mountain Race" & Rank <=3 & Sexo == "Female"))

#Medals
medals <- results2021 |> mutate(Medal = case_when(Rank == 1 ~ "Gold",
                                                  Rank == 2 ~ "Silver",
                                                  Rank == 3 ~ "Bronze",
                                                  TRUE ~ NA_character_))

medals <- na.omit(medals) 

# Contar medallas Generales por país
rank <- 
  medals |>
    group_by(Nation, Medal) |>
    summarise(Count = n(), .groups = 'drop') |>
    tidyr::pivot_wider(names_from = Medal, values_from = Count, values_fill = list(Count = 0)) |>
    mutate(Total = Gold + Silver + Bronze) |>
    arrange(desc(Total)) |>
    mutate(Ranking = row_number())

# Reordenar columnas
rank <- rank %>% select(Ranking, Nation, Gold, Silver, Bronze, Total)
knitr::kable(rank)
