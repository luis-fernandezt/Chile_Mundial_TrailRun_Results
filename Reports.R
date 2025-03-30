#Load libraries
library(readxl)
library(readr)
library(tidyverse)
library(zoo)
library(chron)
library(lubridate)
library(ggplot2)
library(dplyr)
library(viridis)

#load data from Thailand World Mountain and Trail Running Championships 2021
#results2021 <- read_excel("data/ResultsResults.xlsx", 
#                          col_types = c("text", "numeric", "text", "text", "text","date", "date","text", "text"))

#write.csv(results2021, file = "data/WMTRC_Thailand.csv") #save as *.csv

# Resultados WMTRC Thailand 2021 ####
results2021 <- read_csv("data/WMTRC_Thailand.csv", col_types = cols(...1 = col_skip()))
results2021$Date <- as.Date(results2021$Date)
results2021$Time <- (times(format(results2021$Time, "%H:%M:%S")))
results2021$Times <- paste(results2021$Date, results2021$Time )
results2021$Times <- as.POSIXct(results2021$Times, format='%Y-%m-%d %H:%M:%OS')
results2021 <- results2021 %>% select(-Date, -Time)

DNS <-  results2021 |> filter(Rank == "DNS")
DNF <-  results2021 |> filter(Rank == "DNF")

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
  filter(Gender == "Male") |>
  select(Race, Rank, Bib, Name, Gender, Times) |>
  arrange(Race, Rank))

knitr::kable(
cl_2021 |>
  filter(Gender == "Female") |>
  select(Race, Rank, Bib, Name, Gender, Times) |>
  arrange(Race, Rank))

# General results
ggplot(Long, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2021 |> filter(Race == "Long Trail Race") ) +  # Puntos resaltados
  scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M") +  # Formato eje X
  labs(title = "Long Trail Race", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
summary(Long)
knitr::kable(
results2021 |> filter(Race == "Long Trail Race" & Rank <=3 & Gender == "Male"))
knitr::kable(
results2021 |> filter(Race == "Long Trail Race" & Rank <=3 & Gender == "Female"))

ggplot(Short, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2021 |> filter(Race == "Short Trail Race") ) +  # Puntos resaltados
  scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M") +  # Formato eje X
  labs(title = "Short Trail Race", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
summary(Short)
knitr::kable(
results2021 |> filter(Race == "Short Trail Race" & Rank <=3 & Gender == "Male"))
knitr::kable(
results2021 |> filter(Race == "Short Trail Race" & Rank <=3 & Gender == "Female"))

ggplot(Uphill, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2021 |> filter(Race == "Uphill Mountain Race") ) +  # Puntos resaltados
  labs(title = "Uphill Mountain Race", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
summary(Uphill)
knitr::kable(
results2021 |> filter(Race == "Uphill Mountain Race" & Rank <=3 & Gender == "Male"))
knitr::kable(
results2021 |> filter(Race == "Uphill Mountain Race" & Rank <=3 & Gender == "Female"))

ggplot(Downhill, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2021 |> filter(Race == "Up and Downhill Mountain Race") ) +  # Puntos resaltados
  labs(title = "Up and Downhill Mountain Race", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
summary(Downhill)
knitr::kable(
results2021 |> filter(Race == "Up and Downhill Mountain Race" & Rank <=3 & Gender == "Male"))
knitr::kable(
results2021 |> filter(Race == "Up and Downhill Mountain Race" & Rank <=3 & Gender == "Female"))

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




# Resultados WMTRC Innsbruck-Stubai 2023 ####
#results2023 <- read_excel("data/ResultsResults.xlsx", 
 #                            sheet = "Table 2", col_types = c("text", 
  #                                                            "numeric", "text", "text", "numeric", 
   #                                                           "text", "text", "date", "text", "text", 
    #                                                          "text"))

#write.csv(results2023, file = "data/WMTRC_Innsbruck.csv") #save as *.csv

results2023 <- read_csv("data/WMTRC_Innsbruck.csv", col_types = cols(...1 = col_skip()))

results2023$Date <- as.Date(results2023$Date)
results2023$Time <- as.times(results2023$Time)

#Subset data
RStop <- results2023 |> filter(Rank == "race stopped") 
DNF <-   results2023 |> filter(Rank == "DNF")
DNSUp <- results2023 |> filter(Rank == "did not show up")
DNS <-   results2023 |> filter(Rank == "DNS")
DSQ <-  results2023 |> filter(Rank == "DSQ")
cl_2023 <- results2023 |> filter(Nation=="CHI")

#Clean data
results2023 <- na.omit(results2023)
results2023$Rank <- as.numeric(results2023$Rank)
results2023$Rank <- as.numeric(results2023$Rank)
results2023$Times <- paste(results2023$Date, results2023$Time )
results2023$Times <- as.POSIXct(results2023$Times, format='%Y-%m-%d %H:%M:%OS')
results2023 <- results2023 %>% select(-Date, -Time, -img)

#Chilean results
knitr::kable(
  cl_2023 |>
    filter(Gender == "Male" & Rank != "DNF") |>
    select(Race, Rank, Bib, Name, Gender, Time) |>
    arrange(Race, as.numeric(Rank)))

knitr::kable(
  cl_2023 |>
    filter(Gender == "Male" & Rank == "DNF") |>
    select(Race, Rank, Bib, Name, Gender, Time) |>
    arrange(Race, Rank))

knitr::kable(
  cl_2023 |>
    filter(Gender == "Female" & Rank != "DNF" & Rank != "DSQ" ) |>
    select(Race, Rank, Bib, Name, Gender, Time) |>
    arrange(Race, as.numeric(Rank)))

knitr::kable(
  cl_2023 |>
    filter(Gender == "Female" & Rank == "DNF" | Rank == "DSQ" ) |>
    select(Race, Rank, Bib, Name, Gender, Time) |>
    arrange(Race, Rank))

#Subset RACE data
cl_2023_ <- results2023 |> filter(Nation=="CHI")
Long <- results2023 |> filter(Race =="Trail Long")  
Short <- results2023 |> filter(Race =="Trail Short")  
Vertical <- results2023 |> filter(Race =="Vertical")  
Senior <- results2023 |> filter(Race =="Mountain Classic Senior")
Junior <- results2023 |> filter(Race =="Mountain Classic Junior") 

# General results
ggplot(Long, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2023_ |> filter(Race == "Trail Long") ) +  # Puntos resaltados
  scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M") +  # Formato eje X
  labs(title = "WMTRC - Long Trail Race 2023", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
Long |> filter(Gender == 'Male') |> summary()
knitr::kable(results2023 |> filter(Race == "Trail Long" & Rank <=3 & Gender == "Male"))
knitr::kable(results2023 |> filter(Race == "Trail Long" & Nation == "CHI" & Gender == "Male"))
Long |> filter(Gender == 'Female') |> summary()
knitr::kable(results2023 |> filter(Race == "Trail Long" & Rank <=3 & Gender == "Female"))

ggplot(Short, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2023_ |> filter(Race == "Trail Short") ) +  # Puntos resaltados
  scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M") +  # Formato eje X
  labs(title = "WMTRC - Short Trail Race 2023", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
Short |> filter(Gender == 'Male') |> summary()
knitr::kable(results2023 |> filter(Race == "Trail Short" & Rank <=3 & Gender == "Male"))
knitr::kable(results2023 |> filter(Race == "Trail Short" & Nation == "CHI" & Gender == "Male"))
Short |> filter(Gender == 'Female') |> summary()
knitr::kable(results2023 |> filter(Race == "Trail Short" & Rank <=3 & Gender == "Female"))
knitr::kable(results2023 |> filter(Race == "Trail Short" & Nation == "CHI" & Gender == "Female"))

ggplot(Vertical, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2023_ |> filter(Race == "Vertical") ) +  # Puntos resaltados
  scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M") +  # Formato eje X
  labs(title = "WMTRC - Vertical Race 2023", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
Vertical |> filter(Gender == 'Male') |> summary()
knitr::kable(results2023 |> filter(Race == "Vertical" & Rank <=3 & Gender == "Male"))
knitr::kable(results2023 |> filter(Race == "Vertical" & Nation == "CHI" & Gender == "Male"))
Vertical |> filter(Gender == 'Female') |> summary()
knitr::kable(results2023 |> filter(Race == "Vertical" & Rank <=3 & Gender == "Female"))
knitr::kable(results2023 |> filter(Race == "Vertical" & Nation == "CHI" & Gender == "Female"))

ggplot(Senior, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2023_ |> filter(Race == "Mountain Classic Senior") ) +  # Puntos resaltados
  scale_x_datetime(date_breaks = "hour", date_labels = "%H:%M") +  # Formato eje X
  labs(title = "WMTRC - Classic Senior Race 2023", x = "Tiempo (HH:MM)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
Senior |> filter(Gender == 'Male') |> summary()
knitr::kable(results2023 |> filter(Race == "Mountain Classic Senior" & Rank <=3 & Gender == "Male"))
knitr::kable(results2023 |> filter(Race == "Mountain Classic Senior" & Nation == "CHI" & Gender == "Male"))
Senior |> filter(Gender == 'Female') |> summary()
knitr::kable(results2023 |> filter(Race == "Mountain Classic Senior" & Rank <=3 & Gender == "Female"))
knitr::kable(results2023 |> filter(Race == "Mountain Classic Senior" & Nation == "CHI" & Gender == "Female"))

ggplot(Junior, aes(x = Times, y = Gender, fill = Gender)) +
  geom_boxplot(outlier.size=-1) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(aes(x = Times, y = Gender), color = "black", size = 1.5, alpha=0.9) +
  geom_jitter(aes(x = Times, y = Gender), color = "red", size = 3,  alpha=0.9, data = cl_2023_ |> filter(Race == "Mountain Classic Junior") ) +  # Puntos resaltados
  scale_x_datetime(date_breaks = "min", date_labels = "%M") +  # Formato eje X
  labs(title = "WMTRC - Classic Junior Race 2023", x = "Tiempo (Min)", y = "META") +
  theme_minimal() +
  guides(fill="none", color="none")
Junior |> filter(Gender == 'Male') |> summary()
knitr::kable(results2023 |> filter(Race == "Mountain Classic Junior" & Rank <=3 & Gender == "Male"))
knitr::kable(results2023 |> filter(Race == "Mountain Classic Junior" & Nation == "CHI" & Gender == "Male"))
Junior |> filter(Gender == 'Female') |> summary()
knitr::kable(results2023 |> filter(Race == "Mountain Classic Junior" & Rank <=3 & Gender == "Female"))


#Fin