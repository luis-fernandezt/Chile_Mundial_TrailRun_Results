#Load libraries
library(readxl)
library(tidyverse)
library(zoo)
library(chron)
library(lubridate)
library(ggplot2)

#load data from Thailand World Mountain and Trail Running Championships 2021
results2021 <- read_excel("data/02ResultsResults.xlsx", 
                          col_types = c("text", "numeric", "text", "text", "text","date", "date","text", "text"))
results2021$Date <- as.Date(results2021$Date)
results2021$Time <- (times(format(results2021$Time, "%H:%M:%S")))

#results2021$Times <- paste(results2021$Date, results2021$Time )
#results2021$Times <- as.POSIXct(results2021$Times, format='%Y-%m-%d %H:%M:%OS')
#results2021 <- results2021 %>% select(-Date, -Time)

#Clean data
results2021 <- na.omit(results2021)
results2021$Rank <- as.numeric(results2021$Rank)

#raw data
cl_2021 <- results2021 |> filter(Nation=="CHI")
Long <- results2021 |> filter(Race =="Long Trail Race")  
Short <- results2021 |> filter(Race =="Short Trail Race")  
Uphill <- results2021 |> filter(Race =="Uphill Mountain Race")  
Downhill <- results2021 |> filter(Race =="Up and Downhill Mountain Race")  

#Chilean results
cl_2021 |>
  filter(Sexo == "Male") |>
  select(Race, Rank, Bib, Name, Sexo, Time) |>
  arrange(Race, Rank)

cl_2021 |>
  filter(Sexo == "Female") |>
  select(Race, Rank, Bib, Name, Sexo, Time) |>
  arrange(Race, Rank)

#General results
boxplot(Long$Time~Long$Sexo, horizontal=T)
summary(Long)
results2021 |> filter(Race == "Long Trail Race" & Rank <=3 & Sexo == "Male")
results2021 |> filter(Race == "Long Trail Race" & Rank <=3 & Sexo == "Female")

boxplot(Short$Time~Short$Sexo, horizontal=T)
summary(Short)
results2021 |> filter(Race == "Short Trail Race" & Rank <=3 & Sexo == "Male")
results2021 |> filter(Race == "Short Trail Race" & Rank <=3 & Sexo == "Female")

boxplot(Uphill$Time~Uphill$Sexo, horizontal=T)
summary(Uphill)
results2021 |> filter(Race == "Uphill Mountain Race" & Rank <=3 & Sexo == "Male")
results2021 |> filter(Race == "Uphill Mountain Race" & Rank <=3 & Sexo == "Female")

boxplot(Downhill$Time~Downhill$Sexo, horizontal=T)
summary(Downhill)
results2021 |> filter(Race == "Up and Downhill Mountain Race" & Rank <=3 & Sexo == "Male")
results2021 |> filter(Race == "Up and Downhill Mountain Race" & Rank <=3 & Sexo == "Female")

#Medals
medals <- results2021 |> mutate(Medal = car::recode(results2021$Rank, '1="Gold"; 2="Silver"; 3="Bronze"; else = NA'))
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
rank
            