#Load libraries
library(readxl)
library(tidyverse)
library(zoo)

#load data from Thailand World Mountain and Trail Running Championships 2021
results2021 <- read_excel("data/02ResultsResults.xlsx", col_types = c("text", "numeric", "text", "text", "text", "date", "text"))
#results2021$Time <- (times(format(results2021$Time, "%H:%M:%S")))

#raw data
cl_2021 <- results2021 %>% filter(Nation=="CHI")
Long <- results2021 %>% filter(Race =="Long Trail Race")  
Short <- results2021 %>% filter(Race =="Short Trail Race")  
Uphill <- results2021 %>% filter(Race =="Uphill Mountain Race")  
Downhill <- results2021 %>% filter(Race =="Up and Downhill Mountain Race")  

#Clean data
results2021 <- na.omit(results2021)
results2021$Rank <- as.numeric(results2021$Rank)
cl_2021$Rank <- as.numeric(cl_2021$Rank)

#Chilean results
cl_2021
summary(cl_2021)
boxplot(cl_2021$Time~cl_2021$Race, horizontal=T)

#General results
boxplot(Long$Time~Long$Sexo, horizontal=T)
boxplot(Short$Time~Short$Sexo, horizontal=T)
boxplot(Uphill$Time~Uphill$Sexo, horizontal=T)
boxplot(Downhill$Time~Downhill$Sexo, horizontal=T)

library(tidyverse)
library(ggplot2)

Short %>%
  arrange(Nation) %>%
  ggplot(aes(Time, Nation)) +
   geom_boxplot() +
   scale_x_datetime(date_labels = "%H:%M:%S")
