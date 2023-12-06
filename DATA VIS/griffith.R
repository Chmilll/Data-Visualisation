library(dplyr)
library(ggplot2)
library(plotly)
library(ggtext)
library(ggcorrplot)
library(jpeg)
library(readxl)
library(magrittr)
library(imager)
library(countrycode)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)
library(gridtext)
library(shiny)
data <- read_excel("XL DATA VIS.xlsx", sheet = 1)
datatable(data)
tags$img(src = "photo.jpg", width = 600, height = 300)

names(data) <- c("année", "semestre", "gaz naturel en UE", "gaz naturel en France", "électricité en UE", "électricité en France")



########################
my_data = read_excel("Donnée de gaz par pays.xlsx", sheet = 3)
my_data$states <- c("Belgium", "Bulgaria", "Czech Republic", "Denmark", "Germany", "Estonia", "Ireland", "Spain", "France", "Croatia", "Italy", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Sweden", "Bosnia and Herzegovina", "Turkey")

states_map <- map_data("world",c("Belgium", "Bulgaria", "Czech Republic", "Denmark", "Germany", "Estonia", "Ireland", "Spain", "France", "Croatia", "Italy", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Sweden", "Bosnia and Herzegovina", "Turkey")
)

latitude <- c(50.8503, 42.7339, 49.8175, 56.2639, 51.1657, 58.5953, 53.4129, 40.4637, 46.6031, 45.1000, 41.8719, 56.8796, 55.1694, 49.8153, 47.1625, 52.1326, 47.5162, 51.9194, 39.3999, 45.9432, 46.1512, 48.6690, 60.1282, 43.9159, 38.9637)
longitudes <- c(4.3517, 25.4858, 15.4720, 9.5018, 10.4515, 25.0136, -8.2439, -3.7492, 2.3522, 15.2000, 12.5674, 24.1052, 23.8813, 6.1296, 19.5033, 5.2913, 14.5501, 19.1451, -8.2245, 24.9668, 14.9955, 19.0402, 18.6435, 17.6791, 35.2433)
my_data$latitude <- latitude
my_data$longitude <-longitudes

c2 = my_data %>%
  select(-states)%>%
  names()



######################




data %>% str()


data %>% summary()


data %>% head()


my_data = read_excel("Donnée de gaz par pays.xlsx", sheet = 3)

 
c1 = data %>% 
  select(-année) %>% 
  select(-semestre) %>%
  names()




