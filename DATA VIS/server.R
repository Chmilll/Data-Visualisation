library(DT)
library(shiny)
library(ggcorrplot)
library(ggplot2)
library(ggtext)
library(jpeg)
library(maps)
library(shinycssloaders)
library(shinydashboard)
library(plotly)
library(magrittr)
library(imager)
library(readxl)
library(tidyr)
library(DT)

data <- read_excel("XL DATA VIS.xlsx")

datatable(data)
tags$img(src = "photo.jpg", width = 600, height = 300)

my_data = read_excel("Donnée de gaz par pays.xlsx", sheet = 3)
my_data$states <- c("Belgium", "Bulgaria", "Czech Republic", "Denmark", "Germany", "Estonia", "Ireland", "Spain", "France", "Croatia", "Italy", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Sweden", "Bosnia and Herzegovina", "Turkey")

states_map <- map_data("world",c("Belgium", "Bulgaria", "Czech Republic", "Denmark", "Germany", "Estonia", "Ireland", "Spain", "France", "Croatia", "Italy", "Latvia", "Lithuania", "Luxembourg", "Hungary", "Netherlands", "Austria", "Poland", "Portugal", "Romania", "Slovenia", "Slovakia", "Sweden", "Bosnia and Herzegovina", "Turkey")
)
   
latitude <- c(50.8503, 42.7339, 49.8175, 56.2639, 51.1657, 58.5953, 53.4129, 40.4637, 46.6031, 45.1000, 41.8719, 56.8796, 55.1694, 49.8153, 47.1625, 52.1326, 47.5162, 51.9194, 39.3999, 45.9432, 46.1512, 48.6690, 60.1282, 43.9159, 38.9637)
longitudes <- c(4.3517, 25.4858, 15.4720, 9.5018, 10.4515, 25.0136, -8.2439, -3.7492, 2.3522, 15.2000, 12.5674, 24.1052, 23.8813, 6.1296, 19.5033, 5.2913, 14.5501, 19.1451, -8.2245, 24.9668, 14.9955, 19.0402, 18.6435, 17.6791, 35.2433)
my_data$latitude <- latitude
my_data$longitude <-longitudes

c1 = data %>% 
  select(-année) %>% 
  select(-semestre) %>%
  names()

function(input, output, session) {

  
  #############################################################################
  ###########                        DONNEES                       ############
  #############################################################################
  
  ############################### Données
  
   output$dataT <- renderDT({
     datatable(data)
   }) 
   
   
   
   
  ############################### Structure
   
   output$structure <- renderPrint(
     data %>% str()
   ) 
   
   
   
  ############################### Summary Stat
   
  output$summary <- renderPrint(
    data %>% summary()
  ) 
   
   
   
   
   #############################################################################
   ###########                    VISUALISATION                     ############
   #############################################################################
   
   
   ############################### Diagramme en barre

  output$head1 <- renderText(
    paste("Semestres ayant vu les plus hauts prix de", input$var2)
    
  )
  
  output$head2 <- renderText(
    paste("Semestres ayant vu les plus bas prix de", input$var2)
  )
  
   output$top5 <- renderTable({
     data %>% 
       select(semestre,année,input$var2) %>%
       arrange(desc(get(input$var2))) %>%
       head(5)%>%
       mutate(année = gsub("\\.00$", "", as.character(année)))
     
   })
   
   
   output$low5 <- renderTable({
     
     data %>% 
       select(semestre,année,input$var2) %>%
       arrange(get(input$var2)) %>%
       head(5)%>%
       mutate(année = gsub("\\.00$", "", as.character(année)))
   })

   

   
   output$bar <- renderPlotly({
    
     
     data %>%
       group_by(année) %>%
       mutate(total_sem1_sem2 = sum(get(input$var2))) %>%
       plot_ly() %>%
       add_bars(
         x = ~année,
         y = ~get(input$var2),
         color = ~semestre,
         colors = c("S1" = "blue", "S2" = "red"),
         marker = list(line = list(color = 'white', width = 0.7),
                       opacity = 0.7),  # Réduire l'opacité pour mieux visualiser les barres empilées
         hoverinfo = "y+name",  # Afficher l'information au survol
         text = ~paste(get(input$var2), "(", semestre, ")"),
         textposition = 'auto'  # Position du texte (auto pour déterminer automatiquement la meilleure position)
       ) %>%
       add_trace(
         x = ~année,
         y = ~total_sem1_sem2 + max(data[[input$var2]]) * 0.05,  # Ajuster ici le décalage vertical (ici, 5% de la valeur maximale de input$var2)
         text = ~total_sem1_sem2,
         textposition = 'auto',  # Position du texte (auto pour déterminer automatiquement la meilleure position)
         textfont = list(size = 10, color = 'black'),
         type = 'scatter',
         mode = 'text'
       ) %>%
       layout(
         title = paste("Prix par année du", input$var2),
         xaxis = list(title = "Année"),
         yaxis = list(title = input$var2),
         barmode = 'stack'  # Utilisez 'stack' pour empiler les barres
       )
   })

   
   
   
   
   ############################### Évolution du gaz naturel en UE, gaz naturel en France, électricité en UE et électricité en France
   
   output$line_chart_plot <- renderPlotly({
     # Line chart pour l'évolution du gaz naturel en UE, gaz naturel en France,
     # électricité en UE et électricité en France par rapport aux années
     
     data %>%
       plot_ly() %>%
       add_lines(x = ~année, y = ~`gaz naturel en UE`, name = "Gaz Naturel UE") %>%
       add_lines(x = ~année, y = ~`gaz naturel en France`, name = "Gaz Naturel France") %>%
       add_lines(x = ~année, y = ~`électricité en UE`, name = "Électricité UE") %>%
       add_lines(x = ~année, y = ~`électricité en France`, name = "Électricité France") %>%
       layout(title = "Évolution du gaz naturel en UE, gaz naturel en France, électricité en UE et électricité en France",
              xaxis = list(title = "Année"),
              yaxis = list(title = "Valeur"),
              showlegend = TRUE)
   })
   
   
   
   
   
   
   ############################### Distribution - Boxplot - Densité
   
   output$histplot <- renderPlotly({
     p1 <- data %>% plot_ly() %>% add_histogram(x = ~get(input$var1)) %>% 
       layout(xaxis = list(title = paste(input$var1))) 
     
     boxplot <- data %>% plot_ly() %>% add_boxplot(x = ~get(input$var1)) %>% 
       layout(yaxis = list(showticklabels = F))
     
     subplot(boxplot, p1, nrows = 2 , shareX = TRUE) %>%
       hide_legend() %>%
       layout(title = "Distribution - Boxplot - Densité",
              xaxis = list(title = input$var1),
              yaxis = list(title = "Fréquence"))
   })
   
   output$density_plot <- renderPlotly({
     p_density <- data %>% ggplot() +
       geom_density(aes(x = `gaz naturel en UE`, color = "Gaz Naturel UE"), alpha = 0.7) +
       geom_density(aes(x = `gaz naturel en France`, color = "Gaz Naturel France"), alpha = 0.7) +
       geom_density(aes(x = `électricité en UE`, color = "Électricité UE"), alpha = 0.7) +
       geom_density(aes(x = `électricité en France`, color = "Électricité France"), alpha = 0.7) +
       theme_minimal() +
       theme(legend.position = "top")
     
     ggplotly(p_density) %>%
       layout(title = "",
              xaxis = list(title = ""),
              yaxis = list(title = "densité de distribution"))
   })
   
   
  
   
   
   ############################### Matrice de corrélation
   
   
   output$cor <- renderPlotly({
     
     my_df <- data %>% 
       select(-année,-semestre)
     
     
     corr <- round(cor(my_df), 1)
     # matrice de corrélation
     p.mat <- cor_pmat(my_df)
     
     corr.plot <- ggcorrplot(
       corr,
       hc.order = TRUE,
       lab = TRUE,
       outline.col = "white",
       p.mat=p.mat
     )
     
     ggplotly(corr.plot)
     
   })
   
   
   
   ############################### Scatter Charts 
   
   output$scatter <- renderPlotly({
     p = data %>% 
       ggplot(aes(x=.data[[input$var3]], y=.data[[input$var4]])) +
       geom_point() +
       geom_smooth(method=get(input$fit)) +
       labs(title = paste("Relation entre les prix de", input$var3 , "et de" , input$var4),
            x = input$var3,
            y = input$var4) +
       theme(  plot.title = element_textbox_simple(size=10,
                                                   halign=0.5))
     ggplotly(p)
     
   })
   
   
   #############################################################################
   ###########                       MAP                            ############
   #############################################################################
   
   ############################### Onglet 1

   
   # Fonction pour filtrer les données en fonction de l'option sélectionnée
   filtered_datas <- reactive({
     switch(input$country_option,
            "Top 5 pays avec les plus hauts prix de Gaz" = head(arrange(my_data, desc(get(input$selectedYear))), 5),
            "Top 10 pays avec les plus hauts prix de Gaz" = head(arrange(my_data, desc(get(input$selectedYear))), 10),
            "Top 5 pays avec les plus faibles prix de Gaz" = head(arrange(my_data, get(input$selectedYear)), 5),
            "Top 10 pays avec les plus faibles prix de Gaz" = head(arrange(my_data, get(input$selectedYear)), 10),
            "Tous les pays" = my_data)
   })
    
   # Créer une carte Leaflet
   output$maps <- renderLeaflet({
     leaflet(data = filtered_datas()) %>%
       addTiles() %>%
       addMarkers(
         lng = ~longitude,
         lat = ~latitude,
         label = ~states,
         popup = ~paste("Year: ", input$selectedYear, "<br>",
                        "Energy Expense: ", round(get(input$selectedYear), 2))
       )
     })
   
   
   
   ############################### Onglet 2
   
   
   output$map <- renderLeaflet({
     selected_year <- as.character(input$selectedYear)  # Convertir l'année sélectionnée en chaîne de caractères
     expense_threshold <- input$expenseThreshold  # Récupérer la valeur du seuil d'effort
     
     filtered_data <- subset(my_data, as.numeric(get(selected_year)) > expense_threshold)  # Filtrer les pays en fonction de l'année et du seuil
     
     leaflet(data = filtered_data) %>%
       addTiles() %>%
       addMarkers(
         lng = ~longitude,
         lat = ~latitude,
         popup = ~paste("Country: ", states, "<br>",
                        sprintf("Year %s Energy Expenditure: ", selected_year),
                        round(as.numeric(get(selected_year)), 2)),
         label = ~paste("Country: ", states),
         clusterOptions = markerClusterOptions()
       )
   })
   
}

