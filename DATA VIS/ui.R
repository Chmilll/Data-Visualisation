library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(imager)
library(readxl)
library(leaflet)
library(leaflet.extras)
library(DT)
library(tidyr)
library(jpeg)

#######################################################################################################
###########################                      Phase 1                      ##############################        

##########################   Importation, analyse et modication des données   ########################################### 
####################################################################################################### 



#########################  Importation, analyse et modication de la donnée "data"  ################################### 


data <- read_excel("XL DATA VIS.xlsx", sheet = 1)
datatable(data)

names(data) <- c("année", "semestre", "gaz naturel en UE", "gaz naturel en France", "électricité en UE", "électricité en France")

c1 = data %>% 
  select(-année) %>% 
  select(-semestre) %>%
  names()


#########################  Importation, analyse et modication de la donnée "my_data"  ################################### 


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


################################        Importation d'une Photo       ################################### 



#######################################################################################################
################################                Phase 2               ###########################################        
                                     
################################    codage de l'apllication R shiny   ########################################### 
####################################################################################################### 



dashboardPage(
  
  dashboardHeader(title = "Evolution du prix domestique de gaz naturel et d'électricité",
                  titleWidth =600,
                  tags$li(class="dropdown", tags$a(href = "https://www.google.com/url?q=https://github.com/Chmilll/Data-Visu&sa=D&source=editors&ust=1698770312632730&usg=AOvVaw11csfh1J9XucTmMOzCEaIf", icon("github"), "Code source"), target = "_blank"),
                  tags$li(class="dropdown", tags$a(href = "https://www.linkedin.com/in/natha%C3%ABl-adrian-yatou-a0a244241/", icon("linkedin"), "Mon profil"), target = "_blank"),
                  tags$li(class="dropdown", tags$a(href = "https://www.facebook.com/nathael.yatou/", icon("facebook"), "Mes réseaux"), target = "_blank"),
                  tags$li(class="dropdown", tags$a(href = "https://twitter.com/AdrianYatou", icon("twitter"), "Mes réseaux"), target = "_blank")),
                  
  dashboardSidebar(
    #barre de menu
    sidebarMenu(
      id = "sidebar",
      
      menuItem("Donnée", tabName = "data", icon = icon("database")),
      menuItem(text = "Visualisation", tabName = "viz", icon = icon("chart-line"), badgeLabel = "Cliquez pour voir", badgeColor = "green"),
      
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'",selectInput(inputId = "var1", label = "Selectionnez une variable", choices = c1, selected = "gaz naturel en France")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends'",selectInput(inputId = "var2", label = "Selectionnez une donnée", choices = c1)),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation'", selectInput(inputId = "var3", label = "Selectionnez une variable X", choices = c1, selected = "gaz naturel en France")),
      conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation'",selectInput(inputId = "var4", label = "Selectionnez une variable Y", choices = c1, selected = "gaz naturel en UE")),
      menuItem(text = "Map", tabName = "map", icon = icon("map")),
      conditionalPanel("input.sidebar == 'map'",selectInput("selectedYear", "Choisissez l'année:",
                                                            choices = 2011:2022,
                                                            selected = 2022)),
      conditionalPanel("input.sidebar == 'map' && input.t5 == 'bb'", numericInput("expenseThreshold", "Seuil du prix du gaz (€/MWh)", value = 10)),
      conditionalPanel("input.sidebar == 'map' && input.t5 == 'cc'", radioButtons("country_option", "Sélectionner une classe:",
                                                              choices = c("Top 5 pays avec les plus hauts prix de Gaz" , "Top 10 pays avec les plus hauts prix de Gaz",
                                                                          "Top 5 pays avec les plus faibles prix de Gaz" , "Top 10 pays avec les plus faibles prix de Gaz", "Tous les pays"),
                                                              selected = "Tous les pays"))
    )
    
  ),
  
  
  
  
  dashboardBody(
    tabItems(
      ## Menu 1 : Donnée
      tabItem(tabName = "data",
              tabBox(id="t1", width = 12,
                     tabPanel("A propos de", icon = icon("address-card"),
                              fluidRow(
                       column(width = 8, tags$img(src = "photo.jpg", width=600, height = 300),
                              tags$br(),
                              tags$a("Différentes sources d'énergie"), align = "center"),
                       column(width = 4, tags$br(),
                              tags$p("Cette base de données contient les valeurs du prix de l'électricité et du gaz en €/MWh en France et dans l'UE entre 1995 et 2021, elle suit une évolution biannuel (2 fois par an).
  A noter que le gaz est moins efficient en terme énergétique que l'électricité, il est donc normal que son prix au MWh soit largement plus faible.
Nous allons représenter ces données via différentes méthodes de visualisation afin d'avoir une représentation concrète de ces évolutions.
Dans un second temps, nous représenterons la valeur du prix du gaz en €/MWh entre 2011 et 2022 dans les pays de l'UE sur une carte de l'Europe, ainsi, il sera possible de vérifier s'il y a une conséquence géographique sur le prix du gaz dans l'UE."))
                     )
                              ),
                     tabPanel("Donnée",DTOutput("dataT"),icon = icon("address-card")),
                     tabPanel("Structure", icon = icon("address-card"),verbatimTextOutput("structure")),
                     tabPanel("Summary stats", icon = icon("address-card"),verbatimTextOutput("summary")),
                     )),
      
      ## Menu 2 : Visualisation
      tabItem(tabName = "viz",
              tabBox(id= "t2", width=12,
                    tabPanel(title = "Energie domestique par année", value = "trends", icon = icon("chart-simple"),
                             fluidRow(tags$div(align = "center", box(tableOutput("top5"), title =textOutput("head1"), collapsible = TRUE, status = "primary", solidHeader = TRUE)),
                                      tags$div(align = "center", box(tableOutput("low5"), title =textOutput("head2"), collapsible = TRUE, status = "primary", solidHeader = TRUE))),
                             
                             withSpinner(plotlyOutput("bar"))),
                    tabPanel(title = "Graphique en courbe", value = "line_chart", icon = icon("chart-line"),
                             fluidRow(
                               column(width = 12,
                                      plotlyOutput("line_chart_plot")
                               )
                             )
                    ),
                    tabPanel(title = "Distribution", value = "distro", icon = icon("chart-mixed"),
                             fluidRow(
                               column(width = 12,
                             plotlyOutput("histplot")
     ),column(width = 12,
              plotlyOutput("density_plot")
     )
                    )),
                    tabPanel(title = "Matrice de correlation",plotlyOutput("cor")),
                    tabPanel(title = "Relation entre les prix domestiques en france et en UE-27", value = "relation", icon = icon("chart-scatter-bubble"),
                             radioButtons(inputId = "fit", label = "selection de la smooth method", choices = c("loess","lm"), selected = "lm", inline = TRUE),
                             withSpinner(plotlyOutput("scatter")))
                    )),
     ## Menu 3 : Map
      tabItem(tabName = "map",
              tabBox(id= "t5", width=12,
              tabPanel( title = "Carte du prix du gaz : Recherche par classe ", value = "cc", icon = icon("earth-europe"),
                               width = 12,
                               height = "100vh",
                               leafletOutput("maps")),       
              tabPanel( title = "Carte du prix du gaz : Recherche par seuil ", value = "bb", icon = icon("earth-europe"),
                   width = 12,
                   height = "100vh",
                   leafletOutput("map")))
      )
    )
  )
)

