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
      #1
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
      
      #2
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

