source("packages.R")
source("global.R")

# Interface utilisateur (UI)
ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(title = "Shiny Dashboard"),
  
  
  
  
  dashboardSidebar(
    
    
    fileInput("fileInput", "Sélectionner un fichier", multiple = FALSE, accept = NULL),
    
    
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Graphique", tabName = "graphique", icon = icon("chart-line")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Summary", tabName = "Summary", icon = icon("chart-pie"))
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        h2("Shopper Sentiments, Analysis", align = "center"),
        br(),
        p("Explore TeePublic's universe with our dynamic dashboard! 
        Delve into 250,000+ reviews for sentiment insights, identify strategic store 
        locations through geospatial patterns, adapt to temporal trends, 
        and categorize feedback for actionable insights. 
        TeePublic's story unfolds.", align = "center"),
        br(),
        div(
          style = "text-align: center;",
          tags$img(src = "A.jpg", height = "400px", width = "600px"),
          br(),
          br(),
          p(em("Done by"), br(), em(" Mr Amri Karim, Mr Goumeziane Quentin and Mr Rahon-Clos Paco"), br(), em("contact: amri.dk@hotmail.com / 
             quentin.goumeziane@groupe-gema.com / paco.rahon-clos.edu@groupe-gema.com"))
        )
      ),
      
      tabItem(
        tabName = "map",
        leafletOutput("heatMap")
      ),
      
      tabItem(
        tabName = "graphique",
        tabsetPanel(
          tabPanel("Repartition du sentiment", icon = icon("face-grin-stars"),
                   selectInput("annee", "Année", choices = unique(data$Année), multiple = TRUE),
                   selectInput("district_type_crimes", "Choix des pays", choices = unique(data$Pays), multiple = TRUE),          
                   plotOutput("sentiment_rep"),
                   
          ),
          
          tabPanel("Repartition des notes", icon = icon("star-half-stroke"),
                   plotOutput("note_rep")
                   
          ),
          tabPanel("Repartition des pays", icon = icon("globe"),
                   plotOutput("pays_rep")
                   
          )
        )
      ),
      
      
      tabItem(
        tabName = "data",
        tabsetPanel(
          
          tabPanel("DATABASE", icon = icon("database"),
                   
                   #selectInput("annee", "Année", choices = unique(data$Année), multiple = TRUE),
                   dataTableOutput("tableau1")
          ),
          
          tabPanel("Avis Par Pays", icon = icon("earth-americas"),
                   dataTableOutput("tableau2")
          )
        )
      ),
      
      tabItem(
        tabName = "Summary",
        verbatimTextOutput("SummaryData")
      )
    )
  )
)


# Serveur
server <- function(input, output) {
  
  options(shiny.maxRequestSize=100*1024^2)
  ################ancienne version import depuis global########################
  
  # Charger les données de base
  my_data <- reactive({
    data
  })
  
  # Affichage de la carte 
  output$heatMap <- renderLeaflet({
    req(my_data())
    
    leaflet() %>%
      addTiles() %>%
      addHeatmap(
        data = my_data(),
        lng = ~longitude,
        lat = ~latitude,
        blur = 20,
        max = 0.05
      )
  })
  
  ###############################################################################
  
  # 
  # # Charger les données
  # data <- reactive({
  #   
  #   req(input$fileInput)
  #   
  #   infile <- input$fileInput
  #   if (is.null(infile)) {
  #     return(NULL)
  #   }
  #   read.csv(infile$datapath)
  # })
  # 
  
  #################################################################################
  
  #  mapppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp 
  #   us_map <- leaflet(data) %>%
  #     addProviderTiles("Esri.WorldGrayCanvas") %>%
  #     setView(-95.7129, 37.0902, zoom = 4)
  #   
  #   
  #   us_map <- us_map %>%
  #     addCircleMarkers(
  #       radius = 5,
  #       color = "blue",
  #       fillOpacity = 0.8,
  #       popup = ~Sentiment,
  #       label = ~Sentiment,
  #       clusterOptions = markerClusterOptions()
  #     )
  # 
  # output$us_map <- renderLeaflet({
  #   us_map
  # })
  
  
  # Graphique: Répartition du sentiment
  output$sentiment_rep <- renderPlot({
    req(my_data())
    
    ggplot(my_data(), aes(x = Sentiment, fill = Sentiment)) +
      geom_bar() +
      labs(title = "Répartition du Sentiment",
           x = "Sentiment",
           y = "Nombre d'avis") +
      theme_minimal()
  })
  
  # Graphique: Répartition des notes
  output$note_rep <- renderPlot({
    req(my_data())
    
    ggplot(my_data(), aes(x = as.factor(Note), fill = as.factor(Note))) +
      geom_bar() +
      labs(title = "Répartition des Notes",
           x = "Note",
           y = "Nombre d'avis") +
      theme_minimal()
  })
  
  # Graphique: Répartition des pays
  output$pays_rep <- renderPlot({
    req(my_data())
    
    ggplot(my_data(), aes(x = Pays, fill = Pays)) +
      geom_bar() +
      labs(title = "Répartition des Pays",
           x = "Pays",
           y = "Nombre d'avis") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Charge la table de données
  output$tableau1 <- renderDataTable({
    my_data()
  })
  
  output$tableau2 <- renderDataTable({
    distribution_x %>% as.data.frame()
  })
  
  output$SummaryData <- renderPrint({
    summary(my_data())
  })
  
  
  
}

# Prévisualisation de l'UI dans la console
shinyApp(ui = ui, server = server)