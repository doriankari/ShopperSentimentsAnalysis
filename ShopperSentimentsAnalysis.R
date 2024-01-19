source("packages.R")
source("global.R")

# Interface utilisateur (UI)

ui <- dashboardPage(
  
  skin = "purple",
  
  dashboardHeader(title = "Shiny Dashboard"),
  
  
  dashboardSidebar(
    
    
    fileInput("fileInput", "Sélectionner un fichier", multiple = FALSE, accept = NULL),
    checkboxInput("fichierImport", "Validate the importation of data", value = FALSE),
    
    
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
        strong("<- Please upload the data from here "),
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
          p(em("Done by"), br(), em(" M. Amri Karim, M.Goumeziane Quentin, M. Rahon-Clos Paco"), br(), em("contact: amri.dk@hotmail.com / 
             quentin.goumeziane@groupe-gema.com / paco.rahon-clos.edu@groupe-gema.com"))
        )
      ),
      
      tabItem(
        tabName = "map",
        uiOutput("TestMap"),
        leafletOutput("map"),
      ),
      
      tabItem(
        tabName = "graphique",
        tabsetPanel(
          tabPanel("Repartition du sentiment", icon = icon("face-grin-stars"),
                   
                   uiOutput("plot1"),
                   plotOutput("sentiment_rep"),
                   downloadButton("downloadPlot", "Télécharger")
                   
          ),
          
          tabPanel("Repartition des notes", icon = icon("star-half-stroke"),
                   uiOutput("plot2"),
                   plotOutput("note_rep")

                   
          ),
          tabPanel("Repartition des pays", icon = icon("globe"),
                   uiOutput("plot3"),
                   plotOutput("pays_rep")

                   
          )
        )
      ),
      
      
      tabItem(
        tabName = "data",
        tabsetPanel(
          
          tabPanel("DATABASE", icon = icon("database"),
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
  # my_data <- reactive({
  #  data
  #})
  
 
  
  
  
  ###############################################################################
  
  
  # Charger les données
  data <- reactive({
    
    req(input$fileInput)
    
    infile <- input$fileInput
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath)
  })
  
  ###########################Traitement de la donnée############################
  
  
  datatraiter <- reactive ({
    data <- data() #reactive dans data
    
    # Traitement des charactères spéciaux
    data[] <- lapply(data, function(x) iconv(x, "UTF-8", "ASCII", sub = ""))
    
    #Création de la colonne Saison
    data$month <- as.numeric(data$month)
    data$Saison <- cut(data$month, breaks = c(0, 3, 6, 9, 12), labels = c("Hiver", "Printemps", "Été", "Automne"))
    
    #Création de la colonne Moment
    data$review.label <- as.numeric(data$review.label)
    data$type.note <- cut(data$review.label, breaks=c(0, 2, 3, 5), labels=c('Négative', 'Neutre', 'Positive'))
    
    #Filtre des colonnes
    data <- data[, c("store_location",
                     "latitude",
                     "longitude",
                     "date",
                     "month",
                     "title",
                     "review",
                     "review.label",
                     "Saison",
                     "type.note")]
    
    #Modification des noms de colonnes
    colnames(data)[colnames(data) == "store_location"] <- "Pays"
    colnames(data)[colnames(data) == "date"] <- "Année"
    colnames(data)[colnames(data) == "month"] <- "Mois"
    colnames(data)[colnames(data) == "title"] <- "Titre"
    colnames(data)[colnames(data) == "review"] <- "Commentaire"
    colnames(data)[colnames(data) == "review.label"] <- "Note"
    colnames(data)[colnames(data) == "type.note"] <- "Sentiment"
    
    data$longitude <- as.numeric(data$longitude)
    data$latitude <- as.numeric(data$latitude)
    
    return(data %>% as.data.frame())
  })
  
  # ################################################################################
  
  output$TestMap <- renderUI({
    if(input$fichierImport){
      selectInput("pays_sélectionné", "Sélectionnez un pays :",
                  choices = unique(datatraiter()$Pays,multiple = TRUE)
                  
      )
      
    }
  })
  
  #  mapppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppppp
  # Affichage de la carte
  output$map <- renderLeaflet({
    
    # Calculer le centre de la carte en fonction des coordonnées des pays filtrés
    center_lat <- mean(datatraiter()$latitude)
    center_lng <- mean(datatraiter()$longitude)
    
    leaflet(datatraiter()) %>%
      addTiles() %>%
      addMarkers(
        lat = ~latitude,
        lng = ~longitude,
        clusterOptions = markerClusterOptions(),
      )%>%
      setView(lng = center_lng, lat = center_lat, zoom = 4)
  })
  
  
  output$plot1 <- renderUI({
    if(input$fichierImport){
      selectInput("annee", "Année", choices = unique(datatraiter()$Année), multiple = TRUE)
      selectInput("pays", "Choix des pays", choices = unique(datatraiter()$Pays), multiple = TRUE)
      
    }
  })
  
  # Graphique: Répartition du sentiment
  output$sentiment_rep <- renderPlot({
    
    filtered_data <- subset(datatraiter(), Année %in% input$annee & Pays %in% input$district_type_crimes)
    
    ggplot(datatraiter(), aes(x = Sentiment, fill = Sentiment)) +
      geom_bar() +
      labs(title = "Répartition du Sentiment",
           x = "Sentiment",
           y = "Nombre d'avis") +
      theme_minimal()
  })
  
  
  
  
  output$plot2 <- renderUI({
    if(input$fichierImport){
      selectInput("annee", "Année", choices = unique(datatraiter()$Année), multiple = TRUE)
      selectInput("pays", "Choix des pays", choices = unique(datatraiter()$Pays), multiple = TRUE)
      
    }
  })
  
  # Graphique: Répartition des notes
  output$note_rep <- renderPlot({
    
    ggplot(datatraiter(), aes(x = as.factor(Note), fill = as.factor(Note))) +
      geom_bar() +
      labs(title = "Répartition des Notes",
           x = "Note",
           y = "Nombre d'avis") +
      theme_minimal()
  })
  
  
  
  output$plot3 <- renderUI({
    if(input$fichierImport){
      selectInput("annee", "Année", choices = unique(datatraiter()$Année), multiple = TRUE)
      selectInput("pays", "Choix des pays", choices = unique(datatraiter()$Pays), multiple = TRUE)
      
    }
  })
  
  # Graphique: Répartition des pays
  output$pays_rep <- renderPlot({
    
    #Filtre des ventes sur les USA
    data <- subset(datatraiter, Pays%in% c("US", "CA", "AU", "GB", "DE"))
    
    ggplot(data, aes(x = Pays, fill = Pays)) +
      geom_bar() +
      labs(title = "Répartition des Pays",
           x = "Pays",
           y = "Nombre d'avis") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Charge la table de données
  output$tableau1 <- renderDataTable({
    datatraiter()
  })
  
  
  # #Affichage des vente par pays
  output$tableau2 <- renderDT({
    distribution_x <- table(datatraiter()$Pays)
    distribution_x %>% as.data.frame()
  })
  
  output$SummaryData <- renderPrint({
    summary(datatraiter())
  })
  
  observeEvent(input$downloadPlot, {
    # Télécharger le graphique en tant qu'image PNG
    filename <- paste("graphique_", Sys.Date(), ".png", sep = "")
    ggsave(filename, plot = output$plot1, device = "png")
  })

  
}

# Prévisualisation de l'UI dans la console
shinyApp(ui = ui, server = server)