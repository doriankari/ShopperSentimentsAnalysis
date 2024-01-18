source("packages.R")
source("global.R")

# Interface utilisateur (UI)
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Shiny Dashboard"),
  dashboardSidebar(
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Map", tabName = "map", icon = icon("map")),
      menuItem("Graphique", tabName = "graphique", icon = icon("chart-line"),
               tabPanel("Sentiment Distribution", icon = icon("chart-bar"),
                        selectInput("annee", "Année", choices = unique(data$Année), multiple = TRUE)
               ),
               tabPanel("pays", icon = icon("exclamation-triangle"),
                        selectInput("pays", "Choisir les pays", choices = unique(data$Pays), multiple = TRUE)
               )
      ),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Summary", tabName = "Summary", icon = icon("chart-pie"))
    )
  ),
  body = dashboardBody(
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
      p(em("Done by"), br(), em(" Mr Amri Karim and Mr Goumeziane Quentin"), br(), em("contact: amri.dk@hotmail.com / 
             quentin.goumeziane@groupe-gema.com"))
    )
  ),
  tabItem(
    tabName = "map",
    leafletOutput("map")
  ),
  tabItem(
    tabName = "graphique",
    tabsetPanel(
      tabPanel("Sentiment Distribution", icon = icon("chart-bar"),
               selectInput("Mois", "Année", choices = unique(data$Année)),
               plotOutput("TW_plot"), multiple = TRUE),
      tabPanel("Top 10 Type de Crime", icon = icon("exclamation-triangle"),
               selectInput("district_type_crimes", "Choix des pays", choices = unique(data$Pays), multiple = TRUE),
               plotOutput("T10C")
      )
    )
  ),
  tabItem(
    tabName = "data",
    dataTableOutput("tableau")
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
  # Charger les données
  my_data <- reactive({
    data
  })
  
  # Charge la table de données
  output$tableau <- renderDataTable({
    my_data()
  })
  
  # Charge le gra^hique en barres
  output$sentiment_bar_plot <- renderPlot({
    pays_in <- input$pays
    
    # Filtrer les données en fonction des pays sélectionnés
    filtered_data <- my_data()[my_data()$Pays %in% pays_in, ]
    
    # Utilisez ggplot pour créer le graphique en barres
    ggplot(filtered_data, aes(x = Sentiment, y = Sentiment, fill = Sentiment)) +
      geom_bar(stat = "identity") +
      labs(title = "Sentiment Distribution",
           x = "Votre axe X",
           y = "Votre axe Y") +
      theme_minimal()
  })
}

# Prévisualisation de l'UI dans la console
shinyApp(ui = ui, server = server)

##############################################################################
# body <- dashboardBody(
#   fluidRow(
#     box(
#       title = "Box title", width = 6, status = "primary",
#       "Box content"
#     ),
#     box(
#       status = "warning", width = 6,
#       "Box content"
#     )
#   ),
#   
#   fluidRow(
#     column(width = 4,
#            box(
#              title = "Title 1", width = NULL, solidHeader = TRUE, status = "primary",
#              "Box content"
#            ),
#            box(
#              width = NULL, background = "black",
#              "A box with a solid black background"
#            )
#     ),
#     
#     column(width = 4,
#            box(
#              title = "Title 3", width = NULL, solidHeader = TRUE, status = "warning",
#              "Box content"
#            ),
#            box(
#              title = "Title 5", width = NULL, background = "light-blue",
#              "A box with a solid light-blue background"
#            )
#     ),
#     
#     column(width = 4,
#            box(
#              title = "Title 2", width = NULL, solidHeader = TRUE,
#              "Box content"
#            ),
#            box(
#              title = "Title 6", width = NULL, background = "maroon",
#              "A box with a solid maroon background"
#            )
#     )
#   )
# )
##############################################################################
# map_chart$init_server("map_chart",
#                       df = countries_stats,
#                       countries_geo_data = countries_geo_data,
#                       y = selected_year,
#                       m = selected_month)
# 
# countryMap = map_chart$ui("map_chart"),
##############################################################################

# #source("global.R")

# 
# # Define UI
# ui <- dashboardPage(
#  skin = "red",
#   # Theme Selector (optional)
#  
#   
#   # Header
#   dashboardHeader(title = "My Shiny Dashboard"),
#   
#   # Sidebar
#   dashboardSidebar(
#     sidebarMenu(
#       # Sidebar items (add more as needed)
#       menuItem("Home", tabName = "home", icon = icon("home")),
#       menuItem("Data Exploration", tabName = "explore", icon = icon("chart-bar")),
#       menuItem("Visualizations", tabName = "visualize", icon = icon("chart-line")),
#       menuItem("Settings", tabName = "settings", icon = icon("cogs"))
#     )
#   ),
#   
#   # Body
#   dashboardBody(
#     tabItems(
#       # Tab 1: Home
#       tabItem(
#         tabName = "home",
#         h2("Welcome to My Shiny Dashboard"),
#         p("This is a template for your Shiny dashboard. Customize it as per your requirements.")
#       ),
#       
#       # Tab 2: Data Exploration
#       tabItem(
#         tabName = "explore",
#         h2("Data Exploration"),
#         # Add content for data exploration
#         # (e.g., tables, data summaries, etc.)
#       ),
#       
#       # Tab 3: Visualizations
#       tabItem(
#         tabName = "visualize",
#         h2("Visualizations"),
#         # Add content for visualizations
#         # (e.g., plots, charts, maps, etc.)
#       ),
#       
#       # Tab 4: Settings
#       tabItem(
#         tabName = "settings",
#         h2("Settings"),
#         # Add content for settings
#         # (e.g., input controls, configuration options, etc.)
#       )
#     )
#   )
# )
# 
# # Define server logic (empty for now)
# server <- function(input, output) {
#   # Add server logic as needed
# }
# 
# # Create Shiny app
# shinyApp(ui = ui, server = server)
# 
# 
# 
# 
# 
# 





###################################Ancien Appli#################################
# ui <- dashboardPage(
#   
#   shinythemes::themeSelector(),
#   
#   dashboardHeader(title = "Dashboard CRIME LA"),
#   dashboardSidebar(
#     sidebarMenu(
#       pickerInput(
#         "city_select", label = "Villes",
#         choices = unique(data$AREA.NAME),
#         options = list('actions-box' = TRUE),
#         multiple = TRUE,
#         selected = unique(data$AREA.NAME)[1:4]
#       ),
#       pickerInput(
#         "crime_select", label = "Crimes",
#         choices = c("Cambriolage" = "BURGLARY", 
#                     "Viol" = "RAPE, FORCIBLE", 
#                     "Vol" = "ROBBERY", 
#                     "Enlèvement" = "KIDNAPPING", 
#                     "Véhicule volé" = "VEHICLE - STOLEN"),
#         options = list('actions-box' = TRUE),
#         multiple = TRUE,
#         selected = c("BURGLARY", "ROBBERY")
#       ),
#       
#       menuItem("Home", tabName = "home", icon = icon("home")),
#       menuItem("Map", tabName = "map", icon = icon("map")),
#       menuItem("Graphs", tabName = "graphique", icon = icon("chart-line")),
#       menuItem("Data Set", tabName = "data_set", icon = icon("table")),
#       menuItem("Summary", tabName = "Summary", icon = icon("chart-pie"))
#     )
#   ),
#   
#   
#   dashboardBody(
#     tabItems(
#       tabItem(
#         tabName = "home",
#         h2("USA, Los Angeles Crimes Data: 2020 To 2023", align = "center"),
#         br(),
#         p("This application offers an in-depth exploration of crime data in Los Angeles,
#   providing a comprehensive analysis of trends, variations, and criminal 
#   patterns prevalent within this metropolis.
#   Its aim is to deliver a nuanced and contextualized understanding of 
#   the city's criminal landscape through interactive visualizations and 
#   data analysis tools.", align = "center"),
#         br(),
#         div(
#           style = "text-align: center;",
#           tags$img(src = "LAPD.png", height = "400px", width = "600px"),
#           br(),
#           br(),
#           p(em("Done by Mr Amri Karim."),br(),em("contact: amri.dk@hotmail.com"))
#         )
#       ),
#       tabItem(
#         tabName = "map",
#         leafletOutput("map")
#       ),
#       tabItem(
#         tabName = "graphique",
#         tabsetPanel(
#           tabPanel("Armes Utilisées", icon = icon("gun"),
#                    selectInput("district_type_weapon", "Select a district", choices = unique(data$AREA.NAME)),
#                    plotOutput("TW_plot")
#                    
#           ),
#           
#           tabPanel("Sexe des Victimes", icon = icon("venus-mars"),
#                    selectInput("district_sex_victims", "Select a district", choices = unique(data_ethnie$AREA.NAME)),
#                    plotOutput("TSV_plot"),
#           ),
#           
#           tabPanel("Ethnicité des Victimes", icon = icon("globe-americas"),
#                    
#                    selectInput("district_selector_ethnie", "Select a district", choices = unique(data_ethnie$AREA.NAME)),
#                    plotOutput("TEV_plot")
#           ),
#           
#           tabPanel("Top 10 Type de Crime", icon = icon("exclamation-triangle"),
#                    selectInput("district_type_crimes", "Select a district", choices = unique(data$AREA.NAME)),
#                    plotOutput("T10C")
#                    
#           )
#         )
#       ),
#       tabItem(
#         tabName = "data_set",
#         dataTableOutput("tableau"),
#       ),
#       tabItem(
#         tabName = "Summary",
#         verbatimTextOutput("SummaryData")
#       )
#     )
#   )
# )
# #
# # Server logic
# server <- function(input, output) {
#   
#   # Création d'une variable réactive pour filtrer les données en fonction des sélections de ville et de crime
#   filtered_data <- reactive({
#     selected_cities <- input$city_select
#     selected_crimes <- input$crime_select
#     
#     # Filtrer les données en fonction des sélections
#     filtered <- data[data$AREA.NAME %in% selected_cities & data$Crm.Cd.Desc %in% selected_crimes, ]
#     return(filtered)
#   })
#   
#   output$map <- renderLeaflet({
#     leaflet(filtered_data()) %>%
#       addTiles() %>%
#       addCircleMarkers(clusterOptions = markerClusterOptions(
#         radius = 5, # Taille du marqueur
#         color = "blue", # Couleur du marqueur
#         fillOpacity = 0.8, # Opacité du remplissage
#         popup = ~filtered_data()$Crm.Cd.Desc, # Texte du popup
#         label = ~filtered_data()$Crm.Cd.Desc # Texte affiché lorsque vous survolez le marqueur
#       ))
#   })
#   ###### graphs###########################################
#   output$TW_plot <- renderPlot({
#     
#     top_weapons <- data %>%
#       filter(AREA.NAME == input$district_type_weapon)%>%
#       group_by(Weapon.Desc) %>%
#       summarise(nbre_occurences = n()) %>%
#       filter(!is.na(Weapon.Desc) & Weapon.Desc != "") %>%
#       top_n(10, nbre_occurences)
#     
#     ggplot(top_weapons, aes(x = reorder(Weapon.Desc, -nbre_occurences), y = nbre_occurences)) +
#       geom_bar(stat = "identity", fill = "darkblue") +
#       labs(title = paste("Top 10 Weapons - District", input$district_type_weapons),
#            x = "Types d'armes",
#            y = "Nbre d'occurrences") +
#       theme_classic() +
#       theme(axis.text.x = element_text(size = 5, angle = 55, hjust = 1, color = "darkblue"))
#   })
#   
#   output$TSV_plot <- renderPlot({
#     
#     filter_data_ethnie <- data_ethnie %>%
#       filter(AREA.NAME == input$district_sex_victims)
#     
#     ggplot(filter_data_ethnie, aes(x = Vict.Sex, y = nbre_victime, fill = Vict.Sex)) +
#       geom_bar(stat = "identity", position = "dodge") +
#       labs(title = "Nombre de Crimes par Sexe des Victimes",
#            x = "Sexe des Victimes", y = "Nombre de Crimes") +
#       scale_fill_manual(values = c("blue", "red","grey")) +  
#       theme_minimal()  
#   })
#   
#   # Création d'un graphique pour l'ethnie
#   output$TEV_plot <- renderPlot({
#     
#     filter_data_ethnie <- data_ethnie %>%
#       filter(AREA.NAME == input$district_selector_ethnie)
#     
#     ggplot(filter_data_ethnie, aes(x = Vict.Descent, y = nbre_victime, fill = Vict.Descent)) +
#       geom_bar(stat = "identity", position = "dodge") +
#       labs(title = "Nombre de Crimes par Ethnie des Victimes",
#            x = "Ethnie des Victimes", y = "Nombre de Crimes") +
#       scale_fill_manual(values = c("blue", "red","grey","pink")) +  
#       theme_minimal()  
#   })
#   
#   
#   
#   # Création d'un graphique à barres pour représenter le top 10 des types de crime
#   
#   output$T10C <- renderPlot({
#     data_top_crimes <- data %>%
#       filter(AREA.NAME == input$district_type_crimes)
#     
#     # Filtrer pour obtenir les top 10 types de crime
#     top_10_crime <- data_top_crimes %>%
#       group_by(Crm.Cd.Desc) %>%
#       summarise(count = n()) %>%
#       top_n(10, count)
#     
#     
#     ggplot(top_10_crime, aes(x = reorder(Crm.Cd.Desc, -count), y = count)) +
#       geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
#       
#       
#       labs(title = "Top 10 des Types de Crime",
#            x = "Type de Crime", y = "Nombre de Crimes") +
#       theme_minimal() +
#       coord_flip()
#     
#     
#   })
#   
#   
#   
#   output$tableau <- renderDataTable({
#     filtered_data()
#   })
#   
#   output$SummaryData <- renderPrint({
#     summary(filtered_data())
#   })
# }
# 
# 
# shinyApp(ui = ui, server = server)
