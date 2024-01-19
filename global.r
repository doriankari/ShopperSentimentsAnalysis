#Ancienne version pas interactive

# data <- read.csv("DATA/TeePublic_review.csv", header = TRUE, encoding =  "UTF-8")
# 
# # Traitement des charactères spéciaux
# data[] <- lapply(data, function(x) iconv(x, "UTF-8", "ASCII", sub = ""))
# 
# #Création de la colonne Saison
# data$month <- as.numeric(data$month)
# data$Saison <- cut(data$month, breaks = c(0, 3, 6, 9, 12), labels = c("Hiver", "Printemps", "Été", "Automne"))
# 
# #Création de la colonne Moment
# data$review.label <- as.numeric(data$review.label)
# data$type.note <- cut(data$review.label, breaks=c(0, 2, 3, 5), labels=c('Négative', 'Neutre', 'Positive'))
# 
# #Filtre des colonnes
# data <- data[, c("store_location",
#                   "latitude",
#                   "longitude",
#                   "date",
#                   "month",
#                   "title",
#                   "review",
#                   "review.label",
#                   "Saison",
#                   "type.note")]
# 
# #Modification des noms de colonnes
# colnames(data)[colnames(data) == "store_location"] <- "Pays"
# colnames(data)[colnames(data) == "date"] <- "Année"
# colnames(data)[colnames(data) == "month"] <- "Mois"
# colnames(data)[colnames(data) == "title"] <- "Titre"
# colnames(data)[colnames(data) == "review"] <- "Commentaire"
# colnames(data)[colnames(data) == "review.label"] <- "Note"
# colnames(data)[colnames(data) == "type.note"] <- "Sentiment"
# 
# #Affichage des vente par pays
# distribution_x <- table(data$Pays)
# print(distribution_x)
# 
# #Filtre des ventes sur les USA
# data <- subset(data, Pays %in% c("US", "CA", "AU", "GB", "DE"))
# 
# data$longitude <- as.numeric(data$longitude)
# data$latitude <- as.numeric(data$latitude)

print("global is empty")