# Import
data <- read.csv("DATA/TeePublic_review.csv", header = TRUE, encoding =  "UTF-8")

# Traitement des charactères spéciaux
data[] <- lapply(data, function(x) iconv(x, "UTF-8", "ASCII", sub = ""))

#Création de la colonne Saison
data$month <- as.numeric(data$month)
data$Saison <- cut(data$month, breaks = c(0, 3, 6, 9, 12), labels = c("Hiver", "Printemps", "Été", "Automne"))

#Création de la colonne Moment
data$review.label <- as.numeric(data$review.label)
data$type.note <- cut(data$review.label, breaks=c(0, 2, 3, 5), labels=c('Négative', 'Neutre', 'Positive'))

#Affichage des vente par pays
distribution_x <- table(data$store_location)
print(distribution_x)

#Filtre des ventes sur les USA
data <- subset(data, store_location == "US")
