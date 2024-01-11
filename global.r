# Import
data <- read.csv("DATA/TeePublic_review.csv", header = TRUE, encoding =  "UTF-8")
data[] <- lapply(data, function(x) iconv(x, "UTF-8", "ASCII", sub = ""))
