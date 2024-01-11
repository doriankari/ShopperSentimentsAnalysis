# Import

print("data load")

data <- read.csv("DATA/CRIME_DATA_LA.csv", header = TRUE, sep = ";")

data <- data %>%
  filter(LAT >= 24.396308 & LAT <= 49.384358 & LON >= -125.000000 & LON <= -66.934570)

data_pop <- data %>%
  filter(data$Vict.Sex %in% c("F", "M","X"))

data_ethnie <- data_pop %>%
  group_by(AREA.NAME, Vict.Descent, Vict.Sex) %>%
  mutate( Vict.Descent = case_when(
    Vict.Descent == "W" ~ "White",
    Vict.Descent == "B" ~ "Black",
    Vict.Descent == "H" ~ "Hispanic",
    TRUE ~ "Other"
  ),
  Vict.Sex = case_when(
    Vict.Sex == "F" ~ "Ladies",
    Vict.Sex == "M" ~ "Mens",
    TRUE ~ "Other"
  )) %>%
  summarise(nbre_victime = n())

print("data charged")

