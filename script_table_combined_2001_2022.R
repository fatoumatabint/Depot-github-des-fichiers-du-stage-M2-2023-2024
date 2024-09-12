setwd("/Users/binta/Desktop/Rapport de stage/Rapport IEDES/Script.R.Imaclim")

# Chargement des packages ----

library(janitor)
library(readxl)
library(dplyr)
library(stringr)
library(tidyverse)
library(writexl)
library(datawizard)
library(kableExtra)
library(gridExtra)
library(ggthemes)
library(RColorBrewer)
library(ggplot2)
library(purrr)
library(gganimate)
library(transformr)
library(plotly)

## Importer toutes les tables 

table_2001 <- read_excel("table_exports_imports_2001.xlsx")
table_2002 <- read_excel("table_exports_imports_2002.xlsx")
table_2003 <- read_excel("table_exports_imports_2003.xlsx")
table_2004 <- read_excel("table_exports_imports_2004.xlsx")
table_2005 <- read_excel("table_exports_imports_2005.xlsx")
table_2006 <- read_excel("table_exports_imports_2006.xlsx")
table_2007 <- read_excel("table_exports_imports_2007.xlsx")
table_2008 <- read_excel("table_exports_imports_2008.xlsx")
table_2009 <- read_excel("table_exports_imports_2009.xlsx")
table_2010 <- read_excel("table_exports_imports_2010.xlsx")
table_2011 <- read_excel("table_exports_imports_2011.xlsx")
table_2012 <- read_excel("table_exports_imports_2012.xlsx")
table_2013 <- read_excel("table_exports_imports_2013.xlsx")
table_2014 <- read_excel("table_exports_imports_2014.xlsx")
table_2015 <- read_excel("table_exports_imports_2015.xlsx")
table_2016 <- read_excel("table_exports_imports_2016.xlsx")
table_2017 <- read_excel("table_exports_imports_2017.xlsx")
table_2018 <- read_excel("table_exports_imports_2018.xlsx")
table_2019 <- read_excel("table_exports_imports_2019.xlsx")
table_2020 <- read_excel("table_exports_imports_2020.xlsx")
table_2021 <- read_excel("table_exports_imports_2021.xlsx")
table_2022 <- read_excel("table_exports_imports_2022.xlsx")


## Creer une table année dans les données

table_2001$Année <- "2001"
table_2002$Année <- "2002"
table_2003$Année <- "2003"
table_2004$Année <- "2004"
table_2005$Année <- "2005"
table_2006$Année <- "2006"
table_2007$Année <- "2007"
table_2008$Année <- "2008"
table_2009$Année <- "2009"
table_2010$Année <- "2010"
table_2011$Année <- "2011"
table_2012$Année <- "2012"
table_2013$Année <- "2013"
table_2014$Année <- "2014"
table_2015$Année <- "2015"
table_2016$Année <- "2016"
table_2017$Année <- "2017"
table_2018$Année <- "2018"
table_2019$Année <- "2019"
table_2020$Année <- "2020"
table_2021$Année <- "2021"
table_2022$Année <- "2022"

table_combined_2001_2022 <- bind_rows(table_2001,table_2002,table_2003,table_2004,table_2005,table_2006,
                                      table_2007,table_2008,table_2009,table_2010,table_2011,table_2012,
                                      table_2013,table_2014,table_2015,table_2016,table_2017,table_2018,
                                      table_2019,table_2020,table_2021,table_2022)


## réarranger le tableau et créeons le tabeau excel

table_combined_2001_2022 <- table_combined_2001_2022 %>% select(Année, everything())

write_xlsx(table_combined_2001_2022,"table_combined_2001_2022.xlsx")

## Créeons des graphiques 

table_combined_2001_2022_longer <- table_combined_2001_2022 %>%
  pivot_longer(cols = -c(imaclim_sector, Année), names_to = "Region", values_to = "Value")


table_combined_2001_2022_longer <- table_combined_2001_2022_longer %>%
  group_by(Region, imaclim_sector, Année) %>% 
  summarize(value_total = sum(Value, na.rm = TRUE)) %>% 
  group_by(Année) %>%  
  mutate(pourcentage = (value_total * 100) / sum(value_total)) 

fig <- plot_ly(table_combined_2001_2022_longer, x = ~Année, y = ~imaclim_sector, z = ~value_total, 
               color = ~Region, colors = c('blue', 'green', 'red', 'purple'),
               type = 'scatter3d', mode = 'markers', marker = list(size = 5))

# Personnaliser les axes et le titre
fig <- fig %>% layout(
  title = "Part du Commerce par région et par secteur de 2001 à 2022",
  scene = list(
    xaxis = list(title = "Année"),
    yaxis = list(title = "Secteur_Imaclim"),
    zaxis = list(title = "Valeur_totale") ,
    caption = "Source: Données BACI, 2009")
  )


# Afficher le graphique
fig

## 

for (i in 1:45) { 
  rgl.viewpoint(i, 20) 
  filename <- paste("pic", formatC(i, digits = 1, flag = "0"), ".png", sep = "") 
  rgl.snapshot(filename) } 

filename




table_combined_2001_2022_longer <- as.numeric(table_combined_2001_2022_longer$Region)

ggplot(table_combined_2001_2022_longer, aes(x = Année, y = Region, color = imaclim_sector)) +
  geom_line() +
  facet_wrap(~ cut(Region, breaks = 4)) +
  labs(title = "Graphique avec Facettes pour la Quatrième Dimension")















graphique_animated <- ggplot(table_combined_2001_2022_longer, aes(x = imaclim_sector, y = pourcentage, fill = imaclim_sector)) +
  geom_bar(stat = "identity", position = "stack", na.rm = TRUE) +
  facet_wrap(~ Region, scales = "free_x", ncol = 4) +
  labs(
    title = "Part du Commerce par Région et par Secteur en {frame_time}",
    x = "Région",
    y = "Valeur en pourcentage",
    fill = "Secteur",
    caption = "Source: Données BACI, 2022"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),  
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),  
    axis.text.y = element_text(size = 8),  
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.box.margin = margin(t = 10, r = 10, b = 10, l = 10), 
    legend.position = "right",
    strip.text = element_text(size = 10, face = "bold") 
  ) +
  
  
  # Ajout de l'animation selon la variable temporelle
  transition_reveal(Année)  # Assurez-vous que 'Année' correspond à la colonne de votre dataset

## 
table_combined_2001_2022_longer <- table_combined_2001_2022_longer %>%
  mutate(Année = as.numeric(Année))

# Animer avec une pause à la fin
facet_animated_final <- animate(graphique_animated, end_pause = 15, width = 800, height = 600)

# Voir l'animation
facet_animated_final


