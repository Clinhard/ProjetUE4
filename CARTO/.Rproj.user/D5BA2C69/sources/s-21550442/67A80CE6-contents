library(tidyverse)
library(rgdal)
library(dplyr)
library(shiny)
library(leaflet)
library(rgeos)
library(rgdal)
library(devtools)
library(tidyr)
df <- read_delim("DATA/Polluant.csv", ";", escape_double = FALSE, trim_ws = TRUE)
names(df)
df<- df[!is.na(df$COORDONNEE),]

df<- tidyr::separate(data= df,
                     col= COORDONNEE,
                     into=c("Latitude", "Longitude"),
                     sep=",",
                     remove=FALSE)

df$Latitude <- stringr::str_replace_all(df$Latitude, "[(]", "")
df$Longitude <- stringr::str_replace_all(df$Longitude, "[)]", "")


df$Latitude <- as.numeric(df$Latitude)
df$Longitude <- as.numeric(df$Longitude)

saveRDS(df, "DATA/polluant.rds")

sample_data <- df[c(1:100),]
saveRDS(sample_data, "DATA/sample_polluant.rds")
df <- readRDS("DATA/sample_polluant.rds")

ui <- fluidPage(
  leafletOutput("mymap", height = 500))



server <- function(input,output, session){
  
  data <- reactive({
    x <- df
  })
  output$mymap<- renderLeaflet({
    df <- data()
    m <- leaflet(df) %>%
      addTiles() %>%
      addCircles(lng = ~Longitude,
                 lat = ~Latitude,
                 weight = 1,
                 color = "#FF0000",
                 radius = ~sqrt(QUANTITE) * 70)%>%
    addMarkers(lng = ~Longitude,
               lat = ~Latitude,
               popup = paste("departement:",df$NOM_DEPARTEMENT, "<br>",
                             "quantite produite:", df$QUANTITE,"<br>",
                             "type de dechet:",df$NOM_POLLUANT))
                 
      

    m
  
  })
  
}
shinyApp(ui=ui, server=server)