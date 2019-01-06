library(tidyverse)
library(rgdal)
library(dplyr)
library(shiny)
library(leaflet)
library(rgeos)
library(rgdal)
library(devtools)
library(tidyr)
df <- read_delim("DATA/Departement.csv", ";", escape_double = FALSE, trim_ws = TRUE)
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

saveRDS(df, "DATA/data.rds")

sample_data <- df[c(1:100),]
saveRDS(sample_data, "DATA/sample_data.rds")
df <- readRDS("DATA/sample_data.rds")

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
      addMarkers(lng = ~Longitude,
                 lat = ~Latitude,
                 popup = paste("departement",df$NOM_DEPARTEMENT, "<br>",
                               "superficie:", df$SUPERFICIE))
    m
    
  
  })
  
}
shinyApp(ui=ui, server=server)