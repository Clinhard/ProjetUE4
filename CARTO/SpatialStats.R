#### Library ####
library (maps)
library (sp)
library (rgeos)
library (maptools)
library (rgdal)
library (cartography)
library (dplyr)
library (data.table)
library (stringr)
library(spdep)
library(DCluster)
library(BBmisc)
library(McSpatial)

#### Données ####
france_reg.spdf<-readOGR(dsn = "DATA/GEOFLA_2-2_DEPARTEMENT_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00235/GEOFLA_2-2_SHP_LAMB93_FR-ED161/DEPARTEMENT", layer="DEPARTEMENT")
Data_Radioactifs <- read.csv("DATA/dechets-declares-au-31-12-2016.csv", sep =";", stringsAsFactors = FALSE)
Data_Radioactifs <- Data_Radioactifs[which(Data_Radioactifs$DEPARTEMENT != "971" & Data_Radioactifs$DEPARTEMENT != "973" & Data_Radioactifs$DEPARTEMENT != "974"
                                           & Data_Radioactifs$DEPARTEMENT != "987" & Data_Radioactifs$DEPARTEMENT != "988" & Data_Radioactifs$DEPARTEMENT != "98"),]
Data_Radioactifs$ACTIVITE...Bq. <- as.numeric(gsub(",", ".", Data_Radioactifs$ACTIVITE...Bq.))
Data_Radioactifs$DEPARTEMENT[which(Data_Radioactifs$DEPARTEMENT == "1")] <- "01" 
Data_Radioactifs$DEPARTEMENT[which(Data_Radioactifs$DEPARTEMENT == "2")] <- "02" 
Data_Radioactifs$DEPARTEMENT[which(Data_Radioactifs$DEPARTEMENT == "4")] <- "04" 
Data_Radioactifs$DEPARTEMENT[which(Data_Radioactifs$DEPARTEMENT == "6")] <- "06" 
Data_Radioactifs$DEPARTEMENT[which(Data_Radioactifs$DEPARTEMENT == "7")] <- "07" 
Data_Radioactifs$DEPARTEMENT[which(Data_Radioactifs$DEPARTEMENT == "8")] <- "08" 
Data_Radioactifs <- na.omit(Data_Radioactifs)
Data_Polluants <- read.csv("DATA/registre-francais-des-emission-polluantes-reformatted.csv", sep =";", stringsAsFactors = FALSE, encoding = "UTF-8")
Data_Polluants <- Data_Polluants[which(Data_Polluants$Departement != "GUYANE" & Data_Polluants$Departement != "GUADELOUPE" &
                                         Data_Polluants$Departement != "MARTINIQUE" & Data_Polluants$Departement != "MAYOTTE"
                                       & Data_Polluants$Departement != "REUNION" & Data_Polluants$Departement != ""),]
Data_Polluants$Quantite <- as.numeric(gsub(",", ".", Data_Polluants$Quantite))
Data_Maladie <- read.csv("DATA/evenements.csv", sep =";", stringsAsFactors = FALSE)

#### Construction du data frame pour l'attribution des départments de chaque datasets & ceux de la carte (france_reg.spdf) ####
Dep_Datasets_To_Dep_Map <- data.frame(matrix(ncol = 4, nrow = 96))
colnames(Dep_Datasets_To_Dep_Map) <- c("DepartementsMaladie", "DepartementsPolluants", "DepartementsRadioactifs", "DepartementsMap")
Dep_Datasets_To_Dep_Map$DepartementsMaladie <- str_sort(colnames(Data_Maladie)[2:97])
Dep_Datasets_To_Dep_Map$DepartementsMap <-  str_sort(as.character(france_reg.spdf$NOM_DEPT))
Dep_Datasets_To_Dep_Map$DepartementsPolluants <- str_sort(unique(Data_Polluants$Departement))
Data_Radioactifs_Departement <- as.matrix(unique(Data_Radioactifs$DEPARTEMENT))
Data_Radioactifs_Departement[,1] <- str_sort(Data_Radioactifs_Departement[,1])
Data_Radioactifs_Departement_Name_found_FUN <- function(Data_Radioactifs_Departement_Number){
  as.character(france_reg.spdf$NOM_DEPT[which(as.character(france_reg.spdf$CODE_DEPT) == Data_Radioactifs_Departement_Number)])
}
Data_Radioactifs_Departement_Name_Found <- unlist(lapply(Data_Radioactifs_Departement,Data_Radioactifs_Departement_Name_found_FUN))
DataMatrix_Radioactifs_Departement <- data.frame(matrix(ncol=2,nrow = 78))
DataMatrix_Radioactifs_Departement[,1] <- Data_Radioactifs_Departement
DataMatrix_Radioactifs_Departement[,2] <- Data_Radioactifs_Departement_Name_Found
colnames(DataMatrix_Radioactifs_Departement) <- c("DepatementsRadioactifs","DepartementsMap")
Dep_Datasets_To_Dep_Map$DepartementsRadioactifs <- lapply(Dep_Datasets_To_Dep_Map$DepartementsMap,function(DepartementsMapNumber){
  if (DepartementsMapNumber %in% DataMatrix_Radioactifs_Departement$DepartementsMap){DataMatrix_Radioactifs_Departement$DepatementsRadioactifs[which(DataMatrix_Radioactifs_Departement$DepartementsMap == DepartementsMapNumber)]}
  else {NA}
})

#### Construction du data frame Maladie X - Polluants ####
Data_Maladie_Polluants_By_Dep <- data.frame(matrix(ncol = 3, nrow = 96))
colnames(Data_Maladie_Polluants_By_Dep) <- c("Departements","Malades","Quantite")
Data_Maladie_Polluants_By_Dep$Departements <- unlist(lapply(colnames(Data_Maladie)[2:97],function(DepartementsName){
  Dep_Datasets_To_Dep_Map$DepartementsMap[which(Dep_Datasets_To_Dep_Map$DepartementsMaladie == DepartementsName)]
}))
Data_Maladie_Polluants_By_Dep$Malades <- unlist(sapply(Data_Maladie[,2:97],sum))
Data_Maladie_Polluants_By_Dep$Quantite <- unlist(lapply(Data_Maladie_Polluants_By_Dep$Departements,function(DepartementsMapName){
  if (!is.na(Dep_Datasets_To_Dep_Map$DepartementsPolluants[which(Dep_Datasets_To_Dep_Map$DepartementsMap == DepartementsMapName)])){
    PolluantsDepNumber <- Dep_Datasets_To_Dep_Map$DepartementsPolluants[which(Dep_Datasets_To_Dep_Map$DepartementsMap == DepartementsMapName)]
    PolluantsQuantite_By_PolluantsDepNumber <- Data_Polluants$Quantite[which(Data_Polluants$Departement == PolluantsDepNumber)]
    PolluantsQuantite <- sum(PolluantsQuantite_By_PolluantsDepNumber)
    PolluantsQuantite
  }
  else {NA}
}))
Data_Maladie_Polluants_By_Dep <- na.omit(Data_Maladie_Polluants_By_Dep)

#### Construction du data frame Maladie X - Radioactifs ####
Data_Maladie_Radioactifs_By_Dep <- data.frame(matrix(ncol = 3, nrow = 96))
colnames(Data_Maladie_Radioactifs_By_Dep) <- c("Departements","Malades","Quantite")
Data_Maladie_Radioactifs_By_Dep$Departements <- unlist(lapply(colnames(Data_Maladie)[2:97],function(DepartementsName){
  Dep_Datasets_To_Dep_Map$DepartementsMap[which(Dep_Datasets_To_Dep_Map$DepartementsMaladie == DepartementsName)]
}))
Data_Maladie_Radioactifs_By_Dep$Malades <- unlist(sapply(Data_Maladie[,2:97],sum))
Data_Maladie_Radioactifs_By_Dep$Quantite <- unlist(lapply(Data_Maladie_Radioactifs_By_Dep$Departements,function(DepartementsMapName){
  if (!is.na(Dep_Datasets_To_Dep_Map$DepartementsRadioactifs[which(Dep_Datasets_To_Dep_Map$DepartementsMap == DepartementsMapName)])){
    RadioactifsDepNumber <- Dep_Datasets_To_Dep_Map$DepartementsRadioactifs[which(Dep_Datasets_To_Dep_Map$DepartementsMap == DepartementsMapName)]
    RadioactifsQuantite_By_RadioactifsDepNumber <- Data_Radioactifs$ACTIVITE...Bq.[which(Data_Radioactifs$DEPARTEMENT == RadioactifsDepNumber)]
    RadioactifsQuantite <- sum(RadioactifsQuantite_By_RadioactifsDepNumber)
    RadioactifsQuantite
  }
  else {NA}
}))
Data_Maladie_Radioactifs_By_Dep <- na.omit(Data_Maladie_Radioactifs_By_Dep)

#### Cartographie Maladie X ~ Polluants OR Radioactifs ####

bksPolluants <- quantile(Data_Maladie_Polluants_By_Dep$Quantite, seq(0,1,length.out = 9))
colsPolluants <- carto.pal(pal1 = "turquoise.pal", n1 = length(bksPolluants)-1)
bksRadioactifs <- quantile(Data_Maladie_Radioactifs_By_Dep$Quantite, seq(0,1,length.out = 9))
colsRadioactifs <- carto.pal(pal1 = "turquoise.pal", n1 = length(bksRadioactifs)-1)
plot(france_reg.spdf, col = "grey80")
propSymbolsChoroLayer(spdf = france_reg.spdf, spdfid = "NOM_DEPT", df = Data_Maladie_Polluants_By_Dep,                    
                      var = "Malades", var2 = "Quantite",
                      col = colsPolluants, border = "grey50", inches = 0.15,
                      breaks = bksPolluants, legend.var2.values.rnd = 0,
                      legend.var.title.txt = "Nombre\nde personnes atteintes de la maladie X", 
                      legend.var2.title.txt = "Nombre de polluants",
                      legend.var.pos = "bottomleft",
                      add=TRUE)
plot(france_reg.spdf, col = "grey80")
propSymbolsChoroLayer(spdf = france_reg.spdf, spdfid = "NOM_DEPT", df = Data_Maladie_Radioactifs_By_Dep,                    
                      var = "Malades", var2 = "Quantite",
                      col = colsRadioactifs, border = "grey50", inches = 0.15,
                      breaks = bksRadioactifs, legend.var2.values.rnd = 0,
                      legend.var.title.txt = "Nombre\nde personnes atteintes de la maladie X", 
                      legend.var2.title.txt = "Nombre de déchets radioactifs",
                      legend.var.pos = "bottomleft",
                      add=TRUE)

#### Préparation aux tests de Stone et de Moran ####
Data_Maladie_Polluants_By_Dep$expect=Data_Maladie_Polluants_By_Dep$Quantite*sum(Data_Maladie_Polluants_By_Dep$Malades)/sum(Data_Maladie_Polluants_By_Dep$Quantite)
Data_Maladie_Polluants_By_Dep$SMR=Data_Maladie_Polluants_By_Dep$Malades/Data_Maladie_Polluants_By_Dep$expect
Data_Maladie_Radioactifs_By_Dep$expect=Data_Maladie_Radioactifs_By_Dep$Quantite*sum(Data_Maladie_Radioactifs_By_Dep$Malades)/sum(Data_Maladie_Radioactifs_By_Dep$Quantite)
Data_Maladie_Radioactifs_By_Dep$SMR=Data_Maladie_Radioactifs_By_Dep$Malades/Data_Maladie_Radioactifs_By_Dep$expect
france_reg_MaladiePolluants.spdf <- france_reg.spdf
france_reg_MaladiePolluants.spdf$SMR <- unlist(lapply(france_reg_MaladiePolluants.spdf$NOM_DEPT,function(NomDep){
  if (!is.na(Dep_Datasets_To_Dep_Map$DepartementsPolluants[which(Dep_Datasets_To_Dep_Map$DepartementsMap==NomDep)])){
    Data_Maladie_Polluants_By_Dep$SMR[which(Data_Maladie_Polluants_By_Dep$Departements==NomDep)]
  }
  else{NA}
}))
france_reg_MaladiePolluants.spdf$SMR <- unlist(lapply(france_reg_MaladiePolluants.spdf$SMR,function(SMRValue){
  if (!is.na(SMRValue)){
    SMRValueNotNa <- na.omit(france_reg_MaladiePolluants.spdf$SMR)
    fractionNum <- SMRValue-min(SMRValueNotNa)
    fractionDenum <- max(SMRValueNotNa)-min(SMRValueNotNa)
    fraction <- fractionNum/fractionDenum
    fraction*8
  }
  else{NA}
}))
france_reg_MaladieRadioactifs.spdf <- france_reg.spdf
france_reg_MaladieRadioactifs.spdf$SMR <- unlist(lapply(france_reg_MaladieRadioactifs.spdf$NOM_DEPT,function(NomDep){
  if (!is.na(Dep_Datasets_To_Dep_Map$DepartementsRadioactifs[which(Dep_Datasets_To_Dep_Map$DepartementsMap==NomDep)])){
    Data_Maladie_Radioactifs_By_Dep$SMR[which(Data_Maladie_Radioactifs_By_Dep$Departements==NomDep)]
  }
  else{NA}
}))
france_reg_MaladieRadioactifs.spdf$SMR <- log10(france_reg_MaladieRadioactifs.spdf$SMR)
france_reg_MaladieRadioactifs.spdf$SMR[which(france_reg_MaladieRadioactifs.spdf$SMR == -Inf)] <- 0
france_reg_MaladieRadioactifs.spdf$SMR <- unlist(lapply(france_reg_MaladieRadioactifs.spdf$SMR,function(SMRValue){
  if (!is.na(SMRValue)){
    SMRValueNotNa <- na.omit(france_reg_MaladieRadioactifs.spdf$SMR)
    fractionNum <- SMRValue-min(SMRValueNotNa)
    fractionDenum <- max(SMRValueNotNa)-min(SMRValueNotNa)
    fraction <- fractionNum/fractionDenum
    fraction*8
  }
  else{NA}
}))
brksPolluants_Test=seq(0,9,1)
brksRadioactifs_Test=seq(0,9,1)
france_reg_MaladiePolluants_spplot <- spplot(france_reg_MaladiePolluants.spdf,"SMR",at=brksPolluants_Test,col.regions=grey.colors(9,start=1,end=0.1))
france_reg_MaladiePolluants_spplot
france_reg_MaladieRadioactifs_spplot <- spplot(france_reg_MaladieRadioactifs.spdf,"SMR",at=brksRadioactifs_Test,col.regions=grey.colors(9,start=1,end=0.1))
france_reg_MaladieRadioactifs_spplot

#### Test de Stones ####
# Polluants
regionPolluants <- which(france_reg_MaladiePolluants.spdf$NOM_DEPT=="LOZERE")
france_reg_MaladiePolluants_Stone <- data.frame(Observed = Data_Maladie_Polluants_By_Dep$Malades)
france_reg_MaladiePolluants_Stone <- cbind(france_reg_MaladiePolluants_Stone, Expected = Data_Maladie_Polluants_By_Dep$expect)
CoordinatesPolluants_List_x <- unlist(lapply(Data_Maladie_Polluants_By_Dep$Departements,function(DepName){
  coordinates(france_reg.spdf)[which(france_reg.spdf$NOM_DEPT == DepName),1]
}))
CoordinatesPolluants_List_y <- unlist(lapply(Data_Maladie_Polluants_By_Dep$Departements,function(DepName){
  coordinates(france_reg.spdf)[which(france_reg.spdf$NOM_DEPT == DepName),2]
}))
france_reg_MaladiePolluants_Stone <- cbind(france_reg_MaladiePolluants_Stone,x=CoordinatesPolluants_List_x,y=CoordinatesPolluants_List_y)
stone.stat(france_reg_MaladiePolluants_Stone, region=regionPolluants, lambda=1)
stone.test(Observed~offset(log(Expected)), france_reg_MaladiePolluants_Stone, model="poisson", R=99,
           region=regionPolluants, lambda=1)

# Radioactifs
regionRadioactifs <- which(france_reg_MaladieRadioactifs.spdf$NOM_DEPT=="CORREZE")
france_reg_MaladieRadioactifs_Stone <- data.frame(Observed = Data_Maladie_Radioactifs_By_Dep$Malades)
france_reg_MaladieRadioactifs_Stone <- cbind(france_reg_MaladieRadioactifs_Stone, Expected = Data_Maladie_Radioactifs_By_Dep$expect)
CoordinatesRadioactifs_List_x <- unlist(lapply(Data_Maladie_Radioactifs_By_Dep$Departements,function(DepName){
  coordinates(france_reg.spdf)[which(france_reg.spdf$NOM_DEPT == DepName),1]
}))
CoordinatesRadioactifs_List_y <- unlist(lapply(Data_Maladie_Radioactifs_By_Dep$Departements,function(DepName){
  coordinates(france_reg.spdf)[which(france_reg.spdf$NOM_DEPT == DepName),2]
}))
france_reg_MaladieRadioactifs_Stone <- cbind(france_reg_MaladieRadioactifs_Stone,x=CoordinatesRadioactifs_List_x,y=CoordinatesRadioactifs_List_y)
stone.stat(france_reg_MaladieRadioactifs_Stone, region=regionRadioactifs, lambda=1)
stone.test(Observed~offset(log(Expected)), france_reg_MaladieRadioactifs_Stone, model="poisson", R=99,
           region=regionRadioactifs, lambda=1)

#### Moran statistic ####
france_reg.nb <- poly2nb(france_reg.spdf)
col.W.test <- nb2listw(france_reg.nb, zero.policy=TRUE)
france_reg_MaladiePolluants_Moran<-data.frame(Observed = Data_Maladie_Polluants_By_Dep$Malades)
france_reg_MaladiePolluants_Moran<-cbind(france_reg_MaladiePolluants_Moran, Expected = Data_Maladie_Polluants_By_Dep$expect)
france_reg_MaladiePolluants_Moran_SMR <-data.frame(france_reg_MaladiePolluants_Moran$Observed/france_reg_MaladiePolluants_Moran$Expected)
france_reg_MaladieRadioactifs_Moran<-data.frame(Observed = unlist(lapply(france_reg.spdf$NOM_DEPT,function(DepName){
  if (DepName %in% Data_Maladie_Radioactifs_By_Dep$Departements){Data_Maladie_Radioactifs_By_Dep$Malades[which(Data_Maladie_Radioactifs_By_Dep$Departements == DepName)]}
  else{0}
})))
france_reg_MaladieRadioactifs_Moran<-cbind(france_reg_MaladieRadioactifs_Moran, Expected = unlist(lapply(france_reg.spdf$NOM_DEPT,function(DepName){
  if (DepName %in% Data_Maladie_Radioactifs_By_Dep$Departements){Data_Maladie_Radioactifs_By_Dep$expect[which(Data_Maladie_Radioactifs_By_Dep$Departements == DepName)]}
  else{0}
})))
france_reg_MaladieRadioactifs_Moran_SMR <-data.frame(france_reg_MaladieRadioactifs_Moran$Observed/france_reg_MaladieRadioactifs_Moran$Expected)

# Polluants
moranI.stat(data=france_reg_MaladiePolluants_Moran, listw=col.W.test, n=length(france_reg.nb), S0=Szero(col.W.test) )
moranI.stat(data=france_reg_MaladiePolluants_Moran, applyto="residuals", listw=col.W.test, n=length(france_reg.nb),
            S0=Szero(col.W.test) )
moranI.test(Observed~offset(log(Expected)), france_reg_MaladiePolluants_Moran, model="poisson", R=99,
            listw=col.W.test, n=length(france_reg.nb), S0=Szero(col.W.test) )

# Radioactifs
moranI.stat(data=france_reg_MaladieRadioactifs_Moran, listw=col.W.test, n=length(france_reg.nb), S0=Szero(col.W.test) )
moranI.stat(data=france_reg_MaladieRadioactifs_Moran, applyto="residuals", listw=col.W.test, n=length(france_reg.nb),
            S0=Szero(col.W.test) )
moranI.test(Observed~offset(log(Expected)), france_reg_MaladieRadioactifs_Moran, model="poisson", R=99,
            listw=col.W.test, n=length(france_reg.nb), S0=Szero(col.W.test) )
