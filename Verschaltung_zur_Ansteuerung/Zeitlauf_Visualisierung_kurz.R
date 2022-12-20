library(httr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(svMisc)

setwd("C:/.../Projekte/Demonstrator") # Pfad anpassen!

#
# Farbpalette <- read_excel(paste0(getwd(),"/Farbpalette.xlsx"),
#                           sheet = "Standardfarben")
# colnames(Farbpalette)[1]<-"Bezeichnung"
# #Bezeichnung entfernen,da aktuell nicht vorhanden
# Farbpalette<-Farbpalette[,-5]
# #Leerzeile entfernen, aktuell ist keine Farbe hinterlegt
# Farbpalette<-na.omit(Farbpalette)
# 
# hellgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="hellgruen",-1],maxColorValue = 255)
# 
# mittelgruen <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="mittelgruen",-1],maxColorValue = 255)
# 
# dunkelgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="dunkelgruen",-1],maxColorValue = 255)
# 
# rot1<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="rot1",-1],maxColorValue = 255)
# grau1 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau1",-1],maxColorValue = 255)
# grau3 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau3",-1],maxColorValue = 255)
# grau4 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau4",-1],maxColorValue = 255)


#Eingabe <- menu(c("DA", "HLZF", "Flexa"), title="Bitte w?hlen Sie eine Betriebsweise?")

#Tag <- 141
#Stunde <- 0

#vec <- c("0", NA, NA, NA, "4", NA, NA, NA, "8", NA, NA, NA, "12", NA, NA, NA, "16", NA, NA, NA, "20", NA, NA, NA)
#vec <- c(0, NA, NA, NA, 4, NA, NA, NA, 8, NA, NA, NA, 12, NA, NA, NA, 16, NA, NA, NA, 20, NA, NA, NA)
#vec <- c(vec, vec)

#Stundenraster <- 24

Zeitschritt <- 1
Anzahl_Stunden <- 1

Intervall_Prog <- 0
Stund_Prog_erneuern <- 6

# Daten einlesen
directory <- getwd()
Prognose <- read.csv2(paste0(directory,"/Prognose.csv"))
Fahrplan <- read.csv2(paste0(directory,"/Fahrplan.csv"))
Fahrplan <- Fahrplan[,2:(dim(Fahrplan)[2])] # In beiden csv-Datein befindet sich ein Z?hlvektor "X". Einer muss entfernt werden
# Leermatrix_F zum Anpassen der Fahrplanl?nge an die Prognose (24 h auf 48 h)
Leermatrix_F <- matrix(-100, dim(Fahrplan)[1], dim(Fahrplan)[2])
Leermatrix_F <- as.data.frame(Leermatrix_F)
colnames(Leermatrix_F) <- colnames(Fahrplan)
# Leermatrix_P zum Anpassen der Prognosenerweiterung Prog an die Visualisierungsbreite
Leermatrix_P <- matrix(-100, Stund_Prog_erneuern, dim(Fahrplan)[2])
Leermatrix_P <- as.data.frame(Leermatrix_P)
colnames(Leermatrix_P) <- colnames(Fahrplan)
# Leermatrix_Fp zum Anpassen des neuen Fahrplans an die Prognosenbreite
Leermatrix_Fp <- matrix(-100, dim(Fahrplan)[1], dim(Prognose)[2])
Leermatrix_Fp <- as.data.frame(Leermatrix_Fp)
colnames(Leermatrix_Fp) <- colnames(Prognose)
# Anpassen des Fahrplans an Prognose in Visualisierung
Fahrplan <- rbind(Fahrplan, Leermatrix_F)
Visualisierung <- cbind(Prognose, Fahrplan)
Visualisierung$Eth_TS1[24] <- 0.5*10000/860*(70-50)
#Visualisierung$Eth_TS1[25] <- 0.5*10000/860*(70-50)
# Leerzeile zur Darstellung der fortlaufenden Visualisierung
Leerzeile <- (Visualisierung[1,]*0)-100
Leerzeile <- as.data.frame(Leerzeile)
colnames(Leerzeile) <- colnames(Visualisierung)


# Warten bis Sekunde 0 (alle 10 Sekunden Beginn m?glich)
Start <- 1
# while (Start) {
#   Init_time <- Sys.time()
#   Init_time <- as.character(Init_time)
#   Last_Char <- nchar(Init_time)
#   Start <- substr(Init_time, Last_Char, Last_Char)
#   Start <- as.numeric(Start)
# }
starttime <- Sys.time()

while (Zeitschritt) {
  time <- Sys.time()
  while ((starttime + Anzahl_Stunden) >= time) {
    time <- Sys.time()
  }
  Anzahl_Stunden <- Anzahl_Stunden + Zeitschritt
  print(paste0("Tag: ",Tag, ", ", Stunde,":00 Uhr"))
  Stunde <- Stunde + 1
  
  Intervall_Prog <- Intervall_Prog + 1
  if (Intervall_Prog==Stund_Prog_erneuern) {
    Intervall_Prog <- 0
    Prog <- Prognose[1:Stund_Prog_erneuern,]
    Prog <- cbind(Prog, Leermatrix_P)
   #colnames(Prog) <- colnames(Visualisierung)
    Visualisierung[(dim(Visualisierung)[1]-Stund_Prog_erneuern+1):dim(Visualisierung)[1],] <- Prog
    Prognose <- Prognose[(Stund_Prog_erneuern+1):(dim(Prognose)[1]),]
  }
  
  # Gasboiler ein/aus
  if (Visualisierung$Pth_GaBo1_TS1[2]>0) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais3/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais3/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }
  
  # BHKW ein/aus
  if (Visualisierung$Pth_CHP1_TS1[2]>0) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais2/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais2/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }

  # Pufferfüllstand xxx Prozent
  if (Visualisierung$Eth_TS1[2]>6000/860*(70-50)*0) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais6/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais6/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }
  # Pufferfüllstand xxx Prozent
  if (Visualisierung$Eth_TS1[2]>6000/860*(70-50)*0.2) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais7/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais7/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }
  # Pufferfüllstand xxx Prozent
  if (Visualisierung$Eth_TS1[2]>6000/860*(70-50)*0.4) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais0/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais0/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }
  # Pufferfüllstand xxx Prozent
  if (Visualisierung$Eth_TS1[2]>6000/860*(70-50)*0.6) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais1/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais1/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }
  # Pufferfüllstand xxx Prozent
  if (Visualisierung$Eth_TS1[2]>6000/860*(70-50)*0.8) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais4/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais4/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }
  # Pufferfüllstand xxx Prozent
  if (Visualisierung$Pth_TS1_Demth1[2]>0) {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais5/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":true}", encode = "json")
  } else {
    r <- POST("http://192.168.0.124:8084/rest/channel/Relais5/OnOff",authenticate("Admin", "admin"),
              body = "{\"value\":false}", encode = "json")
  }
  
  
  xAchse <- c((Stunde-1), vec[2:48])
  
  par(mfrow=c(3,1))
  plot(1:48, Visualisierung$Eth_TS1, ylim=c(0,250), col='black', type="s", lwd=2, panel.first = grid(), xlab="Zeit in Stunden",ylab="Energie [kWh]", xaxt='n')
  axis(1, at = 1:48, labels = xAchse)
  lines(1:48, Visualisierung$Bedarf_Waerme, col='red', type="s", lwd=2)
  lines(1:48, Visualisierung$Pth_CHP1_TS1, col=dunkelgruen, type="s", lwd=2)
  lines(1:48, Visualisierung$Pth_GaBo1_TS1, col='blue', type="s", lwd=2)
  legend("topright", legend=c("Pufferinhalt", "Wärmebedarf", "BHKW", "Gasboiler"), col=c('black', 'red', dunkelgruen, 'blue'), lty=1:1)
  title("Wärmesektor")
  #
  plot(1:48, Visualisierung$Bedarf_Strom, ylim=c(0,120), col='red', type="s", lwd=2, panel.first = grid(), xlab="Zeit in Stunden",ylab="Leistung [kW]", xaxt='n')
  axis(1, at = 1:48, labels = xAchse)
  lines(1:48, Visualisierung$Erzeugung_PV, col=hellgruen, type="s", lwd=2)
  lines(1:48, Visualisierung$Pel_CHP1_PubGel1 + Visualisierung$Pel_CHP1_Demel1, col=dunkelgruen, type="s", lwd=2)
  legend("topright", legend=c("Strombedarf", "PV", "BHKW"), col=c('red', hellgruen, dunkelgruen), lty=1:1)
  title("Stromsektor") 
  #
  plot(1:48, Visualisierung$Day_Ahead_Preis, ylim=c(0,100), type="s", lwd=2, panel.first = grid(), xlab="Zeit in Stunden",ylab="DA-Preis [???/MWh], Leistung [kW]", xaxt='n')
  axis(1, at = 1:48, labels = xAchse)
  lines(1:48, Visualisierung$Pel_CHP1_PubGel1 + Visualisierung$Pel_CHP1_Demel1, col=hellgruen, type="s", lwd=2)
  lines(1:48, Visualisierung$Pel_CHP1_PubGel1, col=dunkelgruen, type="s", lwd=2)
  legend("topright", legend=c("Spotpreis", "Eigenverbrauch", "Einspeisung"), col=c('black', dunkelgruen, hellgruen), lty=1:1)
  title("Strommarkt")
  
  Visualisierung <- rbind(Visualisierung[2:(dim(Visualisierung)[1]),],Leerzeile)
  vec <- c(vec[2:48], vec[1])
  
  if (Stunde==4) {
    Prognose_neu <- read.csv2(paste0(directory,"/Prognose.csv"))
    Prognose <- rbind(Prognose, Prognose_neu)
  }
  if (Stunde==12) {
    Fahrplan_neu <- read.csv2(paste0(directory,"/Fahrplan.csv"))
    Fahrplan_neu <- Fahrplan_neu[,2:(dim(Fahrplan_neu)[2])] # In beiden csv-Datein befindet sich ein Z?hlvektor "X". Einer muss entfernt werden
    Fahrplan <- cbind(Leermatrix_Fp, Fahrplan_neu)
    Visualisierung[(dim(Visualisierung)[1]-36+1):(dim(Visualisierung)[1]-12),] <- Visualisierung[(dim(Visualisierung)[1]-36+1):(dim(Visualisierung)[1]-12),] + 
                                                                                  Fahrplan + matrix(100, dim(Fahrplan)[1], dim(Fahrplan)[2])
    #Visualisierung$Eth_TS1[12] <- 0.5*10000/860*(70-50)
    Visualisierung$Eth_TS1[36] <- 0.5*10000/860*(70-50)
    #Visualisierung$Eth_TS1[37] <- 0.5*3000/860*(70-50)
  }
  if (Stunde==24) {
    Stunde <- 0
    Tag <- Tag + 1
    #Eingabe <- menu(c("DA", "HLZF", "Flexa"), title="Bitte w?hlen Sie eine Betriebsweise?")
    starttime <- Sys.time()
    Anzahl_Stunden <- 1
    if (Tag==366) {
      Tag <- 1
    }
  }
}


