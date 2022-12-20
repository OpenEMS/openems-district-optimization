library(httr)
library(readxl)
library(ggplot2)
library(gridExtra)
library(svMisc)


Ansteuerung <- 1 # 1=An, 0=Aus

if (Ansteuerung == 1) {
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais0/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais1/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais2/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais3/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais4/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais5/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais6/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais7/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais8/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais9/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais10/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais11/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais12/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":true}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais13/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais14/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
  r <- POST("http://192.168.101.1:8084/rest/channel/Relais15/OnOff",authenticate("Admin", "admin"),
            body = "{\"value\":false}", encode = "json")
}




#
Farbpalette <- read_excel(paste0(getwd(),"/Farbpalette.xlsx"),
                          sheet = "Standardfarben")
colnames(Farbpalette)[1]<-"Bezeichnung"
#Bezeichnung entfernen,da aktuell nicht vorhanden
Farbpalette<-Farbpalette[,-5]
#Leerzeile entfernen, aktuell ist keine Farbe hinterlegt
Farbpalette<-na.omit(Farbpalette)

hellgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="hellgruen",-1],maxColorValue = 255)

mittelgruen <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="mittelgruen",-1],maxColorValue = 255)

dunkelgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="dunkelgruen",-1],maxColorValue = 255)

rot1<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="rot1",-1],maxColorValue = 255)
grau1 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau1",-1],maxColorValue = 255)
grau3 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau3",-1],maxColorValue = 255)
grau4 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau4",-1],maxColorValue = 255)


#Eingabe <- menu(c("DA", "HLZF", "Flexa"), title="Bitte w?hlen Sie eine Betriebsweise?")

Tag <- 120
Stunde <- 0

#vec <- c("0", NA, NA, NA, "4", NA, NA, NA, "8", NA, NA, NA, "12", NA, NA, NA, "16", NA, NA, NA, "20", NA, NA, NA)
vec <- c(0, NA, NA, NA, 4, NA, NA, NA, 8, NA, NA, NA, 12, NA, NA, NA, 16, NA, NA, NA, 20, NA, NA, NA)
vec <- c(vec, vec)

Stundenraster <- 24

Zeitschritt <- 1
Anzahl_Stunden <- 1

Intervall_Prog <- 0
Stund_Prog_erneuern <- 6

# Daten einlesen
directory <- getwd()
Simulation <- read.csv2(paste0(directory,"/Sim_BHKW_PV_EEX_365.csv"))
# Prognose <- read.csv2(paste0(directory,"/Prognose.csv"))
# Fahrplan <- read.csv2(paste0(directory,"/Fahrplan.csv"))
# Fahrplan <- Fahrplan[,2:(dim(Fahrplan)[2])] # In beiden csv-Datein befindet sich ein Z?hlvektor "X". Einer muss entfernt werden
# Leermatrix_F zum Anpassen der Fahrplanl?nge an die Prognose (24 h auf 48 h)
Leermatrix_S <- matrix(-100, dim(Simulation)[1], dim(Simulation)[2])
Leermatrix_S <- as.data.frame(Leermatrix_S)
colnames(Leermatrix_S) <- colnames(Simulation)
# Leermatrix_P zum Anpassen der Prognosenerweiterung Prog an die Visualisierungsbreite
# Leermatrix_P <- matrix(-100, Stund_Prog_erneuern, dim(Fahrplan)[2])
# Leermatrix_P <- as.data.frame(Leermatrix_P)
# colnames(Leermatrix_P) <- colnames(Fahrplan)
# # Leermatrix_Fp zum Anpassen des neuen Fahrplans an die Prognosenbreite
# Leermatrix_Fp <- matrix(-100, dim(Fahrplan)[1], dim(Prognose)[2])
# Leermatrix_Fp <- as.data.frame(Leermatrix_Fp)
# colnames(Leermatrix_Fp) <- colnames(Prognose)
# Anpassen des Fahrplans an Prognose in Visualisierung
Simulation <- rbind(Simulation, Leermatrix_S)
Visualisierung <- Simulation[(Tag*24-24+1):dim(Simulation)[1],]
Visualisierung$Eth_TS1[24] <- 0.5*6000/860*(70-50)
#Visualisierung$Eth_TS1[25] <- 0.5*6000/860*(70-50)
# Leerzeile zur Darstellung der fortlaufenden Visualisierung
Leerzeile <- (Visualisierung[1,]*0)-100
Leerzeile <- as.data.frame(Leerzeile)
colnames(Leerzeile) <- colnames(Visualisierung)


# Warten bis Sekunde 0 (alle 10 Sekunden Beginn m?glich)
Start <- 1
while (Start) {
  Init_time <- Sys.time()
  Init_time <- as.character(Init_time)
  Last_Char <- nchar(Init_time)
  Start <- substr(Init_time, Last_Char, Last_Char)
  Start <- as.numeric(Start)
}
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
   #  Intervall_Prog <- 0
   #  Prog <- Prognose[1:Stund_Prog_erneuern,]
   #  Prog <- cbind(Prog, Leermatrix_P)
   # #colnames(Prog) <- colnames(Visualisierung)
   #  Visualisierung[(dim(Visualisierung)[1]-Stund_Prog_erneuern+1):dim(Visualisierung)[1],] <- Prog
   #  Prognose <- Prognose[(Stund_Prog_erneuern+1):(dim(Prognose)[1]),]
  }
  
  if (Ansteuerung==1) {  
    # BHKW ein/aus
    if (Visualisierung$Pth_CHP1_TS1[1]>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais4/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais4/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    
    # Gasboiler ein/aus
    if (Visualisierung$Pth_GaBo1_Demth1[1]>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais5/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais5/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
  
    
    # Pufferfüllstand xxx Prozent
    if (Visualisierung$Eth_TS1[1]>6000/860*(70-50)*0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais6/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais6/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    # Pufferfüllstand xxx Prozent
    if (Visualisierung$Eth_TS1[1]>6000/860*(70-50)*0.333) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais7/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais7/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    # Pufferfüllstand xxx Prozent
    if (Visualisierung$Eth_TS1[1]>6000/860*(70-50)*0.666) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais0/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais0/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    
    # Puffer Entnahme
    if (Visualisierung$Pth_TS1_Demth1[1]>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais1/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais1/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    
    # Bedarf thermisch
    if ((Visualisierung$Pth_TS1_Demth1[1] + Visualisierung$Pth_GaBo1_Demth1[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais2/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais2/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    
    # PV Produktion
    if ((Visualisierung$Pel_PV1_Demel1[1] + Visualisierung$Pel_PV1_Demel2[1] + Visualisierung$Pel_PV1_PubGel1[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais3/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais3/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    # Strombedarf Bewohner
    if ((Visualisierung$Pel_PV1_Demel1[1] + Visualisierung$Pel_CHP1_Demel1[1] + Visualisierung$Pel_PubGel1_Demel1[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais9/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais9/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    # Strombedarf E-Mobilität
    if ((Visualisierung$Pel_PV1_Demel2[1] + Visualisierung$Pel_CHP1_Demel2[1] + Visualisierung$Pel_PubGel1_Demel2[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais10/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais10/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    # Strombedarf Gesamt
    if ((Visualisierung$Pel_PV1_Demel1[1] + Visualisierung$Pel_CHP1_Demel1[1] + Visualisierung$Pel_PubGel1_Demel1[1] + Visualisierung$Pel_PV1_Demel2[1] + Visualisierung$Pel_CHP1_Demel2[1] + Visualisierung$Pel_PubGel1_Demel2[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais8/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais8/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    # Strombezug
    if ((Visualisierung$Pel_PubGel1_Demel1[1] + Visualisierung$Pel_PubGel1_Demel2[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais12/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais12/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    # Strombezug oder PV Eigenstrom
    if ((Visualisierung$Pel_PV1_Demel1[1] + Visualisierung$Pel_PV1_Demel2[1] + Visualisierung$Pel_PubGel1_Demel1[1] + Visualisierung$Pel_PubGel1_Demel2[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais13/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais13/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    # Einspeisung BHKW
    if ((Visualisierung$Pel_CHP1_PubGel1[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais15/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais15/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
    
    # Einspeisung PV oder BHKW
    if ((Visualisierung$Pel_PV1_PubGel1[1] + Visualisierung$Pel_CHP1_PubGel1[1])>0) {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais14/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":true}", encode = "json")
    } else {
      r <- POST("http://192.168.101.1:8084/rest/channel/Relais14/OnOff",authenticate("Admin", "admin"),
                body = "{\"value\":false}", encode = "json")
    }
  }
  
  
  
  
  xAchse <- c((Stunde-1), vec[2:48])
  
  par(mfrow=c(3,1))
  plot(1:48, (Visualisierung$Eth_TS1[1:48]/139.5*100), ylim=c(0,110), col='black', type="s", lwd=1, panel.first = grid(), xlab="Zeit in Stunden",ylab="Energie [kWh]", xaxt='n')
  axis(1, at = 1:48, labels = xAchse)
  lines(1:48, Visualisierung$Pth_TS1_Demth1[1:48] + Visualisierung$Pth_GaBo1_Demth1[1:48], col='red', type="s", lwd=2)
  lines(1:48, Visualisierung$Pth_CHP1_TS1[1:48], col=dunkelgruen, type="s", lwd=2)
  lines(1:48, Visualisierung$Pth_GaBo1_Demth1[1:48], col='blue', type="s", lwd=2)
  legend("topright", legend=c("Pufferinhalt", "Wärmebedarf", "BHKW", "Gasboiler"), col=c('black', 'red', dunkelgruen, 'blue'), lty=1:1)
  title("Wärmesektor")
  #
  Strombedarf_Bewohner <- Visualisierung$Pel_CHP1_Demel1[1:48] + Visualisierung$Pel_PV1_Demel1[1:48] + Visualisierung$Pel_PubGel1_Demel1[1:48] 
  Strombedarf_EMob <- Visualisierung$Pel_CHP1_Demel2[1:48] + Visualisierung$Pel_PV1_Demel2[1:48] + Visualisierung$Pel_PubGel1_Demel2[1:48]
  Erzeugung_PV <- Visualisierung$Pel_PV1_Demel1[1:48] + Visualisierung$Pel_PV1_Demel2[1:48] + Visualisierung$Pel_PV1_PubGel1[1:48]
  Erzeugung_BHKW <- Visualisierung$Pel_CHP1_Demel1[1:48] + Visualisierung$Pel_CHP1_Demel2[1:48] + Visualisierung$Pel_CHP1_PubGel1[1:48]
  Netzbezug <- Visualisierung$Pel_PubGel1_Demel1[1:48] + Visualisierung$Pel_PubGel1_Demel2[1:48] 
  
  plot(1:48, Strombedarf_Bewohner, ylim=c(0,30), col=hellgruen, type="s", lwd=2, panel.first = grid(), xlab="Zeit in Stunden",ylab="Leistung [kW]", xaxt='n')
  axis(1, at = 1:48, labels = xAchse)
  lines(1:48, Strombedarf_EMob, col=dunkelgruen, type="s", lwd=2)
  lines(1:48, Erzeugung_PV, col='orange', type="s", lwd=2)
  lines(1:48, Erzeugung_BHKW, col='blue', type="s", lwd=2)
  lines(1:48, Netzbezug*(1), col='red', type="s", lwd=2)
  lines(1:48, rep(0,48), col='black', type="s", lwd=1)
  legend("topright", legend=c("Bewohner", "E-Mob", "PV", "BHKW", "Netzbezug"), col=c(hellgruen, dunkelgruen, 'orange', 'blue', 'red'), lty=1:1)
  title("Stromsektor") 
  #
  plot(1:48, Visualisierung$epex[1:48]*1000, col='red', ylim=c(-40,80), type="s", lwd=2, panel.first = grid(), xlab="Zeit in Stunden",ylab="DA-Preis [Euro/MWh], Leistung [kW]", xaxt='n')
  axis(1, at = 1:48, labels = xAchse)
  lines(1:48, Visualisierung$Pel_CHP1_PubGel1[1:48] + Visualisierung$Pel_CHP1_Demel1[1:48] + Visualisierung$Pel_CHP1_Demel2[1:48], col=hellgruen, type="s", lwd=2)
  lines(1:48, Visualisierung$Pel_CHP1_PubGel1[1:48], col=dunkelgruen, type="s", lwd=2)
  lines(1:48, rep(0,48), col='black', type="s", lwd=1)
  legend("topright", legend=c("Spotpreis", "Erzeugung", "Einspeisung"), col=c('red', hellgruen, dunkelgruen), lty=1:1)
  title("Strommarkt")
  
  Visualisierung <- rbind(Visualisierung[2:(dim(Visualisierung)[1]),],Leerzeile)
  vec <- c(vec[2:48], vec[1])
  
  if (Stunde==4) {
    # Prognose_neu <- read.csv2(paste0(directory,"/Prognose.csv"))
    # Prognose <- rbind(Prognose, Prognose_neu)
  }
  if (Stunde==12) {
    # Fahrplan_neu <- read.csv2(paste0(directory,"/Fahrplan.csv"))
    # Fahrplan_neu <- Fahrplan_neu[,2:(dim(Fahrplan_neu)[2])] # In beiden csv-Datein befindet sich ein Z?hlvektor "X". Einer muss entfernt werden
    # Fahrplan <- cbind(Leermatrix_Fp, Fahrplan_neu)
    # Visualisierung[(dim(Visualisierung)[1]-36+1):(dim(Visualisierung)[1]-12),] <- Visualisierung[(dim(Visualisierung)[1]-36+1):(dim(Visualisierung)[1]-12),] + 
    #                                                                               Fahrplan + matrix(100, dim(Fahrplan)[1], dim(Fahrplan)[2])
    # #Visualisierung$Eth_TS1[12] <- 0.5*10000/860*(70-50)
    # Visualisierung$Eth_TS1[36] <- 0.5*10000/860*(70-50)
    # #Visualisierung$Eth_TS1[37] <- 0.5*3000/860*(70-50)
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


