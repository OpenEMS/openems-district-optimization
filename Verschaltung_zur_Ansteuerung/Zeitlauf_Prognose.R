setwd("C:/.../Projekte/Demonstrator") # Pfad anpassen!


Tag <- 100
Stunde <- 0

Stundenraster <- 24

Zeitschritt <- 1
Anzahl_Stunden <- 1

directory <- getwd()
Lastprofil <- read.csv2(paste0(directory,"/Lastprofile_1h.csv"))

Zeitstempel <- Lastprofil[(Tag*Stundenraster-Stundenraster+1):((Tag+1)*Stundenraster),1]
Temperatur <- Lastprofil[(Tag*Stundenraster-Stundenraster+1):((Tag+1)*Stundenraster),2]
Bedarf_Waerme <- Lastprofil[(Tag*Stundenraster-Stundenraster+1):((Tag+1)*Stundenraster),12]
Day_Ahead_Preis <- Lastprofil[(Tag*Stundenraster-Stundenraster+1):((Tag+1)*Stundenraster),13]
Bedarf_Strom <- Lastprofil[(Tag*Stundenraster-Stundenraster+1):((Tag+1)*Stundenraster),14]
Erzeugung_PV <- Lastprofil[(Tag*Stundenraster-Stundenraster+1):((Tag+1)*Stundenraster),15]
Prognose <- rbind(Zeitstempel, Temperatur, Bedarf_Waerme, Day_Ahead_Preis, Bedarf_Strom, Erzeugung_PV)
Prognose <- t(Prognose)
write.csv2(Prognose, file = "Prognose.csv")
print("Prognose ?bergeben")

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
  
  if (Stunde==20) {
    Lauf_Start <- Sys.time()
    Zeitstempel <- Lastprofil[((Tag+1)*Stundenraster-Stundenraster+1):((Tag+2)*Stundenraster),1]
    Temperatur <- Lastprofil[((Tag+1)*Stundenraster-Stundenraster+1):((Tag+2)*Stundenraster),2]
    Bedarf_Waerme <- Lastprofil[((Tag+1)*Stundenraster-Stundenraster+1):((Tag+2)*Stundenraster),12]
    Day_Ahead_Preis <- Lastprofil[((Tag+1)*Stundenraster-Stundenraster+1):((Tag+2)*Stundenraster),13]
    Bedarf_Strom <- Lastprofil[((Tag+1)*Stundenraster-Stundenraster+1):((Tag+2)*Stundenraster),14]
    Erzeugung_PV <- Lastprofil[((Tag+1)*Stundenraster-Stundenraster+1):((Tag+2)*Stundenraster),15]
    Prognose <- rbind(Zeitstempel, Temperatur, Bedarf_Waerme, Day_Ahead_Preis, Bedarf_Strom, Erzeugung_PV)
    Prognose <- t(Prognose)
    write.csv2(Prognose, file = "Prognose.csv")
    Lauf_Ende <- Sys.time()
    Stunde <- Stunde + floor(Lauf_Ende - Lauf_Start)
  }
  if (Stunde==24) {
    Stunde <- 0
    Tag <- Tag + 1
    if (Tag==366) {
      Tag <- 1
    }
    write.csv2(Prognose, file = "Prognose.csv")
    print("Prognose ?bergeben")
  }
}
 

