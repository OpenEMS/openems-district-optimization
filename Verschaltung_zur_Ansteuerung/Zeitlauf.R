
Zeitschritt <- 1
Anzahl_Stunden <- 1
starttime <- Sys.time()

while (Zeitschritt) {
  time <- Sys.time()
  while ((starttime + Anzahl_Stunden) >= time) {
    time <- Sys.time()
  }
  Anzahl_Stunden <- Anzahl_Stunden + Zeitschritt
  print(Sys.time())
}


