#----Lade alle n?tigen Pakages----
#----Lade alle noetigen Pakages----
library(Root)
library(CHP)
library(Electrolyseur)
library(HePu)
library(TS)
library(Bat)
library(Demth)
library(Demel)
library(Demfuel)
library(PubGel)
library(PubGth)
library(PubGfuel)
library(GaBo)
library(PV)
library(Prodth)
library(Wado)
library(Sandbox)
library(lpSolve)
library(Rglpk)
# library(stringr)
# library(dplyr)
# library(ggplot2)
# library(tidyr)
# library(kableExtra)
# library(readxl)
#
setwd("C:/.../Projekte/Demonstrator") # Pfad anpassen!

#
#----Ggf R-Umgebung laden----
setwd("C:/.../Projekte/Demonstrator") # Pfad anpassen!
#
#----Lastprofile erzeugen----
directory <- getwd()
Prognose <- read.csv2(paste0(directory,"/Prognose.csv"))  

Waerme_60min <- Prognose[,4]
Strom_60min <- Prognose[,6]
DA_Preis_60min <- Prognose[,5]/1000
PV_Erz_60min <- Prognose[,7]
#
#----Gasnetz erzeugen----
Gas_Netz <- new.PubGfuel()
#variables(Gas_Netz)<- "P_fuel_plus"
maxP_fuel_plus(Gas_Netz) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
minP_fuel_plus(Gas_Netz) <- 0
#
#----Stromnetz erzeugen----
Strom_Netz <- new.PubGel()
#variables(pubgel)<- c("P_el_minus","P_el_plus")
maxP_el_minus(Strom_Netz) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
minP_el_minus(Strom_Netz) <- 0
maxP_el_plus(Strom_Netz) <- -1
minP_el_plus(Strom_Netz) <- 0
#
#----PV erzeugen----
PV <- new.PV()
#
#----Gasboiler erzeugen----
Gasboiler <- new.GaBo()
info(Gasboiler) <- list(name='Gasboiler')
maxP_th_plus(Gasboiler) <- 100
effMaxP_th_plus(Gasboiler) <- 0.9
#
#----BHKW erzeugen----
BHKW <- new.CHP()
info(BHKW) <- list(name='BHKW')
maxP_el_plus(BHKW) <- 100
minP_el_plus(BHKW) <- 50
effMaxP_el_plus(BHKW) <- 0.38095
effMinP_el_plus(BHKW) <- 0.38095
effMaxP_th_plus(BHKW) <- 0.55238
effMinP_th_plus(BHKW) <- 0.55238
price_maintenance(BHKW) <- 0#.03    #Wartungskosten
minDowntime(BHKW) <- 0
minRuntime(BHKW) <- 2
initial_state(BHKW) <- 0
initial_stateEndurance(BHKW) <- 0
#
#----Puffer erzeugen----
Puffer <- new.TS()
info(Puffer) <- list(name="Puffer")
volume(Puffer) <- 6000 
maxTemp(Puffer) <- 70
minTemp(Puffer) <- 50
maxP_th_plus(Puffer) <- 1000   
maxP_th_minus(Puffer) <- 1000  
effMaxP_th_minus(Puffer) <- 1
eff_losses(Puffer) <- 0.941677 #berechnet #0.99756 
percentageStart(Puffer) <- 0.5
percentageEnd(Puffer) <- 0.5
#
# #----Batterie erzeugen----
# Batterie <- new.Bat()
# maxE_el(Batterie) <- 100
# maxP_el_plus(Batterie) <- 50
# maxP_el_minus(Batterie) <- 50
# effMaxP_el_minus(Batterie) <- 1
# eff_losses(Batterie) <- 1
# percentageStart(Batterie) <- 0.5
# percentageEnd(Batterie) <- 0.5
#
#----Waermebedarf erzeugen----
Waermebedarf <- new.Demth()
info(Waermebedarf) <- list(name='Waermebedarf')
#
#----Strombedarf erzeugen----
Strombedarf <- new.Demel()
info(Strombedarf) <- list(name='Strombedarf')
#
#----Simulationsstart----
Start_time <- Sys.time()
#####
#####For-Schleife beginn###################################
#####Tagessimulationen
sim_day <- 1
sim_day_start <- sim_day
days_to_sim <- 1
stepsize <- 60 # in Minuten
timestep <- 24*60/stepsize
timestep_15min <- 96
timestep_div <- 15*timestep/timestep_15min
lptest2 <- list()
status <- list()
#
#----Simulationsumgebung----
Umgebung <- new.Sandbox()
name(Umgebung) <- "Umgebung"
info(Umgebung) <- list(name="E_World")
timegrid(Umgebung) <- rep(stepsize,timestep) 
#
#
#----Profil auf Zeitschritt anpassen----
Waerme_15min <- transform_loadprofile_sum(Waerme_60min, 60)
Strom_15min <- transform_loadprofile_sum(Strom_60min, 60)
PV_Erz_15min <- transform_loadprofile_sum(PV_Erz_60min, 60)
DA_Preis_15min <- transform_loadprofile_mean(DA_Preis_60min,60)
#
#----Profile initialisieren----
load_15min_th(Waermebedarf) <- Waerme_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(Strombedarf) <- Strom_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(PV) <- PV_Erz_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
#
#------Simulationsumgebung zusammenfuehren----
components(Umgebung) <- list(Gas_Netz, Strom_Netz, 
                             Gasboiler, BHKW, 
                             Puffer, #Batterie,
                             Waermebedarf, Strombedarf, 
                             PV)
# Gas_Netz = PubGfuel1
# Strom_Netz = PubGel1
# Gasboiler = GaBo1
# BHKW = CHP1
# Puffer = TS1
# Batterie = Bat1
# Waermebedarf = Demth1
# Strombedarf = Demel1
# PV = PV1


#----dummy matrix for topologies----
#
nam <- compoNames(Umgebung);
dum <- matrix(0,length(nam),length(nam));
colnames(dum) <- nam
row.names(dum) <- nam
#
#----fuel graph----
x_fuel <- dum;
x_fuel_eff <- dum;
x_fuel_price <- array(dum,dim=c(dim(dum)[1],dim(dum)[2],length(timegrid(Umgebung))));
colnames(x_fuel_price)<- nam
rownames(x_fuel_price)<- nam
#fuel Verbindungen Gas_Netz -> Gasboiler
x_fuel["Umgebung_PubGfuel1","Umgebung_GaBo1"] <- -1;
x_fuel_eff["Umgebung_PubGfuel1","Umgebung_GaBo1"] <- 1;
x_fuel_price["Umgebung_PubGfuel1","Umgebung_GaBo1",] <- 0.0422-0.0061; #################
#
#fuel Verbindungen Gas_Netz -> BHKW
x_fuel["Umgebung_PubGfuel1","Umgebung_CHP1"] <- -1;
x_fuel_eff["Umgebung_PubGfuel1","Umgebung_CHP1"] <- 1;
x_fuel_price["Umgebung_PubGfuel1","Umgebung_CHP1",] <- 0.0422-0.0061; ##################
#
#
adjacency_fuel(Umgebung)<- x_fuel;
adjacencyEff_fuel(Umgebung)<- x_fuel_eff;
adjacencyPrice_fuel(Umgebung)<- x_fuel_price;
#
#----el graph----
x_el <- dum;
x_el_eff <- dum;
x_el_price <- array(dum,dim=c(dim(dum)[1],dim(dum)[2],length(timegrid(Umgebung))));
colnames(x_el_price)<- nam
rownames(x_el_price)<- nam
#el Verbindungen BHKW -> Strom_Netz
x_el["Umgebung_CHP1","Umgebung_PubGel1"] <- -1;
x_el_eff["Umgebung_CHP1","Umgebung_PubGel1"] <- 1;
x_el_price["Umgebung_CHP1","Umgebung_PubGel1",] <- -DA_Preis_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)] #-0.03608; #Förderung und Base
#
#
# el Verbindung BHKW -> Batterie
# x_el["Umgebung_CHP1","Umgebung_Bat1"] <- -1;
# x_el_eff["Umgebung_CHP1","Umgebung_Bat1"] <- 1;
# x_el_price["Umgebung_CHP1","Umgebung_Bat1",] <- -0.04+0.06792; ######################
#
#el Verbindungen BHKW -> Strombedarf
x_el["Umgebung_CHP1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_CHP1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_CHP1","Umgebung_Demel1",] <- 0.06792; #############################
#
#el Verbindungen PV -> Strombedarf
x_el["Umgebung_PV1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_Demel1",] <- 0.06792; #############################
#
#el Verbindungen PV -> Strom_Netz
x_el["Umgebung_PV1","Umgebung_PubGel1"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_PubGel1"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_PubGel1",] <- -0.1026; ############################
#
#el Verbindungen Strom_Netz -> Strombedarf
x_el["Umgebung_PubGel1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PubGel1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PubGel1","Umgebung_Demel1",] <- 0.2345; ##########################
#
#el Verbindungen PV -> Batterie
# x_el["Umgebung_PV1","Umgebung_Bat1"] <- -1;
# x_el_eff["Umgebung_PV1","Umgebung_Bat1"] <- 1;
# x_el_price["Umgebung_PV1","Umgebung_Bat1",] <- 0.06792; #############################
#
#el Verbindungen Batterie -> Strombedarf
# x_el["Umgebung_Bat1","Umgebung_Demel1"] <- -1;
# x_el_eff["Umgebung_Bat1","Umgebung_Demel1"] <- 1;
# x_el_price["Umgebung_Bat1","Umgebung_Demel1",] <- 0; #################################
#
adjacency_el(Umgebung)<- x_el;
adjacencyEff_el(Umgebung)<- x_el_eff;
adjacencyPrice_el(Umgebung)<- x_el_price;
#
#----thermal graph----
x_th <- dum;
x_th_eff <- dum;
x_th_price <- array(dum,dim=c(dim(dum)[1],dim(dum)[2],length(timegrid(Umgebung))));
colnames(x_th_price)<- nam
rownames(x_th_price)<- nam
x_th_WaDo <- dum;
x_th_WaDo_Var <- dum;
#th. Verbindung   Gasboiler -> Puffer
x_th["Umgebung_GaBo1","Umgebung_TS1"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_TS1"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_TS1",] <- 0;
#
#th. Verbindung   BHKW -> Puffer
x_th["Umgebung_CHP1","Umgebung_TS1"] <- -1;
x_th_eff["Umgebung_CHP1","Umgebung_TS1"] <- 1;
x_th_price["Umgebung_CHP1","Umgebung_TS1",] <- 0;

#th. Verbindung   Puffer -> Waermebedarf
x_th["Umgebung_TS1","Umgebung_Demth1"] <- -1;
x_th_eff["Umgebung_TS1","Umgebung_Demth1"] <- 1;
x_th_price["Umgebung_TS1","Umgebung_Demth1",] <- 0;
#
adjacency_th(Umgebung)<- x_th;
adjacencyEff_th(Umgebung)<- x_th_eff;
adjacencyPrice_th(Umgebung)<- x_th_price;
#adjacencyWatch_th(Umgebung)<- x_th_WaDo;
#adjacencyWatch_th_var(Umgebung)<- x_th_WaDo_Var 
#
#----Setzen der Koordinaten----
Umgebung <- finalcoordinates(Umgebung)
#
#----Erstellen des mathematischen Modells----
LM_Umgbung <- LM.Constraints(Umgebung)
#
#----Umwandeln des mathematischen Modells in einen Data-Frame----
a <- as.data.frame.LM(LM_Umgbung)
Zeilenbezeichnungen <- rownames(a)
Spaltenbezeichnungen <- colnames(a)
#
#----Zussatzkosten fuer Netznutzung hinzufuegen----
#Koordinaten <- coord(Umgebung)
#WW_Netz_Kosten <- 3*grepl("Op_Wado",Koordinaten)
#LM_Umgbung@cost <- LM_Umgbung@cost + WW_Netz_Kosten
#
#----Uebergabe des mathematischen Modells an den lp-Solver----
vec_types <- length(LM.binary(LM_Umgbung))
vec_types[LM.binary(LM_Umgbung)==TRUE] <- "B"
vec_types[LM.binary(LM_Umgbung)==FALSE] <- "C"
#
vec_dir <- length(LM.direction(LM_Umgbung))
vec_dir[LM.direction(LM_Umgbung)=="="] <- "=="
vec_dir[LM.direction(LM_Umgbung)=="<="] <- "<="

#
#----Simulationsparameter
Tag <- 141
Stunde <- 0

Stundenraster <- 24

Zeitschritt <- 1
Anzahl_Stunden <- 1




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

  
  if (Stunde==2) {
    Prognose <- read.csv2(paste0(directory,"/Prognose.csv"))    
  }
  
  if (Stunde==4) {
    Lauf_Start <- Sys.time()
    #
    Zeilen_Demth1 <- grep("_Demth1.Profile",Zeilenbezeichnungen) # Waermebedarf
    Zeilen_Demel1 <- grep("_Demel1.Profile",Zeilenbezeichnungen) # Strombedarf
    Zeilen_PV1 <- grep("_PV1.Profile",Zeilenbezeichnungen) # PV
    #
    #Zeilen_Eth1_Start <- grep("_TS1.E_th_time1",Zeilenbezeichnungen) # Pufferstart
    #Zeilen_Eth1_Ende <- grep("_TS1.E_th_end",Zeilenbezeichnungen) # Pufferende
    #
    #----Profile austauschen----
    LM_Umgbung@vector[Zeilen_Demth1] <- Waerme_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)]
    LM_Umgbung@vector[Zeilen_Demel1] <- Strom_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)]
    LM_Umgbung@vector[Zeilen_PV1] <- PV_Erz_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)]
    #
    # Bei der Wämrepumpe wird anders als bei den Profilen nicht der rhv geändert,
    # sondern die Modellmatrix. Hierzu wird zunächst das Objekt "HePu1" = WP in der
    # Komponentenliste bestimmt und dort die SourceTemp im 15 Minuten-Gitter
    # geändert.
    #SourceTemp_15min(Umgebung@components[[which(compoNames(Umgebung)=="Umgebung_HePu1")]]) <- Lufttemp_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
    # Anschließend wird die LM-Funktion aufgerufen, um den COP-Verlauf zu erstellen
    #Profil_WP_Tausch <- LM.Transform_elth_th(Umgebung@components[[which(compoNames(Umgebung)=="Umgebung_HePu1")]])@matrix
    # Im nächsten Schritt werden die Stellen in der Modellmatrix ermittelt, wo die
    # neue Matrix eingefügt werden soll
    #LM_Umgbung@matrix[grepl("Umgebung_HePu1.Transform_elth_th_time",rownames(LM_Umgbung@matrix)),grepl("HePu1",colnames(LM_Umgbung@matrix))] <- Profil_WP_Tausch
    #
    #test <- LM_Umgbung@matrix[grepl("Umgebung_HePu1.Transform_elth_th_time",rownames(LM_Umgbung@matrix)),grepl("HePu1",colnames(LM_Umgbung@matrix))]
    #View(test)
    #object <- Umgebung@components[[which(compoNames(Umgebung)=="Umgebung_HePu1")]]
    #LM_Umgbung@vector[Zeilen_Eth1_Start] <- 19.8
    #LM_Umgbung@vector[Zeilen_Eth1_Ende] <- 19.8
    #----Profil fuer Strommarkt austauschen----
    Spaltenbezeichnungen_price <- names(LM.cost(LM_Umgbung))
    Spalten_Strommarkt <- grep("Pel_CHP1_PubGel1_",Spaltenbezeichnungen_price) # Strommarkt
    #
    LM_Umgbung@cost[Spalten_Strommarkt] <- -DA_Preis_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)] #Förderung und Spot
    #
    #
    #
    #
    lptest <-
      Rglpk_solve_LP(
        LM.cost(LM_Umgbung),
        LM.matrix(LM_Umgbung),
        vec_dir,
        LM.vector(LM_Umgbung),
        bounds = NULL,
        types = vec_types,
        max = FALSE,
        control = list("verbose" = TRUE,
                       "tm_limit" = 5000)
      )
    print("Simulation abgeschlossen")
  #   Lauf_Ende <- Sys.time()
  #   Stunde <- Stunde + floor(Lauf_Ende - Lauf_Start)
  # }
  # 
  # if (Stunde==8) {
  #   Lauf_Start <- Sys.time()
    solution <- data.frame(name= coord(Umgebung),value=lptest$solution)
    var_nr <- dim(solution)[1]/24
    sol_mat <- lptest$solution
    sol_mat <- matrix(sol_mat,nrow=var_nr,byrow=TRUE)
    sol_mat <- t(sol_mat)
    sol_var <- sol_mat
    status <- lptest$status
    Auswertung <- as.data.frame(sol_mat)
    var_name <- unique(str_split_fixed(coord(Umgebung),'_time',2)[,1])
    colnames(Auswertung) <- var_name
    print("Fahrplan auswerten")
    #Sys.sleep(0.1)
    Lauf_Ende <- Sys.time()
    zw1 <- Lauf_Ende - time
    zw2 <- Lauf_Ende - Lauf_Start
    Stunde <- Stunde + as.double(floor(Lauf_Ende - time))
    Anzahl_Stunden <- Anzahl_Stunden + as.double(floor(Lauf_Ende - time))
  }
  
  if (Stunde==10) {
    write.csv2(Auswertung, file = "Fahrplan.csv")
    print("Fahrplan übergeben")
  }
  
  if (Stunde==24) {
    Stunde <- 0
    Tag <- Tag + 1
    if (Tag==366) {
      Tag <- 1
    }
  }
}


