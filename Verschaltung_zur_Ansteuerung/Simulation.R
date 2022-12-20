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
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(kableExtra)
library(readxl)
#---Theme fuer plots----
consolinnotheme<-function(){
  FontSize = 10
  theme(panel.grid.major.y = element_line(colour = "grey", linetype ="dashed" ),
        panel.background = element_rect(fill="white"),
        axis.ticks.length=unit(-0.25, "cm"),
        axis.text.x = element_text(color = "black", margin=unit(c(0.5,0.5,0.5,0.5), "cm"), size=FontSize),
        #ohne x-Achsenbeschriftung
        #axis.title.x = element_blank(),
        axis.text.y = element_text(color = "black",  size=FontSize,margin=unit(c(0.2,0.3,0.3,0.3), "cm")) ,
        axis.title.y= element_text(color = "black", size=FontSize),
        #axis.ticks.x = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=0.7),
        text=element_text(color = "black",size=FontSize),
        legend.position="bottom",
        legend.box = "horizontal",
        plot.title = element_text(color = "black", size = FontSize, face = "bold"),
        legend.title=element_text(color = "black", size=FontSize),
        legend.text=element_text(color = "black", size=FontSize),
        legend.key = element_rect(fill = "white", color = NA)
  )
}
#-----Farbpalette----
Farbpalette <- read_excel(paste0(getwd(),"/Farbpalette.xlsx"),
                          sheet = "Standardfarben")
colnames(Farbpalette)[1]<-"Bezeichnung"
#Bezeichnung entfernen,da aktuell nicht vorhanden
Farbpalette<-Farbpalette[,-5]
#Leerzeile entfernen, aktuell ist keine Farbe fuer Consolinno hinterlegt
Farbpalette<-na.omit(Farbpalette)

hellgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="hellgruen",-1],maxColorValue = 255)

mittelgruen <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="mittelgruen",-1],maxColorValue = 255)

dunkelgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="dunkelgruen",-1],maxColorValue = 255)

rot1<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="rot1",-1],maxColorValue = 255)
grau1 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau1",-1],maxColorValue = 255)
grau3 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau3",-1],maxColorValue = 255)
grau4 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau4",-1],maxColorValue = 255)
#
#----Ggf R-Umgebung laden----
setwd("C:/.../Projekte/Demonstrator") # Pfad anpassen!
#
#----Lastprofile erzeugen----
directory <- getwd()
#last_slash <- gregexpr("/",directory)
#last_slash <- max(last_slash[[1]])
#directory <- strtrim(directory,last_slash)
Lastprofil <- read.csv2(paste0(directory,"/Lastprofile_1h.csv"))
Waerme_60min <- 30*5000*Lastprofil[,2]  
Strom_60min <- 30*3000*Lastprofil[,3]
DA_Preis_60min <- Lastprofil[,4]/1000
PV_Erz_60min <- 39*1013*Lastprofil[,5]
E_Mob_60min <- 11*Lastprofil[,6]

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
maxP_el_plus(BHKW) <- 20
minP_el_plus(BHKW) <- 20
effMaxP_el_plus(BHKW) <- 0.38095
effMinP_el_plus(BHKW) <- 0.38095
effMaxP_th_plus(BHKW) <- 0.55238
effMinP_th_plus(BHKW) <- 0.55238
price_maintenance(BHKW) <- 0#.03    #Wartungskosten
minDowntime(BHKW) <- 0
minRuntime(BHKW) <- 1
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
#----Waermebedarf erzeugen----
Waermebedarf <- new.Demth()
info(Waermebedarf) <- list(name='Waermebedarf')
#
#----Strombedarf erzeugen----
Strombedarf <- new.Demel()
info(Strombedarf) <- list(name='Strombedarf')
#
#----E_Mobilitaet erzeugen----
E_Mob <- new.Demel()
info(E_Mob) <- list(name='E_Mob')
#
#----Simulationsstart----
Start_time <- Sys.time()
#####
#####For-Schleife beginn###################################
#####Tagessimulationen
sim_day <- 1
sim_day_start <- sim_day
days_to_sim <- 365 
stepsize <- 60 # in Minuten
timestep <- 24*60/stepsize
timestep_15min <- 96
timestep_div <- 15*timestep_15min/timestep#15*timestep/timestep_15min
lptest2 <- list()
status <- list()
#
#----Simulationsumgebung----
Umgebung <- new.Sandbox()
name(Umgebung) <- "Umgebung"
info(Umgebung) <- list(name="Quarree100")
timegrid(Umgebung) <- rep(stepsize,timestep) 
#
#
#----Profil auf Zeitschritt anpassen----
Waerme_15min <- transform_loadprofile_sum(Waerme_60min, timestep_div)
Strom_15min <- transform_loadprofile_sum(Strom_60min, timestep_div)
PV_Erz_15min <- transform_loadprofile_sum(PV_Erz_60min, timestep_div)
DA_Preis_15min <- transform_loadprofile_mean(DA_Preis_60min,timestep_div)
E_Mob_15min <- transform_loadprofile_sum(E_Mob_60min, timestep_div)
#
#----Profile initialisieren----
load_15min_th(Waermebedarf) <- Waerme_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(Strombedarf) <- Strom_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(PV) <- PV_Erz_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(E_Mob) <- E_Mob_15min[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
#
#------Simulationsumgebung zusammenfuehren----
components(Umgebung) <- list(Gas_Netz, Strom_Netz, 
                             Gasboiler, BHKW, 
                             Puffer,
                             Waermebedarf, Strombedarf, 
                             E_Mob,
                             PV)
# Gas_Netz = PubGfuel1
# H2_Netz = PubGfuel2
# Solarthermie_Ueberschuss_Netz = PubGth1
# Strom_Netz = PubGel1
# Wind_Strom_Netz = PubGel2
# Gasboiler = GaBo1
# BHKW = CHP1
# Luft_WP = HePu1
# Elektrolyseur = Electrolyseur1
# Slarthermie = SoTh1
# Puffer = TS1
# Batterie = Bat1
# Waermebedarf = Demth1
# Strombedarf = Demel1
# E_Mob = Demel2
# H2_Mob = Demfuel1
# PV_lokal = PV1
# PV_zentral = PV2
# Wind_Strom = PV3


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
x_fuel_price["Umgebung_PubGfuel1","Umgebung_GaBo1",] <- 0.04+0.2*0.019+10; # Gaspreis + CO2-Emissionen * CO2-Preis  
#
#fuel Verbindungen Gas_Netz -> BHKW
x_fuel["Umgebung_PubGfuel1","Umgebung_CHP1"] <- -1;
x_fuel_eff["Umgebung_PubGfuel1","Umgebung_CHP1"] <- 1;
x_fuel_price["Umgebung_PubGfuel1","Umgebung_CHP1",] <- 0.04+0.2*0.019; # Gaspreis + CO2-Emissionen * CO2-Preis   
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
x_el_price["Umgebung_CHP1","Umgebung_PubGel1",] <- -100*DA_Preis_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)] #-0.0528 #-0.03608; #Förderung und Base
#
# el Verbindung BHKW -> Strombedarf
x_el["Umgebung_CHP1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_CHP1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_CHP1","Umgebung_Demel1",] <- 0.06792-200; #-0.0219+0.06792; 
#
# el Verbindung BHKW -> E_Mob
x_el["Umgebung_CHP1","Umgebung_Demel2"] <- -1;
x_el_eff["Umgebung_CHP1","Umgebung_Demel2"] <- 1;
x_el_price["Umgebung_CHP1","Umgebung_Demel2",] <- 0.06792-200; #-0.0219+0.06792;  
#
#el Verbindungen PV -> Strombedarf
x_el["Umgebung_PV1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_Demel1",] <- 0.06792; 
#
# el Verbindung PV -> E_Mob
x_el["Umgebung_PV1","Umgebung_Demel2"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_Demel2"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_Demel2",] <- 0.06792; 
#
#el Verbindungen PV -> Strom_Netz
x_el["Umgebung_PV1","Umgebung_PubGel1"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_PubGel1"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_PubGel1",] <- -0.0855; # Wenn alle 2000 kWp als eine Anlage zählen
#
#el Verbindungen Strom_Netz -> Strombedarf
x_el["Umgebung_PubGel1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PubGel1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PubGel1","Umgebung_Demel1",] <- 0.2452;
#
#el Verbindungen Strom_Netz -> E_Mob
x_el["Umgebung_PubGel1","Umgebung_Demel2"] <- -1;
x_el_eff["Umgebung_PubGel1","Umgebung_Demel2"] <- 1;
x_el_price["Umgebung_PubGel1","Umgebung_Demel2",] <- 0.2452;
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
x_th["Umgebung_GaBo1","Umgebung_Demth1"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth1"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth1",] <- 0;
#
#th. Verbindung   BHKW -> Puffer
x_th["Umgebung_CHP1","Umgebung_TS1"] <- -1;
x_th_eff["Umgebung_CHP1","Umgebung_TS1"] <- 1;
x_th_price["Umgebung_CHP1","Umgebung_TS1",] <- 0;
#
#th. Verbindung   Puffer -> Waermebedarf
x_th["Umgebung_TS1","Umgebung_Demth1"] <- -1;
x_th_eff["Umgebung_TS1","Umgebung_Demth1"] <- 1;
x_th_price["Umgebung_TS1","Umgebung_Demth1",] <- 0;
#
adjacency_th(Umgebung)<- x_th;
adjacencyEff_th(Umgebung)<- x_th_eff;
adjacencyPrice_th(Umgebung)<- x_th_price;
adjacencyWatch_th(Umgebung)<- x_th_WaDo;
adjacencyWatch_th_var(Umgebung)<- x_th_WaDo_Var 
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
while (sim_day < days_to_sim+1) {
  sim_day
  #----Positionen der Zeilen fÃ¼r die Bedarfsuebergabe an die Demths suchen----
  Zeilen_Demth1 <- grep("_Demth1.Profile",Zeilenbezeichnungen) # Waermebedarf
  Zeilen_Demel1 <- grep("_Demel1.Profile",Zeilenbezeichnungen) # Strombedarf
  Zeilen_PV1 <- grep("_PV1.Profile",Zeilenbezeichnungen) # PV
  Zeilen_Demel2 <- grep("_Demel2.Profile",Zeilenbezeichnungen) # E_Mob
  #
  #Zeilen_Eth1_Start <- grep("_TS1.E_th_time1",Zeilenbezeichnungen) # Pufferstart
  #Zeilen_Eth1_Ende <- grep("_TS1.E_th_end",Zeilenbezeichnungen) # Pufferende
  #
  #----Profile austauschen----
  LM_Umgbung@vector[Zeilen_Demth1] <- Waerme_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)]
  LM_Umgbung@vector[Zeilen_Demel1] <- Strom_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)]
  LM_Umgbung@vector[Zeilen_PV1] <- PV_Erz_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)]
  LM_Umgbung@vector[Zeilen_Demel2] <- E_Mob_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)]
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
  LM_Umgbung@cost[Spalten_Strommarkt] <- -100*DA_Preis_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)] #Förderung und Spot
  #
  #
  #Spalten_H2Markt <- grep("Pfuel_Electrolyseur1_PubGfuel2_",Spaltenbezeichnungen_price) # Strommarkt
  #
  #LM_Umgbung@cost[Spalten_H2Markt] <- -1-DA_Preis_60min[(sim_day*timestep-timestep+1):(sim_day*timestep)] #Förderung und Spot
  
  
  
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
                     "tm_limit" = 10000)
    )
  
  
  lptest2$optimum <- c(lptest2$optimum,lptest$optimum)
  lptest2$solution <- c(lptest2$solution,lptest$solution)
  lptest2$status <- c(lptest2$status,lptest$status)
  lptest2$solution_dual <- c(lptest2$solution_dual,lptest$solution_dual)
  lptest2$auxiliary <- c(lptest2$auxiliary,lptest$auxiliary)
  
  solution <- data.frame(name= coord(Umgebung),value=lptest$solution)
  var_nr <- dim(solution)[1]/timestep
  sol_mat <- lptest$solution
  sol_mat <- matrix(sol_mat,nrow=var_nr,byrow=TRUE)
  sol_mat <- t(sol_mat)
  if (sim_day==sim_day_start) {
    sol_var <- sol_mat
    status <- lptest$status
  } else {
    sol_var <- rbind(sol_var, sol_mat)
    status <- rbind(status, lptest$status)
  }
  sim_day <- sim_day+1
}
solution <- data.frame(name= coord(Umgebung),value=lptest2$solution)
#
solution2 <- solution
#
End_time <- Sys.time()




#----Auswertung der Forschleife mit Variablenbezeichnung versehen----
tmp<-unique(str_split_fixed(coord(Umgebung),'_time',2)[,1])

Auswertung <- as.data.frame(sol_var)
colnames(Auswertung) <- tmp




# ggplot(Auswertung, aes(x=1:dim(Auswertung)[1])) + consolinnotheme() +
#   geom_step(aes(y = Pth_GaBo1_TS1, col = "Gasboiler")) +
#   geom_step(aes(y = Pth_CHP1_TS1, col = "BHKW")) +
#   geom_step(aes(y = Pth_HePu1_TS1, col = "WP")) +
#   geom_step(aes(y = Pth_TS1_Demth1, col = "Raumwärme"), linetype="dashed") +
#   geom_step(aes(y = Pth_TS1_TS2+Pth_TS1_TS3+Pth_TS1_TS4+Pth_TS1_TS5+Pth_TS1_TS6+Pth_TS1_TS7, col = "Warmwasser"), linetype="dashed") +
#   scale_colour_manual(" ", limits=c("Gasboiler", "BHKW", "WP", "Raumwärme", "Warmwasser"), values=c("red","blue",dunkelgruen,"black","green")) +
#   theme(legend.position = "bottom") +
#   expand_limits(x = 0, y = 0) +
#   labs(title = "Pufferspeicher zentral - Be- und Entladung", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")





# Auswertung15min <- c()
# TimeGrid <- timegrid(Umgebung)
# 
# for (m in 1:days_to_sim) {
#   for (n in 1:length(TimeGrid)) {
#     Auswertung15 <- rep(sol_var[n+(m*8-8),],TimeGrid[n]/15)
#     Auswertung15 <- matrix(Auswertung15,nrow=var_nr,byrow=FALSE)
#     Auswertung15 <- t(Auswertung15)
#     Auswertung15min <- rbind(Auswertung15min, Auswertung15)
#   }
# }
# 
# Auswertung <- as.data.frame(Auswertung15min)
# colnames(Auswertung) <- tmp

Auswertung365 <- Auswertung
Auswertung365$epex <- DA_Preis_60min



write.csv2(Auswertung365, file = "Sim_BHKW_PV_EEX_365.csv")
#write.csv2(a, file = "Simulationsmodell.csv")

Std_Start <- 3001
Std_End <- 3048
# Pfad <- getwd()
# Pfad <- paste0(Pfad,"/Graphiken")
# 
#ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + consolinnotheme() +
  ggplot(Auswertung365[Std_Start:Std_End,], aes(x=1:(Std_End-Std_Start+1))) + consolinnotheme() +
  geom_step(aes(y = Pth_GaBo1_Demth1, col = "Gasboiler")) +
  geom_step(aes(y = Pth_CHP1_TS1, col = "BHKW")) +
  geom_step(aes(y = Pth_TS1_Demth1, col = "Wärmebedarf"), linetype="dashed") +
  scale_colour_manual(" ", limits=c("Gasboiler", "BHKW", "Wärmebedarf"), values=c("red",dunkelgruen,"blue")) +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  labs(title = "Pufferspeicher zentral - Be- und Entladung", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
# 
# ggsave("Sim_BHKW_PV_EEX_WP_neu_365_Pth_InOut_Puffer_zen.jpg", plot = last_plot(), device = "jpeg", path = Pfad,
#       scale = 1, width = 12, height = 11, units = "cm",
#       dpi = 300, limitsize = TRUE)
# 
#ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + consolinnotheme() +
  ggplot(Auswertung365[Std_Start:Std_End,], aes(x=1:(Std_End-Std_Start+1))) + consolinnotheme() +
  geom_step(aes(y = Eth_TS1, col = "Füllstand")) +
  geom_step(aes(y = maxE_th(Puffer), col = "Maximum")) +
  scale_colour_manual(" ", limits=c("Füllstand", "Maximum"), values=c("red","blue")) +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  labs(title = "Pufferspeicher zentral - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
# 
# ggsave("Sim_BHKW_PV_EEX_WP_neu_365_Eth_Puffer_zen.jpg", plot = last_plot(), device = "jpeg", path = Pfad,
#        scale = 1, width = 12, height = 11, units = "cm",
#        dpi = 300, limitsize = TRUE)
# 
# 


# 
#ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + consolinnotheme() +
  ggplot(Auswertung365[Std_Start:Std_End,], aes(x=1:(Std_End-Std_Start+1))) + consolinnotheme() +
  geom_step(aes(y = Pel_PV1_Demel1+Pel_CHP1_Demel1+Pel_PubGel1_Demel1, col = "Strombedarf"), linetype="solid") +
  geom_step(aes(y = Pel_PV1_Demel2+Pel_CHP1_Demel2+Pel_PubGel1_Demel2, col = "E-Mob"), linetype="solid") +    
  geom_step(aes(y = Pel_PV1_Demel1+Pel_PV1_Demel2+Pel_PV1_PubGel1, col = "PV-Erzeugung"), linetype="solid") +
  scale_colour_manual(" ", limits=c("Strombedarf", "E-Mob", "PV-Erzeugung"),
                      values=c("#E69F00", "#56B4E9", dunkelgruen)) +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  labs(title = "Strombedarf", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
# # 
# ggsave("Sim_BHKW_PV_EEX_WP_neu_365_Strombedarf.jpg", plot = last_plot(), device = "jpeg", path = Pfad,
#        scale = 1, width = 12, height = 11, units = "cm",
#        dpi = 300, limitsize = TRUE)
# 
#ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + consolinnotheme() +
  ggplot(Auswertung365[Std_Start:Std_End,], aes(x=1:(Std_End-Std_Start+1))) + consolinnotheme() +
  geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_Demel2+Pel_CHP1_Demel1+Pel_CHP1_Demel2+Pel_PubGel1_Demel1+Pel_PubGel1_Demel2, fill="Netzbezug", color = "Netzbezug"), stat="identity") +
  geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_Demel2+Pel_CHP1_Demel1+Pel_CHP1_Demel2, fill="BHKW", color = "BHKW"), stat="identity") +
  geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_Demel2, fill="PV", color = "PV"), stat="identity") +
  scale_fill_manual(" ", limits=c("Netzbezug", "BHKW", "PV"), values=c(grau1, dunkelgruen, hellgruen)) +
  scale_colour_manual(" ", limits=c("Netzbezug", "BHKW", "PV"), values=c(grau1, dunkelgruen, hellgruen)) +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  labs(title = "Stromerverbrauch", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")

# ggsave("Sim_BHKW_PV_EEX_WP_neu_365_Stromverbrauch_Bilanz.jpg", plot = last_plot(), device = "jpeg", path = Pfad,
#       scale = 1, width = 12, height = 11, units = "cm",
#       dpi = 300, limitsize = TRUE)
#  
#ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + consolinnotheme() +
  # ggplot(Auswertung365[Std_Start:Std_End,], aes(x=Std_Start:Std_End)) + consolinnotheme() +
  # geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_PubGel1+Pel_CHP1_Demel1+Pel_CHP1_PubGel1, fill="BHKW Einsp", color = "BHKW Einsp"), stat="identity") +
  # geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_PubGel1+Pel_CHP1_Demel1, fill="BHKW Dir", color = "BHKW Dir"), stat="identity") +
  # geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_PubGel1, fill="PV Einsp", color = "PV Einsp"), stat="identity") +
  # geom_bar(aes(y = Pel_PV1_Demel1, fill="PV Dir", color = "PV Dir"), stat="identity") +
  # scale_fill_manual(" ", limits=c("BHKW Einsp", "BHKW Dir", "PV Einsp", "PV Dir"), values=c(grau1, grau3, dunkelgruen, hellgruen)) +
  # scale_colour_manual(" ", limits=c("BHKW Einsp", "BHKW Dir", "PV Einsp", "PV Dir"), values=c(grau1, grau3, dunkelgruen, hellgruen)) +
  # theme(legend.position = "bottom") +
  # expand_limits(x = 0, y = 0) +
  # labs(title = "Stromerzeugung", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
# # 
#ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + consolinnotheme() +
  ggplot(Auswertung365[Std_Start:Std_End,], aes(x=1:(Std_End-Std_Start+1))) + consolinnotheme() +
  geom_bar(aes(y = -Pel_PV1_PubGel1-Pel_CHP1_PubGel1, fill="BHKW Einsp", color = "BHKW Einsp"), stat="identity") +
  geom_bar(aes(y = -Pel_PV1_PubGel1, fill="PV Einsp", color = "PV Einsp"), stat="identity") +
  geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_Demel2+Pel_CHP1_Demel1+Pel_CHP1_Demel2, fill="BHKW Dir", color = "BHKW Dir"), stat="identity") +
  geom_bar(aes(y = Pel_PV1_Demel1+Pel_PV1_Demel2, fill="PV Dir", color = "PV Dir"), stat="identity") +
  scale_fill_manual(" ", limits=c("BHKW Einsp", "BHKW Dir", "PV Einsp", "PV Dir"), values=c(grau1, grau3, dunkelgruen, hellgruen)) +
  scale_colour_manual(" ", limits=c("BHKW Einsp", "BHKW Dir", "PV Einsp", "PV Dir"), values=c(grau1, grau3, dunkelgruen, hellgruen)) +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  labs(title = "Stromerzeugung", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
# # 
# ggsave("Sim_BHKW_PV_EEX_WP_neu_365_Stromerzeugung_Bilanz.jpg", plot = last_plot(), device = "jpeg", path = Pfad,
#       scale = 1, width = 12, height = 11, units = "cm",
#       dpi = 300, limitsize = TRUE)
# 
#ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + consolinnotheme() +
  ggplot(Auswertung365[Std_Start:Std_End,], aes(x=1:(Std_End-Std_Start+1))) + consolinnotheme() +
  geom_step(aes(y = epex*1000, col = "Spot-Preis [€/MWh]"), linetype="solid") +
  geom_step(aes(y = Pel_CHP1_PubGel1, col = "BHKW [kW]"), linetype="solid") +
  scale_colour_manual(" ", limits=c("Spot-Preis [€/MWh]", "BHKW [kW]"),
                      values=c("#E69F00", "#56B4E9")) +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  labs(title = "Strommarkt", x = "Viertelstunde des Tages\n\n", y = "Spot-Preis bzw. Einspeiseleistung")
# # 
# ggsave("Sim_BHKW_PV_EEX_WP_neu_365_Strommarkt.jpg", plot = last_plot(), device = "jpeg", path = Pfad,
#        scale = 1, width = 12, height = 11, units = "cm",
#        dpi = 300, limitsize = TRUE)
# 



Zeit <- End_time - Start_time
