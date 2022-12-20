#----Lade alle noetigen Pakages----
library(Root)
library(HePu)
library(TS)
library(Demth)
library(Demel)
library(Demfuel)
library(PubGel)
library(PubGth)
library(PubGfuel)
library(GaBo)
library(PV)
library(CHP)
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
colortheme<-function(){
  FontSize = 16
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
#Leerzeile entfernen
Farbpalette<-na.omit(Farbpalette)

hellgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="hellgruen",-1],maxColorValue = 255)
mittelgruen <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="mittelgruen",-1],maxColorValue = 255)
dunkelgruen<- rgb(Farbpalette[Farbpalette$Bezeichnung=="dunkelgruen",-1],maxColorValue = 255)
braun<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="braun",-1],maxColorValue = 255)
rot1<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="rot1",-1],maxColorValue = 255)
rot2<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="rot2",-1],maxColorValue = 255)
orange<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="orange",-1],maxColorValue = 255)
blau1<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="blau1",-1],maxColorValue = 255)
lila1<-  rgb(Farbpalette[Farbpalette$Bezeichnung=="lila1",-1],maxColorValue = 255)
grau1 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau1",-1],maxColorValue = 255)
grau3 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau3",-1],maxColorValue = 255)
grau4 <-  rgb(Farbpalette[Farbpalette$Bezeichnung=="grau4",-1],maxColorValue = 255)
#
#----Ggf R-Umgebung laden----

#
#----Lastprofile erzeugen----
directory <- getwd()
Lastprofile <- read.csv2(paste0(directory,"/Lastprofile_MAGGIE_kW.csv"))
#
Profil_PV_West <- Lastprofile[,3]
Profil_PV_Sued <- Lastprofile[,4]
Profil_PV_Ost <- Lastprofile[,5]
#
Profil_Strombedarf <- Lastprofile[,6]
#
Profil_DA_Preis <- Lastprofile[,7]/1000
#
Profil_TWW1 <- Lastprofile[,8]
Profil_TWW2 <- Lastprofile[,9]
Profil_TWW3 <- Lastprofile[,10]
Profil_TWW4 <- Lastprofile[,11]
Profil_TWW5 <- Lastprofile[,12]
Profil_TWW6 <- Lastprofile[,13]
#
Profil_RW1 <- Lastprofile[,14]
Profil_RW2 <- Lastprofile[,15]
Profil_RW3 <- Lastprofile[,16]
Profil_RW4 <- Lastprofile[,17]
Profil_RW5 <- Lastprofile[,18]
Profil_RW6 <- Lastprofile[,19]
#
Profil_Temp <- Lastprofile[,20]
#
#----Temperaturniveaus festlegen
NT = 55 # Vorlauf Niedertemperatur
HT = 80 # Vorlauf Hochtemperatur
Base = 30 # Rücklauf
#
#----Erdgasnetz erzeugen----
Gas_Netz <- new.PubGfuel()
maxP_fuel_plus(Gas_Netz) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
minP_fuel_plus(Gas_Netz) <- 0
#
#----Künstliche thermische Quelle erzeugen----
kunstQuell_th <- new.PubGth()
maxP_th_plus(kunstQuell_th) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
minP_th_plus(kunstQuell_th) <- 0
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
PV_West <- new.PV()
info(PV_West) <- list(name='PV_West')
PV_Sued <- new.PV()
info(PV_Sued) <- list(name='PV_Sued')
PV_Ost <- new.PV()
info(PV_Ost) <- list(name='PV_Ost')
#
#----Gasboiler erzeugen----
Gasboiler <- new.GaBo()
info(Gasboiler) <- list(name='Gasboiler1')
maxP_th_plus(Gasboiler) <- 200
effMaxP_th_plus(Gasboiler) <- 0.9
#
#----BHKW erzeugen----
BHKW <- new.CHP()
info(BHKW) <- list(name='BHKW')
maxP_el_plus(BHKW) <- 22
minP_el_plus(BHKW) <- 11
effMaxP_el_plus(BHKW) <- 0.3
effMinP_el_plus(BHKW) <- 0.2
effMaxP_th_plus(BHKW) <- 0.5
effMinP_th_plus(BHKW) <- 0.4
price_maintenance(BHKW) <- 0#.03    #Wartungskosten
minDowntime(BHKW) <- 1
minRuntime(BHKW) <- 1
initial_state(BHKW) <- 0
initial_stateEndurance(BHKW) <- 0
#
#----Waermepumpe erzeugen----
WP <- new.HePu()
info(WP) <- list(name='Waermepumpe')
maxP_th_plus(WP) <- 40
COP1(WP) <- 1.78
COP2(WP) <- 3.32
SourceTemp1(WP) <- -15
SourceTemp2(WP) <- 7
#
#----Puffer erzeugen----
#----Puffer der Waermepumpe erzeugen----
PS_WP <- new.TS()
info(PS_WP) <- list(name="Puffer_WP")
volume(PS_WP) <- 500
maxTemp(PS_WP) <- NT
minTemp(PS_WP) <- Base
maxP_th_plus(PS_WP) <- 200    #derzeit ist eine Beschränkung noch nötig
maxP_th_minus(PS_WP) <- 200    #derzeit ist eine Beschränkung noch nötig
effMaxP_th_minus(PS_WP) <- 1
eff_losses(PS_WP) <- 0.9987
percentageStart(PS_WP) <- 1
percentageEnd(PS_WP) <- 1
#
#----Puffer zentral erzeugen----
PS_z <- new.TS()
info(PS_z) <- list(name="PS_z")
volume(PS_z) <- 2000
maxTemp(PS_z) <- HT
minTemp(PS_z) <- NT
maxP_th_plus(PS_z) <- 200    
maxP_th_minus(PS_z) <- 200    
effMaxP_th_minus(PS_z) <- 1
eff_losses(PS_z) <- 0.9987
percentageStart(PS_z) <- 0.5
percentageEnd(PS_z) <- 0.5
#
#----Puffer denzentral 1 erzeugen----
PS_dez1 <- new.TS()
info(PS_dez1) <- list(name="PS_dez1")
volume(PS_dez1) <- 560
maxTemp(PS_dez1) <- HT
minTemp(PS_dez1) <- NT
maxP_th_plus(PS_dez1) <- 100    
maxP_th_minus(PS_dez1) <- 100    
effMaxP_th_minus(PS_dez1) <- 1
eff_losses(PS_dez1) <- 0.9987
percentageStart(PS_dez1) <- 0.5
percentageEnd(PS_dez1) <- 0.5
#
#----Puffer denzentral 2 erzeugen----
PS_dez2 <- PS_dez1
info(PS_dez2) <- list(name="PS_dez2")
#
#----Puffer denzentral 3 erzeugen----
PS_dez3 <- PS_dez1
info(PS_dez3) <- list(name="PS_dez3")
#
#----Puffer denzentral 4 erzeugen----
PS_dez4 <- PS_dez1
info(PS_dez4) <- list(name="PS_dez4")
#
#----Puffer denzentral 5 erzeugen----
PS_dez5 <- PS_dez1
info(PS_dez5) <- list(name="PS_dez5")
#
#----Puffer denzentral 6 erzeugen----
PS_dez6 <- PS_dez1
info(PS_dez6) <- list(name="PS_dez6")
#
#----WW 1 erzeugen----
TWW1_NT <- new.Demth()
info(TWW1_NT) <- list(name='TWW1_NT')
#
TWW1_HT <- new.Demth()
info(TWW1_HT) <- list(name='TWW1_HT')
#
#----WW 2 erzeugen----
TWW2_NT <- new.Demth()
info(TWW2_NT) <- list(name='TWW2_NT')
#
TWW2_HT <- new.Demth()
info(TWW2_HT) <- list(name='TWW2_HT')
#
#----WW 3 erzeugen----
TWW3_NT <- new.Demth()
info(TWW3_NT) <- list(name='TWW3_NT')
#
TWW3_HT <- new.Demth()
info(TWW3_HT) <- list(name='TWW3_HT')
#
#----WW 4 erzeugen----
TWW4_NT <- new.Demth()
info(TWW4_NT) <- list(name='TWW4_NT')
#
TWW4_HT <- new.Demth()
info(TWW4_HT) <- list(name='TWW4_HT')
#
#----WW 5 erzeugen----
TWW5_NT <- new.Demth()
info(TWW5_NT) <- list(name='TWW5_NT')
#
TWW5_HT <- new.Demth()
info(TWW5_HT) <- list(name='TWW5_HT')
#
#----WW 6 erzeugen----
TWW6_NT <- new.Demth()
info(TWW6_NT) <- list(name='TWW6_NT')
#
TWW6_HT <- new.Demth()
info(TWW6_HT) <- list(name='TWW6_HT')
#
#----RW erzeugen----
RW1_NT <- new.Demth()
info(RW1_NT) <- list(name='RW1_NT')
#
RW1_HT <- new.Demth()
info(RW1_HT) <- list(name='RW1_HT')
#
RW2_NT <- new.Demth()
info(RW2_NT) <- list(name='RW2_NT')
#
RW2_HT <- new.Demth()
info(RW2_HT) <- list(name='RW2_HT')
#
RW3_NT <- new.Demth()
info(RW3_NT) <- list(name='RW3_NT')
#
RW3_HT <- new.Demth()
info(RW3_HT) <- list(name='RW3_HT')
#
RW4_NT <- new.Demth()
info(RW4_NT) <- list(name='RW4_NT')
#
RW4_HT <- new.Demth()
info(RW4_HT) <- list(name='RW4_HT')
#
RW5_NT <- new.Demth()
info(RW5_NT) <- list(name='RW5_NT')
#
RW5_HT <- new.Demth()
info(RW5_HT) <- list(name='RW5_HT')
#
RW6_NT <- new.Demth()
info(RW6_NT) <- list(name='RW6_NT')
#
RW6_HT <- new.Demth()
info(RW6_HT) <- list(name='RW6_HT')
#
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
days_to_sim <- 10
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
info(Umgebung) <- list(name="MAGGIE")
timegrid(Umgebung) <- rep(stepsize,timestep) 
#
#
#----Profile initialisieren----
load_15min_th(TWW1_NT) <- (NT-Base)/(HT-Base)*Profil_TWW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW2_NT) <- (NT-Base)/(HT-Base)*Profil_TWW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW3_NT) <- (NT-Base)/(HT-Base)*Profil_TWW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_th(TWW4_NT) <- (NT-Base)/(HT-Base)*Profil_TWW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW5_NT) <- (NT-Base)/(HT-Base)*Profil_TWW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW6_NT) <- (NT-Base)/(HT-Base)*Profil_TWW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW1_HT) <- (HT-NT)/(HT-Base)*Profil_TWW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW2_HT) <- (HT-NT)/(HT-Base)*Profil_TWW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW3_HT) <- (HT-NT)/(HT-Base)*Profil_TWW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_th(TWW4_HT) <- (HT-NT)/(HT-Base)*Profil_TWW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW5_HT) <- (HT-NT)/(HT-Base)*Profil_TWW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(TWW6_HT) <- (HT-NT)/(HT-Base)*Profil_TWW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW1_NT) <- (NT-Base)/(HT-Base)*Profil_RW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW2_NT) <- (NT-Base)/(HT-Base)*Profil_RW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW3_NT) <- (NT-Base)/(HT-Base)*Profil_RW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW4_NT) <- (NT-Base)/(HT-Base)*Profil_RW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_th(RW5_NT) <- (NT-Base)/(HT-Base)*Profil_RW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW6_NT) <- (NT-Base)/(HT-Base)*Profil_RW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW1_HT) <- (HT-NT)/(HT-Base)*Profil_RW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW2_HT) <- (HT-NT)/(HT-Base)*Profil_RW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW3_HT) <- (HT-NT)/(HT-Base)*Profil_RW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW4_HT) <- (HT-NT)/(HT-Base)*Profil_RW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_th(RW5_HT) <- (HT-NT)/(HT-Base)*Profil_RW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_th(RW6_HT) <- (HT-NT)/(HT-Base)*Profil_RW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
load_15min_el(Strombedarf) <- Profil_Strombedarf[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(PV_West) <- Profil_PV_West[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(PV_Sued) <- Profil_PV_Sued[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
load_15min_el(PV_Ost) <- Profil_PV_Ost[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
SourceTemp_15min(WP) <- Profil_Temp[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
#
#
#------Simulationsumgebung zusammenfuehren----
components(Umgebung) <- list(Gas_Netz, 
                             PS_dez1, TWW1_HT,
                             PS_dez2, TWW2_HT,
                             PS_dez3, TWW3_HT,
                             PS_dez4, TWW4_HT,
                             PS_dez5, TWW5_HT,
                             PS_dez6, TWW6_HT,
                             RW1_HT, RW2_HT, RW3_HT, RW4_HT, RW5_HT, RW6_HT,
                             TWW1_NT,
                             TWW2_NT,
                             TWW3_NT,
                             TWW4_NT,
                             TWW5_NT,
                             TWW6_NT,
                             RW1_NT, RW2_NT, RW3_NT, RW4_NT, RW5_NT, RW6_NT,
                             Gasboiler, 
                             BHKW,
                             PS_z,
                             PS_WP, WP,
                             Strombedarf,
                             PV_West, PV_Sued, PV_Ost,
                             Strom_Netz)
#
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
#
#fuel Verbindungen Gas_Netz -> Gasboiler
x_fuel["Umgebung_PubGfuel1","Umgebung_GaBo1"] <- -1;
x_fuel_eff["Umgebung_PubGfuel1","Umgebung_GaBo1"] <- 1;
x_fuel_price["Umgebung_PubGfuel1","Umgebung_GaBo1",] <- 0.04+0.2*0.019+10; # Gaspreis + CO2-Emissionen * CO2-Preis   
#
#fuel Verbindungen Gas_Netz -> BHWK
x_fuel["Umgebung_PubGfuel1","Umgebung_CHP1"] <- -1;
x_fuel_eff["Umgebung_PubGfuel1","Umgebung_CHP1"] <- 1;
x_fuel_price["Umgebung_PubGfuel1","Umgebung_CHP1",] <- 0.04+0.2*0.019; # Gaspreis + CO2-Emissionen * CO2-Preis   
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
x_el_price["Umgebung_CHP1","Umgebung_PubGel1",] <- -0.0528-Profil_DA_Preis[(sim_day*timestep-timestep+1):(sim_day*timestep)] #-0.03608; #Förderung und Base
#
#el Verbindungen BHKW -> WP
x_el["Umgebung_CHP1","Umgebung_HePu1"] <- -1;
x_el_eff["Umgebung_CHP1","Umgebung_HePu1"] <- 1;
x_el_price["Umgebung_CHP1","Umgebung_HePu1",] <- 0.06789;
#
#el Verbindungen BHKW -> Strombedarf
x_el["Umgebung_CHP1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_CHP1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_CHP1","Umgebung_Demel1",] <- 0.06789;
#
#el Verbindungen PV_West -> Strom_Netz
x_el["Umgebung_PV1","Umgebung_PubGel1"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_PubGel1"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_PubGel1",] <- -0.10;
#
#el Verbindungen PV_Sued -> Strom_Netz
x_el["Umgebung_PV2","Umgebung_PubGel1"] <- -1;
x_el_eff["Umgebung_PV2","Umgebung_PubGel1"] <- 1;
x_el_price["Umgebung_PV2","Umgebung_PubGel1",] <- -0.10;
#
#el Verbindungen PV_Ost -> Strom_Netz
x_el["Umgebung_PV3","Umgebung_PubGel1"] <- -1;
x_el_eff["Umgebung_PV3","Umgebung_PubGel1"] <- 1;
x_el_price["Umgebung_PV3","Umgebung_PubGel1",] <- -0.10;
#
#el Verbindungen PV_West -> WP
x_el["Umgebung_PV1","Umgebung_HePu1"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_HePu1"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_HePu1",] <- 0.06789;
#
#el Verbindungen PV_Sued -> WP
x_el["Umgebung_PV2","Umgebung_HePu1"] <- -1;
x_el_eff["Umgebung_PV2","Umgebung_HePu1"] <- 1;
x_el_price["Umgebung_PV2","Umgebung_HePu1",] <- 0.06789;
#
#el Verbindungen PV_Ost -> WP
x_el["Umgebung_PV3","Umgebung_HePu1"] <- -1;
x_el_eff["Umgebung_PV3","Umgebung_HePu1"] <- 1;
x_el_price["Umgebung_PV3","Umgebung_HePu1",] <- 0.06789;
#
#el Verbindungen PV_West -> Strombedarf
x_el["Umgebung_PV1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PV1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PV1","Umgebung_Demel1",] <- 0.06789;
#
#el Verbindungen PV_Sued -> Strombedarf
x_el["Umgebung_PV2","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PV2","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PV2","Umgebung_Demel1",] <- 0.06789;
#
#el Verbindungen PV_Ost -> Strombedarf
x_el["Umgebung_PV3","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PV3","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PV3","Umgebung_Demel1",] <- 0.06789;
#
#el Verbindungen Strom_Netz -> WP
x_el["Umgebung_PubGel1","Umgebung_HePu1"] <- -1;
x_el_eff["Umgebung_PubGel1","Umgebung_HePu1"] <- 1;
x_el_price["Umgebung_PubGel1","Umgebung_HePu1",] <- 0.3;
#
#el Verbindungen Strom_Netz -> Strombedarf
x_el["Umgebung_PubGel1","Umgebung_Demel1"] <- -1;
x_el_eff["Umgebung_PubGel1","Umgebung_Demel1"] <- 1;
x_el_price["Umgebung_PubGel1","Umgebung_Demel1",] <- 0.3;
#
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
# x_th_WaDo <- dum;
# x_th_WaDo_Var <- dum;
#
#th. Verbindung   WP -> PS_WP
x_th["Umgebung_HePu1","Umgebung_TS8"] <- -1;
x_th_eff["Umgebung_HePu1","Umgebung_TS8"] <- 1;
x_th_price["Umgebung_HePu1","Umgebung_TS8",] <- -1;
#
#th. Verbindung   PS_WP -> TWW1_NT
x_th["Umgebung_TS8","Umgebung_Demth13"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth13"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth13",] <- 0;
#
#th. Verbindung   PS_WP -> TWW2_NT
x_th["Umgebung_TS8","Umgebung_Demth14"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth14"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth14",] <- 0;
#
#th. Verbindung   PS_WP -> TWW3_NT
x_th["Umgebung_TS8","Umgebung_Demth15"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth15"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth15",] <- 0;
#
#th. Verbindung   PS_WP -> TWW4_NT
x_th["Umgebung_TS8","Umgebung_Demth16"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth16"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth16",] <- 0;
#
#th. Verbindung   PS_WP -> TWW5_NT
x_th["Umgebung_TS8","Umgebung_Demth17"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth17"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth17",] <- 0;
#
#th. Verbindung   PS_WP -> TWW6_NT
x_th["Umgebung_TS8","Umgebung_Demth18"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth18"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth18",] <- 0;
#
#th. Verbindung   PS_WP -> RW1_NT
x_th["Umgebung_TS8","Umgebung_Demth19"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth19"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth19",] <- 0;
#
#th. Verbindung   PS_WP -> RW2_NT
x_th["Umgebung_TS8","Umgebung_Demth20"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth20"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth20",] <- 0;
#
#th. Verbindung   PS_WP -> RW3_NT
x_th["Umgebung_TS8","Umgebung_Demth21"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth21"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth21",] <- 0;
#
#th. Verbindung   PS_WP -> RW4_NT
x_th["Umgebung_TS8","Umgebung_Demth22"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth22"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth22",] <- 0;
#
#th. Verbindung   PS_WP -> RW5_NT
x_th["Umgebung_TS8","Umgebung_Demth23"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth23"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth23",] <- 0;
#
#th. Verbindung   PS_WP -> RW6_NT
x_th["Umgebung_TS8","Umgebung_Demth24"] <- -1;
x_th_eff["Umgebung_TS8","Umgebung_Demth24"] <- 1;
x_th_price["Umgebung_TS8","Umgebung_Demth24",] <- 0;
#
#th. Verbindung   BHKW -> PS_z
x_th["Umgebung_CHP1","Umgebung_TS7"] <- -1;
x_th_eff["Umgebung_CHP1","Umgebung_TS7"] <- 1;
x_th_price["Umgebung_CHP1","Umgebung_TS7",] <- 0;
#
#th. Verbindung   PS_z -> PS_dez1
x_th["Umgebung_TS7","Umgebung_TS1"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_TS1"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_TS1",] <- 0;
#
#th. Verbindung   PS_z -> PS_dez2
x_th["Umgebung_TS7","Umgebung_TS2"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_TS2"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_TS2",] <- 0;
#
#th. Verbindung   PS_z -> PS_dez3
x_th["Umgebung_TS7","Umgebung_TS3"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_TS3"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_TS3",] <- 0;
#
#th. Verbindung   PS_z -> PS_dez4
x_th["Umgebung_TS7","Umgebung_TS4"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_TS4"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_TS4",] <- 0;
#
#th. Verbindung   PS_z -> PS_dez5
x_th["Umgebung_TS7","Umgebung_TS5"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_TS5"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_TS5",] <- 0;
#
#th. Verbindung   PS_z -> PS_dez6
x_th["Umgebung_TS7","Umgebung_TS6"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_TS6"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_TS6",] <- 0;
#
#th. Verbindung   PS_z -> RW1_HT
x_th["Umgebung_TS7","Umgebung_Demth7"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_Demth7"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_Demth7",] <- 0;
#
#th. Verbindung   PS_z -> RW2_HT
x_th["Umgebung_TS7","Umgebung_Demth8"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_Demth8"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_Demth8",] <- 0;
#
#th. Verbindung   PS_z -> RW3_HT
x_th["Umgebung_TS7","Umgebung_Demth9"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_Demth9"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_Demth9",] <- 0;
#
#th. Verbindung   PS_z -> RW4_HT
x_th["Umgebung_TS7","Umgebung_Demth10"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_Demth10"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_Demth10",] <- 0;
#
#th. Verbindung   PS_z -> RW5_HT
x_th["Umgebung_TS7","Umgebung_Demth11"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_Demth11"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_Demth11",] <- 0;
#
#th. Verbindung   PS_z -> RW6_HT
x_th["Umgebung_TS7","Umgebung_Demth12"] <- -1;
x_th_eff["Umgebung_TS7","Umgebung_Demth12"] <- 1;
x_th_price["Umgebung_TS7","Umgebung_Demth12",] <- 0;
#
#th. Verbindung   PS_dez1 -> TWW1_HT
x_th["Umgebung_TS1","Umgebung_Demth1"] <- -1;
x_th_eff["Umgebung_TS1","Umgebung_Demth1"] <- 1;
x_th_price["Umgebung_TS1","Umgebung_Demth1",] <- 0;
#
#th. Verbindung   PS_dez2 -> TWW2_HT
x_th["Umgebung_TS2","Umgebung_Demth2"] <- -1;
x_th_eff["Umgebung_TS2","Umgebung_Demth2"] <- 1;
x_th_price["Umgebung_TS2","Umgebung_Demth2",] <- 0;
#
#th. Verbindung   PS_dez3 -> TWW3_HT
x_th["Umgebung_TS3","Umgebung_Demth3"] <- -1;
x_th_eff["Umgebung_TS3","Umgebung_Demth3"] <- 1;
x_th_price["Umgebung_TS3","Umgebung_Demth3",] <- 0;
#
#th. Verbindung   PS_dez4 -> TWW4_HT
x_th["Umgebung_TS4","Umgebung_Demth4"] <- -1;
x_th_eff["Umgebung_TS4","Umgebung_Demth4"] <- 1;
x_th_price["Umgebung_TS4","Umgebung_Demth4",] <- 0;
#
#th. Verbindung   PS_dez5 -> TWW5_HT
x_th["Umgebung_TS5","Umgebung_Demth5"] <- -1;
x_th_eff["Umgebung_TS5","Umgebung_Demth5"] <- 1;
x_th_price["Umgebung_TS5","Umgebung_Demth5",] <- 0;
#
#th. Verbindung   PS_dez6 -> TWW6_HT
x_th["Umgebung_TS6","Umgebung_Demth6"] <- -1;
x_th_eff["Umgebung_TS6","Umgebung_Demth6"] <- 1;
x_th_price["Umgebung_TS6","Umgebung_Demth6",] <- 0;
#
#th. Verbindung   Gasboiler -> PS_dez1
x_th["Umgebung_GaBo1","Umgebung_TS1"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_TS1"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_TS1",] <- 0;
#
#th. Verbindung   Gasboiler -> PS_dez2
x_th["Umgebung_GaBo1","Umgebung_TS2"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_TS2"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_TS2",] <- 0;
#
#th. Verbindung   Gasboiler -> PS_dez3
x_th["Umgebung_GaBo1","Umgebung_TS3"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_TS3"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_TS3",] <- 0;
#
#th. Verbindung   Gasboiler -> PS_dez4
x_th["Umgebung_GaBo1","Umgebung_TS4"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_TS4"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_TS4",] <- 0;
#
#th. Verbindung   Gasboiler -> PS_dez5
x_th["Umgebung_GaBo1","Umgebung_TS5"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_TS5"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_TS5",] <- 0;
#
#th. Verbindung   Gasboiler -> PS_dez6
x_th["Umgebung_GaBo1","Umgebung_TS6"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_TS6"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_TS6",] <- 0;
#
#th. Verbindung   Gasboiler -> RW1_HT
x_th["Umgebung_GaBo1","Umgebung_Demth7"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth7"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth7",] <- 0;
#
#th. Verbindung   Gasboiler -> RW2_HT
x_th["Umgebung_GaBo1","Umgebung_Demth8"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth8"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth8",] <- 0;
#
#th. Verbindung   Gasboiler -> RW3_HT
x_th["Umgebung_GaBo1","Umgebung_Demth9"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth9"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth9",] <- 0;
#
#th. Verbindung   Gasboiler -> RW4_HT
x_th["Umgebung_GaBo1","Umgebung_Demth10"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth10"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth10",] <- 0;
#
#th. Verbindung   Gasboiler -> RW5_HT
x_th["Umgebung_GaBo1","Umgebung_Demth11"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth11"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth11",] <- 0;
#
#th. Verbindung   Gasboiler -> RW6_HT
x_th["Umgebung_GaBo1","Umgebung_Demth12"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth12"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth12",] <- 0;
#
#th. Verbindung   Gasboiler -> TWW1_NT
x_th["Umgebung_GaBo1","Umgebung_Demth13"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth13"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth13",] <- 0;
#
#th. Verbindung   Gasboiler -> TWW2_NT
x_th["Umgebung_GaBo1","Umgebung_Demth14"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth14"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth14",] <- 0;
#
#th. Verbindung   Gasboiler -> TWW3_NT
x_th["Umgebung_GaBo1","Umgebung_Demth15"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth15"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth15",] <- 0;
#
#th. Verbindung   Gasboiler -> TWW4_NT
x_th["Umgebung_GaBo1","Umgebung_Demth16"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth16"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth16",] <- 0;
#
#th. Verbindung   Gasboiler -> TWW5_NT
x_th["Umgebung_GaBo1","Umgebung_Demth17"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth17"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth17",] <- 0;
#
#th. Verbindung   Gasboiler -> TWW6_NT
x_th["Umgebung_GaBo1","Umgebung_Demth18"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth18"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth18",] <- 0;
#
#th. Verbindung   Gasboiler -> RW1_NT
x_th["Umgebung_GaBo1","Umgebung_Demth19"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth19"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth19",] <- 0;
#
#th. Verbindung   Gasboiler -> RW2_NT
x_th["Umgebung_GaBo1","Umgebung_Demth20"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth20"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth20",] <- 0;
#
#th. Verbindung   Gasboiler -> RW3_NT
x_th["Umgebung_GaBo1","Umgebung_Demth21"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth21"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth21",] <- 0;
#
#th. Verbindung   Gasboiler -> RW4_NT
x_th["Umgebung_GaBo1","Umgebung_Demth22"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth22"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth22",] <- 0;
#
#th. Verbindung   Gasboiler -> RW5_NT
x_th["Umgebung_GaBo1","Umgebung_Demth23"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth23"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth23",] <- 0;
#
#th. Verbindung   Gasboiler -> RW6_NT
x_th["Umgebung_GaBo1","Umgebung_Demth24"] <- -1;
x_th_eff["Umgebung_GaBo1","Umgebung_Demth24"] <- 1;
x_th_price["Umgebung_GaBo1","Umgebung_Demth24",] <- 0;
#
adjacency_th(Umgebung)<- x_th;
adjacencyEff_th(Umgebung)<- x_th_eff;
adjacencyPrice_th(Umgebung)<- x_th_price;
# adjacencyWatch_th(Umgebung)<- x_th_WaDo;
# adjacencyWatch_th_var(Umgebung)<- x_th_WaDo_Var 
#
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
  Zeilen_Demth1 <- grep("_Demth1.Profile",Zeilenbezeichnungen) # TWW1_HT
  Zeilen_Demth2 <- grep("_Demth2.Profile",Zeilenbezeichnungen) # TWW2_HT
  Zeilen_Demth3 <- grep("_Demth3.Profile",Zeilenbezeichnungen) # TWW3_HT
  Zeilen_Demth4 <- grep("_Demth4.Profile",Zeilenbezeichnungen) # TWW4_HT
  Zeilen_Demth5 <- grep("_Demth5.Profile",Zeilenbezeichnungen) # TWW5_HT
  Zeilen_Demth6 <- grep("_Demth6.Profile",Zeilenbezeichnungen) # TWW6_HT
  Zeilen_Demth7 <- grep("_Demth7.Profile",Zeilenbezeichnungen) # RW1_HT
  Zeilen_Demth8 <- grep("_Demth8.Profile",Zeilenbezeichnungen) # RW2_HT
  Zeilen_Demth9 <- grep("_Demth9.Profile",Zeilenbezeichnungen) # RW3_HT
  Zeilen_Demth10 <- grep("_Demth10.Profile",Zeilenbezeichnungen) # RW4_HT
  Zeilen_Demth11 <- grep("_Demth11.Profile",Zeilenbezeichnungen) # RW5_HT
  Zeilen_Demth12 <- grep("_Demth12.Profile",Zeilenbezeichnungen) # RW6_HT
  Zeilen_Demth13 <- grep("_Demth13.Profile",Zeilenbezeichnungen) # TWW1_NT
  Zeilen_Demth14 <- grep("_Demth14.Profile",Zeilenbezeichnungen) # TWW2_NT
  Zeilen_Demth15 <- grep("_Demth15.Profile",Zeilenbezeichnungen) # TWW3_NT
  Zeilen_Demth16 <- grep("_Demth16.Profile",Zeilenbezeichnungen) # TWW4_NT
  Zeilen_Demth17 <- grep("_Demth17.Profile",Zeilenbezeichnungen) # TWW5_NT
  Zeilen_Demth18 <- grep("_Demth18.Profile",Zeilenbezeichnungen) # TWW6_NT
  Zeilen_Demth19 <- grep("_Demth19.Profile",Zeilenbezeichnungen) # RW1_NT
  Zeilen_Demth20 <- grep("_Demth20.Profile",Zeilenbezeichnungen) # RW2_NT
  Zeilen_Demth21 <- grep("_Demth21.Profile",Zeilenbezeichnungen) # RW3_NT
  Zeilen_Demth22 <- grep("_Demth22.Profile",Zeilenbezeichnungen) # RW4_NT
  Zeilen_Demth23 <- grep("_Demth23.Profile",Zeilenbezeichnungen) # RW5_NT
  Zeilen_Demth24 <- grep("_Demth24.Profile",Zeilenbezeichnungen) # RW6_NT
  Zeilen_Demel1 <- grep("_Demel1.Profile",Zeilenbezeichnungen) # Strombedarf
  Zeilen_PV1 <- grep("_PV1.Profile",Zeilenbezeichnungen) # PV_West
  Zeilen_PV2 <- grep("_PV2.Profile",Zeilenbezeichnungen) # PV_Sued
  Zeilen_PV3 <- grep("_PV3.Profile",Zeilenbezeichnungen) # PV_Ost
  Zeilen_HePu1 <- grep("_HePu1.Profile",Zeilenbezeichnungen) # WP
  
  #Zeilen_Eth1_Start <- grep("_TS1.E_th_time1",Zeilenbezeichnungen) # Pufferstart
  #Zeilen_Eth1_Ende <- grep("_TS1.E_th_end",Zeilenbezeichnungen) # Pufferende
  #
  #----Profile austauschen----
  LM_Umgbung@vector[Zeilen_Demth1] <- (NT-Base)/(HT-Base)*Profil_TWW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth2] <- (NT-Base)/(HT-Base)*Profil_TWW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth3] <- (NT-Base)/(HT-Base)*Profil_TWW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth4] <- (NT-Base)/(HT-Base)*Profil_TWW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth5] <- (NT-Base)/(HT-Base)*Profil_TWW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth6] <- (NT-Base)/(HT-Base)*Profil_TWW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth7] <- (NT-Base)/(HT-Base)*Profil_RW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth8] <- (NT-Base)/(HT-Base)*Profil_RW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  LM_Umgbung@vector[Zeilen_Demth9] <- (NT-Base)/(HT-Base)*Profil_RW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth10] <- (NT-Base)/(HT-Base)*Profil_RW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth11] <- (NT-Base)/(HT-Base)*Profil_RW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth12] <- (NT-Base)/(HT-Base)*Profil_RW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth13] <- (HT-NT)/(HT-Base)*Profil_TWW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth14] <- (HT-NT)/(HT-Base)*Profil_TWW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth15] <- (HT-NT)/(HT-Base)*Profil_TWW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth16] <- (HT-NT)/(HT-Base)*Profil_TWW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth17] <- (HT-NT)/(HT-Base)*Profil_TWW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth18] <- (HT-NT)/(HT-Base)*Profil_TWW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth19] <- (HT-NT)/(HT-Base)*Profil_RW1[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth20] <- (HT-NT)/(HT-Base)*Profil_RW2[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  LM_Umgbung@vector[Zeilen_Demth21] <- (HT-NT)/(HT-Base)*Profil_RW3[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth22] <- (HT-NT)/(HT-Base)*Profil_RW4[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth23] <- (HT-NT)/(HT-Base)*Profil_RW5[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)] 
  LM_Umgbung@vector[Zeilen_Demth24] <- (HT-NT)/(HT-Base)*Profil_RW6[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  LM_Umgbung@vector[Zeilen_Demel1] <- Profil_Strombedarf[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  LM_Umgbung@vector[Zeilen_PV1] <- Profil_PV_West[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  LM_Umgbung@vector[Zeilen_PV2] <- Profil_PV_Sued[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  LM_Umgbung@vector[Zeilen_PV3] <- Profil_PV_Ost[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  LM_Umgbung@vector[Zeilen_HePu1] <- Profil_Temp[(sim_day*timestep_15min-timestep_15min+1):(sim_day*timestep_15min)]
  
  # 
  #----Profil fuer Strommarkt austauschen----
  Spaltenbezeichnungen_price <- names(LM.cost(LM_Umgbung))
  Spalten_Strommarkt <- grep("Pel_CHP1_PubGel1_",Spaltenbezeichnungen_price) # Strommarkt
  LM_Umgbung@cost[Spalten_Strommarkt] <- -0.0528-Profil_DA_Preis[(sim_day*timestep-timestep+1):(sim_day*timestep)] #Förderung und Spot
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
                     "tm_limit" = 0.2*60*1000)
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
  sim_day
  
  write.table(sol_var, file = "results/sol_var_Szeanario1.txt", dec = ".", sep = "\t")
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


Auswertung365 <- Auswertung
Auswertung365$Spot <- Profil_DA_Preis[1:24]

Zeit <- End_time - Start_time


png("results/Wärmeerzeugung.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
    geom_step(size=1.1,aes(y = Pth_GaBo1_TS7, col = "Gasboiler")) +  
    geom_step(size=1.1,aes(y = Pth_CHP1_TS7, col = "BHKW")) +  
    geom_step(size=1.1,aes(y = Pth_HePu1_TS8, col = "WP")) +
    theme(legend.position = "bottom") +
    expand_limits(x = 0, y = 0) +
    scale_color_manual("Kompoente", values=c("WP" = dunkelgruen, "Gasboiler" = rot2, "BHKW"=orange ))+
    labs(title = "Wärmeerzeugung", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
dev.off()


png("results/Wärmebedarf.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_step(size=1.1,aes(y = Pth_TS7_Demth7+Pth_TS7_Demth8+Pth_TS7_Demth9+Pth_TS7_Demth10+Pth_TS7_Demth11+Pth_TS7_Demth12+
                           Pth_GaBo1_Demth7+Pth_GaBo1_Demth8+Pth_GaBo1_Demth9+Pth_GaBo1_Demth10+Pth_GaBo1_Demth11+Pth_GaBo1_Demth12+
                           Pth_TS8_Demth19+Pth_TS8_Demth20+Pth_TS8_Demth21+Pth_TS8_Demth22+Pth_TS8_Demth23+Pth_TS8_Demth24, col = "RW")) +
  geom_step(size=1.1,aes(y = Pth_TS1_Demth1+Pth_TS2_Demth2+Pth_TS3_Demth3+Pth_TS4_Demth4+Pth_TS5_Demth5+Pth_TS6_Demth6+
                           Pth_TS8_Demth13+Pth_TS8_Demth14+Pth_TS8_Demth15+Pth_TS8_Demth16+Pth_TS8_Demth17+Pth_TS8_Demth18, col = "TWW")) +
  theme(legend.position = "bottom") +
  scale_color_manual("Verbraucher", values=c("TWW" = blau1, "RW" = rot2))+
  expand_limits(x = 0, y = 0) +
  labs(title = "Wärmebedarf", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
dev.off()


png("results/BHKW_Wärmeerzeugung.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_step(size=1.1,aes(y = Pth_CHP1_TS7, col = "BHKW"), stat="identity") +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  scale_color_manual("Puffer", values=c("BHKW" = mittelgruen ))+
  labs(title = "BHKW", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
dev.off()


png("results/WP_Wärmeerzeugung.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_step(size=1.1,aes(y = Pth_HePu1_TS8, col = "WP"), stat="identity") +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  scale_color_manual("Puffer", values=c("WP" = mittelgruen ))+
  labs(title = "WP", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
dev.off()


png("results/Gasboiler_Wärmeerzeugung.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_step(size=1.1,aes(y = Pth_GaBo1_TS1+Pth_GaBo1_TS2+Pth_GaBo1_TS3+Pth_GaBo1_TS4+Pth_GaBo1_TS5+Pth_GaBo1_TS6+
                           Pth_GaBo1_Demth7+Pth_GaBo1_Demth8+Pth_GaBo1_Demth9+Pth_GaBo1_Demth10+Pth_GaBo1_Demth11+Pth_GaBo1_Demth12+
                           Pth_GaBo1_Demth13+Pth_GaBo1_Demth14+Pth_GaBo1_Demth15+Pth_GaBo1_Demth16+Pth_GaBo1_Demth17+Pth_GaBo1_Demth18+
                           Pth_GaBo1_Demth19+Pth_GaBo1_Demth20+Pth_GaBo1_Demth21+Pth_GaBo1_Demth22+Pth_GaBo1_Demth23+Pth_GaBo1_Demth24, col = "Gasboiler"), stat="identity") +
  theme(legend.position = "bottom") +
  expand_limits(x = 0, y = 0) +
  scale_color_manual("Puffer", values=c("Gasboiler" = mittelgruen ))+
  labs(title = "Gasboiler", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
dev.off()


png("results/Puffer1.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS1, fill = "dez Puffer 1"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_dez1), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("dez Puffer 1" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "dezentraler Pufferspeicher 1 - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()


png("results/Puffer2.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS2, fill = "dez Puffer 2"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_dez2), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("dez Puffer 2" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "dezentraler Pufferspeicher 2 - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()


png("results/Puffer3.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS3, fill = "dez Puffer 3"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_dez3), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("dez Puffer 3" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "dezentraler Pufferspeicher 3 - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()


png("results/Puffer4.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS4, fill = "dez Puffer 4"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_dez4), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("dez Puffer 4" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "dezentraler Pufferspeicher 4 - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()


png("results/Puffer5.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS5, fill = "dez Puffer 5"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_dez5), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("dez Puffer 5" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "dezentraler Pufferspeicher 5 - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()


png("results/Puffer6.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS6, fill = "dez Puffer 6"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_dez6), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("dez Puffer 6" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "dezentraler Pufferspeicher 6 - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()


png("results/Puffer_zentral.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS7, fill = "zentraler Puffer"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_z), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("zentraler Puffer" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "zentraler Pufferspeicher - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()


png("results/Puffer_WP.png", width = 900, height = 400)
ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
  geom_bar(aes(y = Eth_TS8, fill = "Puffer WP"), stat="identity") +
  geom_step(size=1.1,aes(y = maxE_th(PS_WP), col = "Maximum")) +
  theme(legend.position = "bottom") +
  scale_colour_manual("", values = c("Maximum" = blau1))+
  scale_fill_manual("Puffer", values=c("Puffer WP" = dunkelgruen ))+
  expand_limits(x = 0, y = 0) +
  labs(col = "",title = "Wärmepumpen-Pufferspeicher - Füllstand", x = "Viertelstunde des Tages\n\n", y = "Energie [kWh]")
dev.off()




# png("results/künstiliche_Quelle_thermisch.png", width = 900, height = 400)
# ggplot(Auswertung365, aes(x=1:dim(Auswertung365)[1])) + colortheme() +
#   geom_step(size=1.1,aes(y = Pth_PubGth1_TS1, col = "PSP1_30_90")) +  
#   geom_step(size=1.1,aes(y = Pth_PubGth1_TS2, col = "PSP2_30_90")) +
#   geom_step(size=1.1,aes(y = Pth_PubGth1_TS3, col = "PSP3_30_65")) +
#   geom_step(size=1.1,aes(y = Pth_PubGth1_TS4, col = "PSP4_30_65")) +  
#   geom_step(size=1.1,aes(y = Pth_PubGth1_TS5, col = "PSP5_30_65")) +  
#   geom_step(size=1.1,aes(y = Pth_PubGth1_TS6, col = "PSP6_30_65")) +  
#   theme(legend.position = "bottom") +
#   scale_color_manual("Puffer", values=c("PSP1_30_90" = rot1, "PSP2_30_90"=orange,"PSP3_30_65" =blau1, "PSP4_30_65"= hellgruen,"PSP5_30_65"=mittelgruen,"PSP6_30_65"=dunkelgruen ))+
#   expand_limits(x = 0, y = 0) +
#   labs(col="Puffer",title = "Einsatz künstliche Quelle", x = "Viertelstunde des Tages\n\n", y = "Leistung [kW]")
# dev.off()
# 
# 
# 
# 
# 
# 
# 



#----Ausgabe der BHKW-Fahrpläne----
Pfad <- getwd()
tab1 <- (1:(dim(Auswertung365)[1]*2))
# tab1 <- c(0,900, 901, 1800, 1801, 2700, 2701, 3600,3601,
#           7200,7201,10800,10801,14400,14401,18000,
#           18001,21600,21601,25200,25201,28800,28801,32400,32401,
#           36000,36001,39600,39601,43200,43201,46800,46801,50400,
#           50401,54000,54001,57600,57601,61200,61201,64800,64801,
#           68400,68401,72000,72001,75600,75601,79200,79201,82800,
#           82801,86400)
tab1 <- as.data.frame(tab1)

Fahrplan_opt_CHP1 <- tab1
Fahrplan_opt_CHP1$P <- kronecker((Auswertung365$Pel_CHP1_Demel1+Auswertung365$Pel_CHP1_PubGel1+Auswertung365$Pel_CHP1_HePu1),c(1,1))
write.table(Fahrplan_opt_CHP1, file = "results/R_Fahrplan_opt_CHP1.txt",dec = ".", sep = "\t")

Fahrplan_opt_WP1 <- tab1
Fahrplan_opt_WP1$P <- kronecker((Auswertung365$Pel_PubGel1_HePu1+Auswertung365$Pel_PV1_HePu1+Auswertung365$Pel_PV2_HePu1+Auswertung365$Pel_PV3_HePu1+Auswertung365$Pel_CHP1_HePu1),c(1,1))
write.table(Fahrplan_opt_WP1, file = "results/R_Fahrplan_opt_WP1.txt",dec = ".", sep = "\t")

write.table(Auswertung365, file = "results/Auswertung365.txt", dec = ".", sep = "\t")


costs <- data.frame( t(lptest2$optimum))
colnames(costs) <- c("day1","day2")

write.table(costs, file = "results/costs.txt", dec = ".", sep = "\t")
# 
# 
# 
