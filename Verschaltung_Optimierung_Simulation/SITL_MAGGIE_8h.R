

Optimierung <- function(Profil_PV_Ost,
                        Profil_PV_Sued,
                        Profil_PV_West,
                        Profil_Strombedarf,
                        Profil_DA_Preis,
                        Profil_TWW1_NT,
                        Profil_TWW2_NT,
                        Profil_TWW3_NT,
                        Profil_TWW4_NT,
                        Profil_TWW5_NT,
                        Profil_TWW6_NT,
                        Profil_TWW1_HT,
                        Profil_TWW2_HT,
                        Profil_TWW3_HT,
                        Profil_TWW4_HT,
                        Profil_TWW5_HT,
                        Profil_TWW6_HT,
                        Profil_RW1_NT,
                        Profil_RW2_NT,
                        Profil_RW3_NT,
                        Profil_RW4_NT,
                        Profil_RW5_NT,
                        Profil_RW6_NT,
                        Profil_RW1_HT,
                        Profil_RW2_HT,
                        Profil_RW3_HT,
                        Profil_RW4_HT,
                        Profil_RW5_HT,
                        Profil_RW6_HT,
                        Profil_Temp,
                        Anzahl_Zeitschritte, Zeitintervall, Solver,
                        TS1_Init,
                        TS2_Init,
                        TS3_Init,
                        TS4_Init,
                        TS5_Init,
                        TS6_Init,
                        TS7_Init,
                        TS8_Init,
                        BHKW_initialstate) {
  
      #----Lade alle noetigen Pakages----
      library(Root)
      library(HePu)
      library(TS)
      library(Demth)
      library(Demel)
      library(PubGel)
      library(PubGth)
      library(PubGfuel)
      library(GaBo)
      library(PV)
      library(CHP)
      library(Sandbox)
      if (Solver == 1) {
        library(Rglpk)
      } else if (Solver == 2) {
        library(lpSolve)
      }
      #
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
      minDowntime(BHKW) <- 0
      minRuntime(BHKW) <- 0.5
      initial_state(BHKW) <- BHKW_initialstate
      initial_stateEndurance(BHKW) <- 0
      #
      #----Waermepumpe erzeugen----
      WP <- new.HePu()
      info(WP) <- list(name='Waermepumpe')
      maxP_th_plus(WP) <- 40
      COP1(WP) <- 1.78
      COP2(WP) <- 3 #3.32
      SourceTemp1(WP) <- -15
      SourceTemp2(WP) <- 7
      #
      #----Puffer erzeugen----
      #----Puffer der Waermepumpe erzeugen----
      PS_WP <- new.TS()
      info(PS_WP) <- list(name="Puffer_WP")
      volume(PS_WP) <- 400 # 500
      maxTemp(PS_WP) <- NT
      minTemp(PS_WP) <- Base
      maxP_th_plus(PS_WP) <- 200    #derzeit ist eine Beschränkung noch nötig
      maxP_th_minus(PS_WP) <- 200    #derzeit ist eine Beschränkung noch nötig
      effMaxP_th_minus(PS_WP) <- 1
      eff_losses(PS_WP) <- 0.9987
      percentageStart(PS_WP) <- TS8_Init
      percentageEnd(PS_WP) <- -1
      #
      #----Puffer zentral erzeugen----
      PS_z <- new.TS()
      info(PS_z) <- list(name="PS_z")
      volume(PS_z) <- 800 # 1000
      maxTemp(PS_z) <- HT
      minTemp(PS_z) <- NT
      maxP_th_plus(PS_z) <- 200    
      maxP_th_minus(PS_z) <- 200    
      effMaxP_th_minus(PS_z) <- 1
      eff_losses(PS_z) <- 0.9987
      percentageStart(PS_z) <- TS7_Init
      percentageEnd(PS_z) <- -1
      #
      #----Puffer denzentral 1 erzeugen----
      PS_dez1 <- new.TS()
      info(PS_dez1) <- list(name="PS_dez1")
      volume(PS_dez1) <- 500 # 560
      maxTemp(PS_dez1) <- HT
      minTemp(PS_dez1) <- NT
      maxP_th_plus(PS_dez1) <- 100    
      maxP_th_minus(PS_dez1) <- 100    
      effMaxP_th_minus(PS_dez1) <- 1
      eff_losses(PS_dez1) <- 0.9987
      percentageStart(PS_dez1) <- TS1_Init
      percentageEnd(PS_dez1) <- -1
      #
      #----Puffer denzentral 2 erzeugen----
      PS_dez2 <- PS_dez1
      info(PS_dez2) <- list(name="PS_dez2")
      percentageStart(PS_dez2) <- TS2_Init
      #
      #----Puffer denzentral 3 erzeugen----
      PS_dez3 <- PS_dez1
      info(PS_dez3) <- list(name="PS_dez3")
      percentageStart(PS_dez3) <- TS3_Init
      #
      #----Puffer denzentral 4 erzeugen----
      PS_dez4 <- PS_dez1
      info(PS_dez4) <- list(name="PS_dez4")
      percentageStart(PS_dez4) <- TS4_Init
      #
      #----Puffer denzentral 5 erzeugen----
      PS_dez5 <- PS_dez1
      info(PS_dez5) <- list(name="PS_dez5")
      percentageStart(PS_dez5) <- TS5_Init
      #
      #----Puffer denzentral 6 erzeugen----
      PS_dez6 <- PS_dez1
      info(PS_dez6) <- list(name="PS_dez6")
      percentageStart(PS_dez6) <- TS6_Init
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
      #----Simulationsumgebung----
      Umgebung <- new.Sandbox()
      name(Umgebung) <- "Umgebung"
      info(Umgebung) <- list(name = "FMU_MAGGIE")
      timegrid(Umgebung) <- rep(Zeitintervall, Anzahl_Zeitschritte)
      timestep_15min <- 96
      timestep_div <- 15*timestep_15min/Anzahl_Zeitschritte
      #
      #----Profile initialisieren----
      load_15min_th(TWW1_NT) <- Profil_TWW1_NT
      load_15min_th(TWW2_NT) <- Profil_TWW2_NT
      load_15min_th(TWW3_NT) <- Profil_TWW3_NT
      load_15min_th(TWW4_NT) <- Profil_TWW4_NT
      load_15min_th(TWW5_NT) <- Profil_TWW5_NT
      load_15min_th(TWW6_NT) <- Profil_TWW6_NT
      load_15min_th(TWW1_HT) <- Profil_TWW1_HT
      load_15min_th(TWW2_HT) <- Profil_TWW2_HT
      load_15min_th(TWW3_HT) <- Profil_TWW3_HT
      load_15min_th(TWW4_HT) <- Profil_TWW4_HT
      load_15min_th(TWW5_HT) <- Profil_TWW5_HT
      load_15min_th(TWW6_HT) <- Profil_TWW6_HT
      load_15min_th(RW1_NT) <- Profil_RW1_NT
      load_15min_th(RW2_NT) <- Profil_RW2_NT
      load_15min_th(RW3_NT) <- Profil_RW3_NT
      load_15min_th(RW4_NT) <- Profil_RW4_NT
      load_15min_th(RW5_NT) <- Profil_RW5_NT
      load_15min_th(RW6_NT) <- Profil_RW6_NT
      load_15min_th(RW1_HT) <- Profil_RW1_HT
      load_15min_th(RW2_HT) <- Profil_RW2_HT
      load_15min_th(RW3_HT) <- Profil_RW3_HT
      load_15min_th(RW4_HT) <- Profil_RW4_HT
      load_15min_th(RW5_HT) <- Profil_RW5_HT
      load_15min_th(RW6_HT) <- Profil_RW6_HT
      load_15min_el(Strombedarf) <- Profil_Strombedarf
      load_15min_el(PV_West) <- Profil_PV_West
      load_15min_el(PV_Sued) <- Profil_PV_Sued
      load_15min_el(PV_Ost) <- Profil_PV_Ost
      SourceTemp_15min(WP) <- Profil_Temp
      #
      Profil_DA_Preis <- Profil_DA_Preis # transform_loadprofile_mean(Profil_DA_Preis,15/(timestep_15min/Anzahl_Zeitschritte))
      #
      #------Simulationsumgebung zusammenfuehren----
      components(Umgebung) <- list(Gas_Netz, # PubGfuel 1
                                   PS_dez1, TWW1_HT, # TS1, Demth1
                                   PS_dez2, TWW2_HT, # TS2, Demth2
                                   PS_dez3, TWW3_HT, # TS3, Demth3
                                   PS_dez4, TWW4_HT, # TS4, Demth4
                                   PS_dez5, TWW5_HT, # TS5, Demth5
                                   PS_dez6, TWW6_HT, # TS6, Demth6
                                   RW1_HT, RW2_HT, RW3_HT, RW4_HT, RW5_HT, RW6_HT, # Demth 7-12
                                   TWW1_NT, # Demth 13
                                   TWW2_NT, # Demth 14
                                   TWW3_NT, # Demth 15
                                   TWW4_NT, # Demth 16
                                   TWW5_NT, # Demth 17
                                   TWW6_NT, # Demth 18
                                   RW1_NT, RW2_NT, RW3_NT, RW4_NT, RW5_NT, RW6_NT, # Demth 19-24
                                   Gasboiler, # GaBo 1
                                   BHKW, # CHP 1
                                   PS_z, # TS7 
                                   PS_WP, WP, # TS8, HePu 1
                                   Strombedarf, # Demel 1
                                   PV_West, PV_Sued, PV_Ost, # PV 1-3
                                   Strom_Netz) # PubGel 1
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
      x_fuel_price["Umgebung_PubGfuel1","Umgebung_GaBo1",] <- 0.2*10000 #0.581+100000; # Gaspreis + CO2-Emissionen * CO2-Preis   0.2*10000 #
      #
      #fuel Verbindungen Gas_Netz -> BHWK
      x_fuel["Umgebung_PubGfuel1","Umgebung_CHP1"] <- -1;
      x_fuel_eff["Umgebung_PubGfuel1","Umgebung_CHP1"] <- 1;
      x_fuel_price["Umgebung_PubGfuel1","Umgebung_CHP1",] <- 0.2 #0.581; # Gaspreis + CO2-Emissionen * CO2-Preis   0.2 #
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
      x_el_price["Umgebung_CHP1","Umgebung_PubGel1",] <- -0.366 #-0.16-Profil_DA_Preis  #   
      #
      #el Verbindungen BHKW -> WP
      x_el["Umgebung_CHP1","Umgebung_HePu1"] <- -1;
      x_el_eff["Umgebung_CHP1","Umgebung_HePu1"] <- 1;
      x_el_price["Umgebung_CHP1","Umgebung_HePu1",] <- 0 # -0.08 + 0.065*0.4;
      #
      #el Verbindungen BHKW -> Strombedarf
      x_el["Umgebung_CHP1","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_CHP1","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_CHP1","Umgebung_Demel1",] <- 0 # -0.08 + 0.065*0.4 - 0.2607*0.9;
      #
      #el Verbindungen PV_West -> Strom_Netz
      x_el["Umgebung_PV1","Umgebung_PubGel1"] <- -1;
      x_el_eff["Umgebung_PV1","Umgebung_PubGel1"] <- 1;
      x_el_price["Umgebung_PV1","Umgebung_PubGel1",] <- -0.366 #-0.0855;  #   -0.4 #
      #
      #el Verbindungen PV_Sued -> Strom_Netz
      x_el["Umgebung_PV2","Umgebung_PubGel1"] <- -1;
      x_el_eff["Umgebung_PV2","Umgebung_PubGel1"] <- 1;
      x_el_price["Umgebung_PV2","Umgebung_PubGel1",] <- -0.366 #-0.0855;  #   -0.4 #
      #
      #el Verbindungen PV_Ost -> Strom_Netz
      x_el["Umgebung_PV3","Umgebung_PubGel1"] <- -1;
      x_el_eff["Umgebung_PV3","Umgebung_PubGel1"] <- 1;
      x_el_price["Umgebung_PV3","Umgebung_PubGel1",] <- -0.366 #-0.0855;  #   -0.4 #  
      #
      #el Verbindungen PV_West -> WP
      x_el["Umgebung_PV1","Umgebung_HePu1"] <- -1;
      x_el_eff["Umgebung_PV1","Umgebung_HePu1"] <- 1;
      x_el_price["Umgebung_PV1","Umgebung_HePu1",] <- 0 # 0.065*0.4;
      #
      #el Verbindungen PV_Sued -> WP
      x_el["Umgebung_PV2","Umgebung_HePu1"] <- -1;
      x_el_eff["Umgebung_PV2","Umgebung_HePu1"] <- 1;
      x_el_price["Umgebung_PV2","Umgebung_HePu1",] <- 0 # 0.065*0.4;
      #
      #el Verbindungen PV_Ost -> WP
      x_el["Umgebung_PV3","Umgebung_HePu1"] <- -1;
      x_el_eff["Umgebung_PV3","Umgebung_HePu1"] <- 1;
      x_el_price["Umgebung_PV3","Umgebung_HePu1",] <- 0 # 0.065*0.4;
      #
      #el Verbindungen PV_West -> Strombedarf
      x_el["Umgebung_PV1","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_PV1","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_PV1","Umgebung_Demel1",] <- 0 # 0.065*0.4 - (0.2607*0.9);
      #
      #el Verbindungen PV_Sued -> Strombedarf
      x_el["Umgebung_PV2","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_PV2","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_PV2","Umgebung_Demel1",] <- 0 # 0.065*0.4 - (0.2607*0.9);
      #
      #el Verbindungen PV_Ost -> Strombedarf
      x_el["Umgebung_PV3","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_PV3","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_PV3","Umgebung_Demel1",] <- 0 # 0.065*0.4 - (0.2607*0.9);
      #
      #el Verbindungen Strom_Netz -> WP
      x_el["Umgebung_PubGel1","Umgebung_HePu1"] <- -1;
      x_el_eff["Umgebung_PubGel1","Umgebung_HePu1"] <- 1;
      x_el_price["Umgebung_PubGel1","Umgebung_HePu1",] <- 0.366 # 0.2607;
      #
      #el Verbindungen Strom_Netz -> Strombedarf
      x_el["Umgebung_PubGel1","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_PubGel1","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_PubGel1","Umgebung_Demel1",] <- 0.366 # 0.2607 - (0.2607*0.9);
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
      x_th_price["Umgebung_HePu1","Umgebung_TS8",] <- 0; #-1;
      #
      #th. Verbindung   PS_WP -> TWW1_NT
      x_th["Umgebung_TS8","Umgebung_Demth13"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth13"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth13",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> TWW2_NT
      x_th["Umgebung_TS8","Umgebung_Demth14"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth14"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth14",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> TWW3_NT
      x_th["Umgebung_TS8","Umgebung_Demth15"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth15"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth15",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> TWW4_NT
      x_th["Umgebung_TS8","Umgebung_Demth16"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth16"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth16",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> TWW5_NT
      x_th["Umgebung_TS8","Umgebung_Demth17"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth17"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth17",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> TWW6_NT
      x_th["Umgebung_TS8","Umgebung_Demth18"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth18"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth18",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> RW1_NT
      x_th["Umgebung_TS8","Umgebung_Demth19"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth19"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth19",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> RW2_NT
      x_th["Umgebung_TS8","Umgebung_Demth20"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth20"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth20",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> RW3_NT
      x_th["Umgebung_TS8","Umgebung_Demth21"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth21"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth21",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> RW4_NT
      x_th["Umgebung_TS8","Umgebung_Demth22"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth22"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth22",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> RW5_NT
      x_th["Umgebung_TS8","Umgebung_Demth23"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth23"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth23",] <- 0#.08;
      #
      #th. Verbindung   PS_WP -> RW6_NT
      x_th["Umgebung_TS8","Umgebung_Demth24"] <- -1;
      x_th_eff["Umgebung_TS8","Umgebung_Demth24"] <- 1;
      x_th_price["Umgebung_TS8","Umgebung_Demth24",] <- 0#.08;
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
      x_th_price["Umgebung_TS7","Umgebung_Demth7",] <- 0#.08;
      #
      #th. Verbindung   PS_z -> RW2_HT
      x_th["Umgebung_TS7","Umgebung_Demth8"] <- -1;
      x_th_eff["Umgebung_TS7","Umgebung_Demth8"] <- 1;
      x_th_price["Umgebung_TS7","Umgebung_Demth8",] <- 0#.08;
      #
      #th. Verbindung   PS_z -> RW3_HT
      x_th["Umgebung_TS7","Umgebung_Demth9"] <- -1;
      x_th_eff["Umgebung_TS7","Umgebung_Demth9"] <- 1;
      x_th_price["Umgebung_TS7","Umgebung_Demth9",] <- 0#.08;
      #
      #th. Verbindung   PS_z -> RW4_HT
      x_th["Umgebung_TS7","Umgebung_Demth10"] <- -1;
      x_th_eff["Umgebung_TS7","Umgebung_Demth10"] <- 1;
      x_th_price["Umgebung_TS7","Umgebung_Demth10",] <- 0#.08;
      #
      #th. Verbindung   PS_z -> RW5_HT
      x_th["Umgebung_TS7","Umgebung_Demth11"] <- -1;
      x_th_eff["Umgebung_TS7","Umgebung_Demth11"] <- 1;
      x_th_price["Umgebung_TS7","Umgebung_Demth11",] <- 0#.08;
      #
      #th. Verbindung   PS_z -> RW6_HT
      x_th["Umgebung_TS7","Umgebung_Demth12"] <- -1;
      x_th_eff["Umgebung_TS7","Umgebung_Demth12"] <- 1;
      x_th_price["Umgebung_TS7","Umgebung_Demth12",] <- 0#.08;
      #
      #th. Verbindung   PS_dez1 -> TWW1_HT
      x_th["Umgebung_TS1","Umgebung_Demth1"] <- -1;
      x_th_eff["Umgebung_TS1","Umgebung_Demth1"] <- 1;
      x_th_price["Umgebung_TS1","Umgebung_Demth1",] <- 0#.08;
      #
      #th. Verbindung   PS_dez2 -> TWW2_HT
      x_th["Umgebung_TS2","Umgebung_Demth2"] <- -1;
      x_th_eff["Umgebung_TS2","Umgebung_Demth2"] <- 1;
      x_th_price["Umgebung_TS2","Umgebung_Demth2",] <- 0#.08;
      #
      #th. Verbindung   PS_dez3 -> TWW3_HT
      x_th["Umgebung_TS3","Umgebung_Demth3"] <- -1;
      x_th_eff["Umgebung_TS3","Umgebung_Demth3"] <- 1;
      x_th_price["Umgebung_TS3","Umgebung_Demth3",] <- 0#.08;
      #
      #th. Verbindung   PS_dez4 -> TWW4_HT
      x_th["Umgebung_TS4","Umgebung_Demth4"] <- -1;
      x_th_eff["Umgebung_TS4","Umgebung_Demth4"] <- 1;
      x_th_price["Umgebung_TS4","Umgebung_Demth4",] <- 0#.08;
      #
      #th. Verbindung   PS_dez5 -> TWW5_HT
      x_th["Umgebung_TS5","Umgebung_Demth5"] <- -1;
      x_th_eff["Umgebung_TS5","Umgebung_Demth5"] <- 1;
      x_th_price["Umgebung_TS5","Umgebung_Demth5",] <- 0#.08;
      #
      #th. Verbindung   PS_dez6 -> TWW6_HT
      x_th["Umgebung_TS6","Umgebung_Demth6"] <- -1;
      x_th_eff["Umgebung_TS6","Umgebung_Demth6"] <- 1;
      x_th_price["Umgebung_TS6","Umgebung_Demth6",] <- 0#.08;
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
      x_th_price["Umgebung_GaBo1","Umgebung_Demth7",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW2_HT
      x_th["Umgebung_GaBo1","Umgebung_Demth8"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth8"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth8",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW3_HT
      x_th["Umgebung_GaBo1","Umgebung_Demth9"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth9"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth9",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW4_HT
      x_th["Umgebung_GaBo1","Umgebung_Demth10"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth10"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth10",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW5_HT
      x_th["Umgebung_GaBo1","Umgebung_Demth11"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth11"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth11",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW6_HT
      x_th["Umgebung_GaBo1","Umgebung_Demth12"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth12"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth12",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> TWW1_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth13"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth13"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth13",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> TWW2_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth14"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth14"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth14",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> TWW3_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth15"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth15"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth15",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> TWW4_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth16"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth16"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth16",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> TWW5_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth17"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth17"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth17",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> TWW6_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth18"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth18"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth18",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW1_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth19"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth19"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth19",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW2_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth20"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth20"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth20",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW3_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth21"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth21"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth21",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW4_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth22"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth22"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth22",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW5_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth23"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth23"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth23",] <- 0#.08;
      #
      #th. Verbindung   Gasboiler -> RW6_NT
      x_th["Umgebung_GaBo1","Umgebung_Demth24"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_Demth24"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_Demth24",] <- 0#.08;
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
      if (Solver == 1) {
        #----Uebergabe des mathematischen Modells an den lp-Solver----
        vec_types <- length(LM.binary(LM_Umgbung))
        vec_types[LM.binary(LM_Umgbung) == TRUE] <- "B"
        vec_types[LM.binary(LM_Umgbung) == FALSE] <- "C"
        #
        vec_dir <- length(LM.direction(LM_Umgbung))
        vec_dir[LM.direction(LM_Umgbung) == "="] <- "=="
        vec_dir[LM.direction(LM_Umgbung) == "<="] <- "<="
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
                           "tm_limit" = 1 * 60 * 1000)
          )
      } else if (Solver == 2) {
        lptest <-
          lp (
            direction = "min",
            objective.in = LM.cost(LM_Umgbung),
            const.mat = LM.matrix(LM_Umgbung),
            const.dir = LM.direction(LM_Umgbung),
            const.rhs = LM.vector(LM_Umgbung),
            transpose.constraints = TRUE,
            #int.vec,
            presolve = 0,
            compute.sens = 0,
            binary.vec = LM.binary(LM_Umgbung),
            all.int = FALSE,
            all.bin = FALSE,
            scale = 196,
            #dense.const,
            num.bin.solns = 1,
            use.rw = FALSE
          )
      }
      #
      solution <-
        data.frame(name = coord(Umgebung), value = lptest$solution)
      var_nr <- dim(solution)[1] / Anzahl_Zeitschritte
      sol_mat <- lptest$solution
      sol_mat <- matrix(sol_mat, nrow = var_nr, byrow = TRUE)
      sol_mat <- t(sol_mat)
      #
      #----Auswertung der Forschleife mit Variablenbezeichnung versehen----
      tmp <- unique(str_split_fixed(coord(Umgebung), '_time', 2)[, 1])
      #
      Auswertung <- as.data.frame(sol_mat)
      colnames(Auswertung) <- tmp
      #
      return(Auswertung)
    }


