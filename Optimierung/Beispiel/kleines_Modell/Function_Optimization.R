Optimierung <- function(Demand_Power,
                        Demand_Heat,
                        Production_PV,
                        ThermalStorage_Init,
                        CHP_Init,
                        Anzahl_Zeitschritte, Zeitintervall) {
  
      #----Lade alle noetigen Pakages----
      library(Root)
      library(TS)
      library(Demth)
      library(Demel)
      library(PubGel)
      library(PubGth)
      library(PubGfuel)
      library(GaBo)
      library(CHP)
      library(PV)
      library(Sandbox)
      library(Rglpk)
      #
      #----Erdgasnetz erzeugen----
      Gas_Netz <- new.PubGfuel()
      maxP_fuel_plus(Gas_Netz) <- -1 # -1 unbeschränkt
      minP_fuel_plus(Gas_Netz) <- 0
      #
      #----Künstliche thermische Quelle erzeugen----
      kunstQuell_th <- new.PubGth()
      maxP_th_plus(kunstQuell_th) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
      minP_th_plus(kunstQuell_th) <- 0
      #
      #----Stromnetz erzeugen----
      Strom_Netz <- new.PubGel() # Vorzeichen vertauscht, wegen Elektroylseur als CHP. Siehe Anmerkungen oben
      maxP_el_minus(Strom_Netz) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
      minP_el_minus(Strom_Netz) <- 0
      maxP_el_plus(Strom_Netz) <- -1
      minP_el_plus(Strom_Netz) <- 0
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
      maxP_el_plus(BHKW) <- 60
      minP_el_plus(BHKW) <- 60
      effMaxP_el_plus(BHKW) <- 0.386
      effMinP_el_plus(BHKW) <- 0.386
      effMaxP_th_plus(BHKW) <- 0.447
      effMinP_th_plus(BHKW) <- 0.447
      price_maintenance(BHKW) <- 0#.03    #Wartungskosten
      minDowntime(BHKW) <- 0
      minRuntime(BHKW) <- 2
      initial_state(BHKW) <- CHP_Init
      initial_stateEndurance(BHKW) <- 0
      #
      #----Puffer erzeugen----
      ThermalStorage <- new.TS()
      info(ThermalStorage) <- list(name="ThermalStorage")
      volume(ThermalStorage) <- 3000
      maxTemp(ThermalStorage) <- 90
      minTemp(ThermalStorage) <- 70
      maxP_th_plus(ThermalStorage) <- 1000
      maxP_th_minus(ThermalStorage) <- 1000
      effMaxP_th_minus(ThermalStorage) <- 1
      eff_losses(ThermalStorage) <- 1
      percentageStart(ThermalStorage) <- ThermalStorage_Init
      percentageEnd(ThermalStorage) <- -1
      #
      #----Photovoltaik erzeugen----
      PV <- new.PV()
      info(PV) <- list(name='PV')
      #
      #----Waermebedarf erzeugen----
      HeatDemand <- new.Demth()
      info(HeatDemand) <- list(name='HeatDemand')
      #
      #----Strombedarf erzeugen----
      PowerDemand <- new.Demel()
      info(PowerDemand) <- list(name='PowerDemand')
      #
      #----Simulationsumgebung----
      Umgebung <- new.Sandbox()
      name(Umgebung) <- "Umgebung"
      info(Umgebung) <- list(name = "EdgeOpt")
      timegrid(Umgebung) <- rep(Zeitintervall, Anzahl_Zeitschritte)
      timestep_15min <- 96
      timestep_div <- 15*timestep_15min/Anzahl_Zeitschritte
      #
      #----Profile initialisieren----
      load_15min_th(HeatDemand) <- Demand_Heat
      load_15min_el(PowerDemand) <- Demand_Power
      load_15min_el(PV) <- Production_PV
      #
      #------Simulationsumgebung zusammenfuehren----
      components(Umgebung) <- list(Gas_Netz, # PubGfuel1
                                   Strom_Netz, # PubGel1
                                   Gasboiler, 
                                   BHKW, 
                                   ThermalStorage, #TS1
                                   HeatDemand, #Demth1
                                   PowerDemand, #Demel1
                                   PV,
                                   kunstQuell_th) #PubGth2
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
      #fuel Verbindungen Gas_Netz -> Gasboiler1
      x_fuel["Umgebung_PubGfuel1","Umgebung_GaBo1"] <- -1;
      x_fuel_eff["Umgebung_PubGfuel1","Umgebung_GaBo1"] <- 1;
      x_fuel_price["Umgebung_PubGfuel1","Umgebung_GaBo1",] <- 0.065; # Gaspreis 
      #
      #fuel Verbindungen Gas_Netz -> BHKW
      x_fuel["Umgebung_PubGfuel1","Umgebung_CHP1"] <- -1;
      x_fuel_eff["Umgebung_PubGfuel1","Umgebung_CHP1"] <- 1;
      x_fuel_price["Umgebung_PubGfuel1","Umgebung_CHP1",] <- 0.065; # Gaspreis    
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
      ####
      #
      #el Verbindungen BHKW -> Strom_Netz
      x_el["Umgebung_CHP1","Umgebung_PubGel1"] <- -1;
      x_el_eff["Umgebung_CHP1","Umgebung_PubGel1"] <- 1;
      x_el_price["Umgebung_CHP1","Umgebung_PubGel1",] <- -0.08-0.035 # KWK-Förderung + Base
      #
      #el Verbindungen BHKW -> Strombedarf
      x_el["Umgebung_CHP1","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_CHP1","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_CHP1","Umgebung_Demel1",] <- -0.04+0.065 # KWK-Eigenstromzulage - EEG-Umlage
      #
      #el Verbindungen PV -> Strom_Netz
      x_el["Umgebung_PV1","Umgebung_PubGel1"] <- -1;
      x_el_eff["Umgebung_PV1","Umgebung_PubGel1"] <- 1;
      x_el_price["Umgebung_PV1","Umgebung_PubGel1",] <- -0.1 # EEG-Förderung
      #
      #el Verbindungen PV -> Strombedarf
      x_el["Umgebung_PV1","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_PV1","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_PV1","Umgebung_Demel1",] <- 0 # EEG-Umlagenbefreit für PV < 30 kWp
      #
      #el Verbindungen Strom_Netz -> Strombedarf
      x_el["Umgebung_PubGel1","Umgebung_Demel1"] <- -1;
      x_el_eff["Umgebung_PubGel1","Umgebung_Demel1"] <- 1;
      x_el_price["Umgebung_PubGel1","Umgebung_Demel1",] <- 0.25 # Stromtarif
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
      #
      #### Hochtemperatur
      #
      #th. Verbindung   Gasboiler -> ThermalStorage
      x_th["Umgebung_GaBo1","Umgebung_TS1"] <- -1;
      x_th_eff["Umgebung_GaBo1","Umgebung_TS1"] <- 1;
      x_th_price["Umgebung_GaBo1","Umgebung_TS1",] <- 0;
      #
      #th. Verbindung   BHKW -> ThermalStorage
      x_th["Umgebung_CHP1","Umgebung_TS1"] <- -1;
      x_th_eff["Umgebung_CHP1","Umgebung_TS1"] <- 1;
      x_th_price["Umgebung_CHP1","Umgebung_TS1",] <- 0;
      #
      #th. Verbindung   ThermalStorage -> Wärmebedarf
      x_th["Umgebung_TS1","Umgebung_Demth1"] <- -1;
      x_th_eff["Umgebung_TS1","Umgebung_Demth1"] <- 1;
      x_th_price["Umgebung_TS1","Umgebung_Demth1",] <- 0;
      #
      ####
      ########## Künstliche Quelle
      ####
      #th. Verbindung   kunstQuell_th -> ThermalStorage # Zur Sicherheit, dass ein Ergebnis gefunden wird
      x_th["Umgebung_PubGth1","Umgebung_TS1"] <- -1;
      x_th_eff["Umgebung_PubGth1","Umgebung_TS1"] <- 1;
      x_th_price["Umgebung_PubGth1","Umgebung_TS1",] <- 100000000;
      #
      adjacency_th(Umgebung)<- x_th;
      adjacencyEff_th(Umgebung)<- x_th_eff;
      adjacencyPrice_th(Umgebung)<- x_th_price;
      #
      #----Setzen der Koordinaten----
      Umgebung <- finalcoordinates(Umgebung)
      #
      #----Erstellen des mathematischen Modells----
      LM_Umgbung <- LM.Constraints(Umgebung)
      #

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
                          "tm_limit" = 60 * 1000))
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

