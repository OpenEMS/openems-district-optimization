
#'create a model with battery
#'@export


sandbox_bat<-function(p_max,E_start,E_end,steps){
  #construct el. Demand


  p_min<- 2 ;
  half <- floor(steps/2)
  demand_el <- c(seq(from = p_min,to = p_max,length.out = half + 1),
                 seq(from = p_max,to = p_min,length.out = steps - half )[-1])
  #empty sandbox
  sandbox <- new.Sandbox();
  name(sandbox)<-"ShaveIt"
  timegrid(sandbox) <-rep(60,steps);

  #define battery
  bat<-Bat::new.Bat();
  maxP_el_plus(bat) <- p_max/2;
  maxP_el_minus(bat) <- p_max/2;
  maxE_el(bat) <- p_max/2;
  percentageStart(bat) <- E_start;
  eff_losses(bat) <- 1;
  effMaxP_el_minus(bat) <-1;
  percentageEnd(bat) <- E_end;

  #define demand
  dem <- new.Demel();
  load_abstract_el(dem)<-demand_el;

  #define public grid
  pub_grid <- PubGel::new.PubGel();
  variables(pub_grid) <- c("P_el_plus","P_el_minus");
  maxP_el_minus(pub_grid) <- -1
  maxP_el_plus(pub_grid) <- -1
  minP_el_minus(pub_grid) <- 0
  minP_el_plus(pub_grid) <- 0

  #put objects in sandbox
  components(sandbox)<-list(dem,bat,pub_grid);

  #topology
  nam <- compoNames(sandbox);
  dum <- matrix(0,length(nam),length(nam));
  colnames(dum)<-nam;
  row.names(dum)<-nam;

  x_top<-dum;


  x_top["ShaveIt_Bat1","ShaveIt_PubGel1"] <- -1;
  x_top["ShaveIt_Bat1","ShaveIt_Demel1"] <- -1;
  x_top["ShaveIt_PubGel1","ShaveIt_Demel1"] <- -1;
  x_top["ShaveIt_PubGel1","ShaveIt_Bat1"] <- -1;

  adjacency_el(sandbox) <- x_top;

  dum <- array(0,dim = c(length(nam),length(nam),steps));
  dimnames(dum)<-list(nam,nam,paste0("t",1:steps))

  x_price<-dum;

  x_price["ShaveIt_Bat1","ShaveIt_PubGel1",] <- -10;
  x_price["ShaveIt_Bat1","ShaveIt_Demel1",] <- 0;
  x_price["ShaveIt_PubGel1","ShaveIt_Demel1",] <- 20;
  x_price["ShaveIt_PubGel1","ShaveIt_Bat1",] <- 20;

  adjacencyPrice_el(sandbox) <- x_price;

  sandbox <- finalcoordinates(sandbox)

  sandbox


}

#'@export
sandbox_bat_only<-function(p_max,E_start,E_end,steps){



  #empty sandbox
  sandbox <- new.Sandbox();
  name(sandbox)<-"ShaveIt"
  timegrid(sandbox) <-rep(60,steps);

  #define battery
  bat<-Bat::new.Bat();
  maxP_el_plus(bat) <- p_max;
  maxP_el_minus(bat) <- p_max;
  maxE_el(bat) <- p_max
  percentageStart(bat) <- E_start;
  eff_losses(bat) <- 1;
  effMaxP_el_minus(bat) <-1;
  percentageEnd(bat) <- E_end;


  #define public grid
  pub_grid <- new.PubGel();
  variables(pub_grid) <- c("P_el_plus","P_el_minus");
  maxP_el_minus(pub_grid) <- -1
  maxP_el_plus(pub_grid) <- -1
  minP_el_minus(pub_grid) <- 0
  minP_el_plus(pub_grid) <- 0

  #put objects in sandbox
  components(sandbox)<-list(bat,pub_grid);

  #topology
  nam <- compoNames(sandbox);
  dum <- matrix(0,length(nam),length(nam));
  colnames(dum)<-nam;
  row.names(dum)<-nam;

  x_top<-dum;


  x_top["ShaveIt_Bat1","ShaveIt_PubGel1"] <- -1;
  x_top["ShaveIt_PubGel1","ShaveIt_Bat1"] <- -1;

  adjacency_el(sandbox) <- x_top;

  dum <- array(0,dim = c(length(nam),length(nam),steps));
  dimnames(dum)<-list(nam,nam,paste0("t",1:steps))

  x_price<-dum;

  x_price["ShaveIt_Bat1","ShaveIt_PubGel1",] <- - 10;

  x_price["ShaveIt_PubGel1","ShaveIt_Bat1",] <- 20;

  adjacencyPrice_el(sandbox) <- x_price;

  sandbox <- finalcoordinates(sandbox)

  sandbox


}


#'create a model with battery
#'@export


telekom<-function(timegrid,E_max,P_max=NULL,eta_in=0.9,eta_standby=0.97,E_start=NULL,E_end=NULL){
  if(is.null(P_max)){
    P_max <- E_max
  }
  if(is.null(E_end)){
    E_end <- -1
  }
  if(is.null(E_start)){
    E_start <- -1
    E_end <- -1
  }
  #empty sandbox
  sandbox <- new.Sandbox();
  name(sandbox)<-"Telekom"
  timegrid(sandbox) <-timegrid;

  #define battery
  bat<-Bat::new.Bat();
  maxP_el_plus(bat) <- P_max;
  maxP_el_minus(bat) <- P_max;
  maxE_el(bat) <- E_max;
  if(E_start/E_max>=0){
  percentageStart(bat) <- E_start/E_max;
  }else{
  percentageStart(bat) <- -1
  }
  eff_losses(bat) <- eta_standby^{1/30};
  effMaxP_el_minus(bat) <-eta_in;
  if(E_end/E_max>=0){
  percentageEnd(bat) <- E_end/E_max;
  }else{
  percentageEnd(bat) <- -1
  }
  #define demand
  dem <- new.Demel();


  #define public grid
  pub_grid <- PubGel::new.PubGel();
  variables(pub_grid) <- c("P_el_plus","P_el_minus");
  maxP_el_minus(pub_grid) <- -1
  maxP_el_plus(pub_grid) <- -1
  minP_el_minus(pub_grid) <- 0
  minP_el_plus(pub_grid) <- 0

  #put objects in sandbox
  components(sandbox)<-list(dem,bat,pub_grid);

  #topology
  nam <- compoNames(sandbox);
  dum <- matrix(0,length(nam),length(nam));
  colnames(dum)<-nam;
  row.names(dum)<-nam;

  x_top<-dum;


  x_top["Telekom_Bat1","Telekom_PubGel1"] <- -1;
  x_top["Telekom_Bat1","Telekom_Demel1"] <- -1;
  x_top["Telekom_PubGel1","Telekom_Demel1"] <- -1;
  x_top["Telekom_PubGel1","Telekom_Bat1"] <- -1;

  adjacency_el(sandbox) <- x_top;

  dum <- array(0,dim = c(length(nam),length(nam),length(timegrid)));
  dimnames(dum)<-list(nam,nam,paste0("t",1:length(timegrid)))




  adjacencyPrice_el(sandbox) <- dum;

  sandbox <- finalcoordinates(sandbox)

  sandbox


}

#'@export
sandbox_bat_only<-function(p_max,E_start,E_end,steps){



  #empty sandbox
  sandbox <- new.Sandbox();
  name(sandbox)<-"ShaveIt"
  timegrid(sandbox) <-rep(60,steps);

  #define battery
  bat<-Bat::new.Bat();
  maxP_el_plus(bat) <- p_max;
  maxP_el_minus(bat) <- p_max;
  maxE_el(bat) <- p_max
  percentageStart(bat) <- E_start;
  eff_losses(bat) <- 1;
  effMaxP_el_minus(bat) <-1;
  percentageEnd(bat) <- E_end;


  #define public grid
  pub_grid <- new.PubGel();
  variables(pub_grid) <- c("P_el_plus","P_el_minus");
  maxP_el_minus(pub_grid) <- -1
  maxP_el_plus(pub_grid) <- -1
  minP_el_minus(pub_grid) <- 0
  minP_el_plus(pub_grid) <- 0

  #put objects in sandbox
  components(sandbox)<-list(bat,pub_grid);

  #topology
  nam <- compoNames(sandbox);
  dum <- matrix(0,length(nam),length(nam));
  colnames(dum)<-nam;
  row.names(dum)<-nam;

  x_top<-dum;


  x_top["ShaveIt_Bat1","ShaveIt_PubGel1"] <- -1;
  x_top["ShaveIt_PubGel1","ShaveIt_Bat1"] <- -1;

  adjacency_el(sandbox) <- x_top;

  dum <- array(0,dim = c(length(nam),length(nam),steps));
  dimnames(dum)<-list(nam,nam,paste0("t",1:steps))

  x_price<-dum;

  x_price["ShaveIt_Bat1","ShaveIt_PubGel1",] <- - 10;

  x_price["ShaveIt_PubGel1","ShaveIt_Bat1",] <- 20;

  adjacencyPrice_el(sandbox) <- x_price;

  sandbox <- finalcoordinates(sandbox)

  sandbox


}

#'@export
model_bat_shave<-function(p_max,a,b,steps){

  sandbox<-sandbox_bat(p_max,a,b,steps)
  LM.Shave(sandbox,rep(T,steps),"ShaveIt_PubGel1")

}



#'@export

sandbox_chp <- function(grid,forecast,stable){
  #
  #
  #
  stopifnot(is.numeric(grid))

  stopifnot(is.data.frame(forecast))
  stopifnot(is.logical(stable))

  stopifnot(all(colnames(forecast)==c("Pth_Demth1","Price_DA")))

  #
  #
  #--------------------------------------------------------
  #Umgebung aufsetzen
  sandbox<-new.Sandbox();
  name(sandbox)<-"RoitherBerg";

  timegrid(sandbox)<-grid;
  timestep <- length(grid)
  #
  #--------------------------------------------------------
  #machines
  #--------------------------------------------------------
  #
  #
  #BHKW1
  chp1<-new.CHP();
  maxP_el_plus(chp1)<-140;
  minP_el_plus(chp1)<-70;
  effMaxP_el_plus(chp1)<-0.3;
  effMinP_el_plus(chp1)<-0.3;
  effMaxP_th_plus(chp1)<-0.6;
  effMinP_th_plus(chp1)<-0.6;
  price_maintenance(chp1)<-0;
  minRuntime(chp1)<-3
  #
  #--------------------------------------------------------
  #thermischer Verbraucher
  dem_th<-new.Demth();
  load_abstract_th(dem_th)<-forecast[,"Pth_Demth1"]
  #
  #--------------------------------------------------------
  #
  #
  #Warmwasserspeicher
  puff<-new.TS();
  maxTemp(puff)<-90;
  minTemp(puff)<-70;
  volume(puff)<-40000;
  maxP_th_minus(puff)<- -1
  maxP_th_plus(puff)<- -1
  effMaxP_th_minus(puff)<- 1
  eff_losses(puff)<- 0.999
  percentageStart(puff)<-0.5
  percentageEnd(puff)<- -1
  #
  #--------------------------------------------------------
  #
  #Stromnetz anlegen
  pubgel <- new.PubGel()
  #Damit in das Stromnetz Strom eingespeist werden kann, wird die Variable "P_el_minus" ben?tigt
  variables(pubgel)<-"P_el_minus"
  #Stromeinspeisung unbeschraenkt
  maxP_el_minus(pubgel) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
  #keine Mindesteinspeisung fordern
  minP_el_minus(pubgel) <- 0
  #
  #--------------------------------------------------------
  #
  #gasnetz anlegen
  pubgfuel <- new.PubGfuel()
  #Damit in das Stromnetz Strom eingespeist werden kann, wird die Variable "P_el_minus" benoetigt
  variables(pubgfuel)<-"P_fuel_plus"
  #Stromeinspeisung unbeschraenkt
  maxP_fuel_plus(pubgfuel) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
  #keine Mindesteinspeisung fordern
  minP_fuel_plus(pubgfuel) <- 0
  #
  #--------------------------------------------------------
  #
  #define sandbox
  #
  #--------------------------------------------------------
  #
  components(sandbox)<-list(pubgfuel,puff,dem_th,chp1,pubgel);
  #
  #--------------------------------------------------------
  #
  #dummy matrix for topologies
  #
  nam<-compoNames(sandbox);
  dum<-matrix(0,length(nam),length(nam));
  colnames(dum)<-nam
  row.names(dum)<-nam
  #
  #--------------------------------------------------------
  #
  #topology and graphs
  #
  #--------------------------------------------------------
  #
  #electric graph
  x<-dum;
  x_price<-array(dum,dim = c(dim(dum),length(grid)));
  dimnames(x_price) <- list(row.names(dum), colnames(dum),paste0("price_t",1:length(grid)))
  #el. Verbindungen BHKW
  x["RoitherBerg_CHP1","RoitherBerg_PubGel1"] <- - 1;
  x_price["RoitherBerg_CHP1","RoitherBerg_PubGel1",] <- forecast$Price_DA ;


  adjacency_el(sandbox) <- x
  adjacencyEff_el(sandbox)<- matrix(as.numeric(x!=0),dim(x)[1]);
  adjacencyPrice_el(sandbox) <- x_price
  #
  #--------------------------------------------------------
  #
  #thermal graph
  x<-dum;
  x_price<-dum;
  #th. Verbindungen BHKW
  x["RoitherBerg_CHP1","RoitherBerg_TS1"] <- -1;

  #th. Verbindungen Warmwasserspeicher
  x["RoitherBerg_TS1","RoitherBerg_Demth1"] <- -1;
  x_price["RoitherBerg_TS1","RoitherBerg_Demth1"] <- -10;


  adjacency_th(sandbox)<- x;
  adjacencyEff_th(sandbox)<- matrix(as.numeric(x!=0),dim(x)[1]);
  adjacencyPrice_th(sandbox)<- x_price;
  #
  #--------------------------------------------------------
  #
  #fuel graph
  x<-dum;
  x_price<-dum;
  #fuel Verbindungen BHKW
  x["RoitherBerg_PubGfuel1","RoitherBerg_CHP1"] <- -1;


  x_price["RoitherBerg_PubGfuel1","RoitherBerg_CHP1"] <- 5;



  adjacency_fuel(sandbox)<- x;
  adjacencyEff_fuel(sandbox)<- matrix(as.numeric(x!=0),dim(x)[1]);
  adjacencyPrice_fuel(sandbox)<- x_price;
  #
  #--------------------------------------------------------
  #
  #setzen der Koordinaten
  #
  sandbox<-finalcoordinates(sandbox)
  #
  #stabelize, if neccessary
  #
  if(stable){
    sandbox <- stabilize(sandbox)
  }

  return(sandbox)

}





#'@export

sandbox_2chp <- function(grid,forecast,stable){
  #
  #
  #
  stopifnot(is.numeric(grid))

  stopifnot(is.data.frame(forecast))
  stopifnot(is.logical(stable))

  stopifnot(all(colnames(forecast)==c("Pth_Demth1","Price_DA")))

  #
  #
  #--------------------------------------------------------
  #Umgebung aufsetzen
  sandbox<-new.Sandbox();
  name(sandbox)<-"RoitherBerg";

  timegrid(sandbox)<-grid;
  timestep <- length(grid)
  #
  #--------------------------------------------------------
  #machines
  #--------------------------------------------------------
  #
  #
  #BHKW1
  chp1<-new.CHP();
  maxP_el_plus(chp1)<-140;
  minP_el_plus(chp1)<-70;
  effMaxP_el_plus(chp1)<-0.3;
  effMinP_el_plus(chp1)<-0.3;
  effMaxP_th_plus(chp1)<-0.6;
  effMinP_th_plus(chp1)<-0.6;
  price_maintenance(chp1)<-0;
  minRuntime(chp1)<-3
  #
  chp2 <- chp1
  #
  #--------------------------------------------------------
  #thermischer Verbraucher
  dem_th<-new.Demth();
  load_abstract_th(dem_th)<-forecast[,"Pth_Demth1"]
  #
  #--------------------------------------------------------
  #
  #
  #Warmwasserspeicher
  puff<-new.TS();
  maxTemp(puff)<-90;
  minTemp(puff)<-70;
  volume(puff)<-40000;
  maxP_th_minus(puff)<- -1
  maxP_th_plus(puff)<- -1
  effMaxP_th_minus(puff)<- 1
  eff_losses(puff)<- 0.999
  percentageStart(puff)<-0.5
  percentageEnd(puff)<- -1
  #
  #--------------------------------------------------------
  #
  #Stromnetz anlegen
  pubgel <- new.PubGel()
  #Damit in das Stromnetz Strom eingespeist werden kann, wird die Variable "P_el_minus" ben?tigt
  variables(pubgel)<-"P_el_minus"
  #Stromeinspeisung unbeschraenkt
  maxP_el_minus(pubgel) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
  #keine Mindesteinspeisung fordern
  minP_el_minus(pubgel) <- 0
  #
  #--------------------------------------------------------
  #
  #gasnetz anlegen
  pubgfuel <- new.PubGfuel()
  #Damit in das Stromnetz Strom eingespeist werden kann, wird die Variable "P_el_minus" benoetigt
  variables(pubgfuel)<-"P_fuel_plus"
  #Stromeinspeisung unbeschraenkt
  maxP_fuel_plus(pubgfuel) <- -1 #-1 entspricht keiner Maximalgrenze ->unbeschraenkt
  #keine Mindesteinspeisung fordern
  minP_fuel_plus(pubgfuel) <- 0
  #
  #--------------------------------------------------------
  #
  #define sandbox
  #
  #--------------------------------------------------------
  #
  components(sandbox)<-list(pubgfuel,puff,dem_th,chp1,chp2,pubgel);
  #
  #--------------------------------------------------------
  #
  #dummy matrix for topologies
  #
  nam<-compoNames(sandbox);
  dum<-matrix(0,length(nam),length(nam));
  colnames(dum)<-nam
  row.names(dum)<-nam
  #
  #--------------------------------------------------------
  #
  #topology and graphs
  #
  #--------------------------------------------------------
  #
  #electric graph
  x<-dum;
  x_price<-array(dum,dim = c(dim(dum),length(grid)));
  dimnames(x_price) <- list(row.names(dum), colnames(dum),paste0("price_t",1:length(grid)))
  #el. Verbindungen BHKW
  x["RoitherBerg_CHP1","RoitherBerg_PubGel1"] <- - 1;
  x_price["RoitherBerg_CHP1","RoitherBerg_PubGel1",] <- forecast$Price_DA ;
  #
  x["RoitherBerg_CHP2","RoitherBerg_PubGel1"] <- - 1;
  x_price["RoitherBerg_CHP2","RoitherBerg_PubGel1",] <- forecast$Price_DA ;


  adjacency_el(sandbox) <- x
  adjacencyEff_el(sandbox)<- matrix(as.numeric(x!=0),dim(x)[1]);
  adjacencyPrice_el(sandbox) <- x_price
  #
  #--------------------------------------------------------
  #
  #thermal graph
  x<-dum;
  x_price<-dum;
  #th. Verbindungen BHKW
  x["RoitherBerg_CHP1","RoitherBerg_TS1"] <- -1;
  x["RoitherBerg_CHP2","RoitherBerg_TS1"] <- -1;
  #th. Verbindungen Warmwasserspeicher
  x["RoitherBerg_TS1","RoitherBerg_Demth1"] <- -1;
  x_price["RoitherBerg_TS1","RoitherBerg_Demth1"] <- -10;


  adjacency_th(sandbox)<- x;
  adjacencyEff_th(sandbox)<- matrix(as.numeric(x!=0),dim(x)[1]);
  adjacencyPrice_th(sandbox)<- x_price;
  #
  #--------------------------------------------------------
  #
  #fuel graph
  x<-dum;
  x_price<-dum;
  #fuel Verbindungen BHKW
  x["RoitherBerg_PubGfuel1","RoitherBerg_CHP1"] <- -1;
  x_price["RoitherBerg_PubGfuel1","RoitherBerg_CHP1"] <- 5;
  x["RoitherBerg_PubGfuel1","RoitherBerg_CHP2"] <- -1;


  x_price["RoitherBerg_PubGfuel1","RoitherBerg_CHP2"] <- 5;


  adjacency_fuel(sandbox)<- x;
  adjacencyEff_fuel(sandbox)<- matrix(as.numeric(x!=0),dim(x)[1]);
  adjacencyPrice_fuel(sandbox)<- x_price;
  #
  #--------------------------------------------------------
  #
  #setzen der Koordinaten
  #
  sandbox<-finalcoordinates(sandbox)
  #
  #stabelize, if neccessary
  #
  if(stable){
    sandbox <- stabilize(sandbox)
  }

  return(sandbox)

}
