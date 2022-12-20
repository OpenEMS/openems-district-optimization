#----Overview----------------------------------------------
# LM.Constraints
#
#---LM.Constraints-----------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a CHP by binding
#'@importFrom Root LM.Constraints
#'
#'@export

setMethod("LM.Constraints", signature(object = "CHP"),function(object){

  if( length(maxP_el_plus(object))*
      length(minP_el_plus(object))*
      length(effMaxP_el_plus(object))*
      length(effMaxP_th_plus(object))*
      length(effMinP_el_plus(object))*
      length(effMinP_th_plus(object))*
      length(price_maintenance(object))*
      length(minDowntime(object))*
      length(minRuntime(object))*
      length(coord(object))>0){

    n<-length(coord(object))


    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    if(maxP_th_plus(object)!=-1){
      M1 <- LM.maxP_th_plus_Op(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }
    if(minP_th_plus(object)!=0){
    M1 <- LM.minP_th_plus_Op(object)
    M <- rbind(M,M1@matrix)
    V <- c(V,M1@vector)
    D <- c(D, M1@direction)
    C <- C+M1@cost
    }

    M3<-LM.Transform_fuel_el(object)
    M4<-LM.Transform_fuel_th(object)
    M5<-LM.minDowntime(object)
    M6<-LM.minRuntime(object)
    M7<-LM.modulated(object)
  

    M<-rbind(M, M3@matrix, M4@matrix, M5@matrix, M6@matrix, M7@matrix);

    V<-c(V,M3@vector, M4@vector, M5@vector, M6@vector, M7@vector);

    D<-c(D,M3@direction, M4@direction, M5@direction, M6@direction, M7@direction);
      #
    C<-C+M3@cost+M4@cost+M5@cost+M6@cost+M6@cost+price_maintenance(object)*Root::tau(object)*colSums(Op(object))#*maxP_el_plus(object)#+price_el_plus(object)*colSums(P_el_plus(object))+price_th_plus(object)*colSums(P_th_plus(object))+
    #  price_fuel_minus(object)*colSums(P_fuel_minus(object))+M5@cost+M6@cost

    if(length(load_P_th_plus(object))>0){
      M7<-LM.P_th_plus(object)
      M<-rbind(M,M7@matrix)
      V<-c(V,M7@vector)
      D<-c(D,M7@direction)
      C<-C+M7@cost
    }

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }

  stop(paste("Attributes of", object@name, "are missing: minP_el_plus, effMaxP_el_plus, effMaxP_th_plus, effMinP_el_plus, effMinP_th_plus, price_maintenance, minRuntime, minDowntime,  coord
        are needed"))
})
