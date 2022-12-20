#----Overview----------------------------------------------
#LM.Constraints

#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a BAT by binding
#'
#'@importFrom Root LM.Constraints
#'@export

setMethod("LM.Constraints", signature(object = "Bat"),function(object){
  if(length(maxE_el(object))*
     length(EnergyEnd(object))*
     length(EnergyStart(object))*
     length(maxP_el_minus(object))*
     length(maxP_el_plus(object))*
     length(effMaxP_el_minus(object))*
     length(eff_losses(object))*
     length(coord(object))>0){

    n<-length(coord(object))

    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    if(maxP_el_plus(object)!=-1){
      M1<-LM.maxP_el_plus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }

    if(maxP_el_minus(object)!=-1){
      M1<-LM.maxP_el_minus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }

    if(maxP_el_minus(object)!=-1 &
       maxP_el_plus(object)!=-1 ){
      M1<-LM.maxP_el(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost


    }

    M3 <- LM.E_el(object)
    M4 <- LM.maxE_el(object)

    if(length(object@fullChargingCycles)*length(object@lifeTime)>0){
    M5 <-LM.maxChargingCycles(object)

    M<-rbind(M,M3@matrix,M4@matrix,M5@matrix);
    V<-c(V,M3@vector,M4@vector,M5@vector);
    D<-c(D,M3@direction,M4@direction,M5@direction);

    C<-C+M3@cost+M4@cost+M5@cost#+price_el_plus(object)*colSums(P_el_plus(object))+price_el_minus(object)*colSums(P_el_minus(object));
    }else{
    M<-rbind(M,M3@matrix,M4@matrix);
    V<-c(V,M3@vector,M4@vector);
    D<-c(D,M3@direction,M4@direction);

    C<-C+M3@cost+M4@cost#+price_el_plus(object)*colSums(P_el_plus(object))+price_el_minus(object)*colSums(P_el_minus(object));
    }
    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }
  stop(paste("Attributes of", object@name, "are missing: maxE_el, EnergyEnd, EnergyStart, maxP_el_minus, maxP_el_plus, effMaxP_el_minus, eff_losses,coordinates are needed"))

})

