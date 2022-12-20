#----Overview----------------------------------------------
# LM.Constraints
#
#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a FS by binding
#'
#'@importFrom Root LM.Constraints
#'@export

setMethod("LM.Constraints", signature(object = "FS"),function(object){
  if(length(maxE_fuel(object))*
     length(EnergyEnd(object))*
     length(EnergyStart(object))*
     length(maxP_fuel_minus(object))*
     length(maxP_fuel_plus(object))*
     length(eff_losses(object))*
     length(coord(object))>0){

    n<-length(coord(object))
    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    if(maxP_fuel_plus(object)!=-1){
      M1 <- LM.maxP_fuel_plus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }

    if(maxP_fuel_plus(object)!=-1){
      M1 <- LM.maxP_fuel_minus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }

    if(maxP_fuel_plus(object)!=-1 &
       maxP_fuel_minus(object)!=-1){
      M1 <- LM.maxP_fuel(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
      }


    M3<- LM.E_fuel(object)
    M4<- LM.maxE_fuel(object)

    M<-rbind(M,M3@matrix,M4@matrix);
    V<-c(V,M3@vector,M4@vector);
    D<-c(D,M3@direction,M4@direction);

    C<-C+M3@cost+M4@cost#+price_th_plus(object)*colSums(P_fuel_plus(object))+price_th_minus(object)*colSums(P_fuel_minus(object));

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }
  stop(paste("Attributes of", object@name, "are missing: maxE_fuel, EnergyEnd, EnergyStart, maxP_fuel_minus, maxP_fuel_plus, eff_losses and coordinates are needed"))

})

