#----Overview----------------------------------------------
# LM.Constraints
#
#---LM.Constraints-----------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a Wado by binding
#'@importFrom Root LM.Constraints
#'
#'@export

setMethod("LM.Constraints", signature(object = "Wado"),function(object){

  if( (length(maxP_el_plus(object))*length(maxP_el_minus(object))*length(coord(object))>0 &
        all(c("P_el_plus","P_el_minus") %in% variables(object))) |
      (length(maxP_fuel_plus(object))*length(maxP_fuel_minus(object))*length(coord(object))>0 &
       all(c("P_fuel_plus","P_fuel_minus") %in% variables(object)))|
      (length(maxP_th_plus(object))*length(maxP_th_minus(object))*length(coord(object))>0 &
       all(c("P_th_plus","P_th_minus") %in% variables(object)))){

    n<-length(coord(object))
    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    if(length(maxP_el_plus(object))*length(maxP_el_minus(object))*length(minP_el_plus(object))*length(minP_el_minus(object))>0 &
       all(c("P_el_plus","P_el_minus") %in% variables(object))){


      if(maxP_el_plus(object)!=-1){
        M1 <- LM.maxP_el_plus_Op(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost
      }
      if(maxP_el_minus(object)!=-1){
        M1 <- LM.maxP_el_minus_Op(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost
      }

      M3<-LM.Tunnel_el(object)

      M<-rbind(M, M3@matrix)

      V<-c(V,M3@vector)
      D<-c(D,M3@direction)
      C<-C+M3@cost}


    if(length(maxP_fuel_plus(object))*length(maxP_fuel_minus(object))*length(minP_fuel_plus(object))*length(minP_fuel_minus(object))>0 &
       all(c("P_fuel_plus","P_fuel_minus") %in% variables(object))){


      if(maxP_fuel_plus(object)!=-1){
        M1 <- LM.maxP_fuel_plus_Op(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost
      }
      if(maxP_fuel_minus(object)!=-1){
        M1 <- LM.maxP_fuel_minus_Op(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost
      }

      M6<-LM.Tunnel_fuel(object)


      M<-rbind(M, M6@matrix)
      V<-c(V,M6@vector)
      D<-c(D,M6@direction)
      C<-C+M6@cost
      }

    if(length(maxP_th_plus(object))*length(maxP_th_minus(object))>0 &
       all(c("P_th_plus","P_th_minus") %in% variables(object))){


      if(maxP_th_plus(object)!=-1){
        M1 <- LM.maxP_th_plus_Op(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost
      }

      if(maxP_th_minus(object)!=-1){
        M1 <- LM.maxP_th_minus_Op(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost
      }

      M9<-LM.Tunnel_th(object)


      M<-rbind(M, M9@matrix)

      V<-c(V,M9@vector)
      D<-c(D,M9@direction)
      C<-C+M9@cost}

  if(length(M)>0){
    m <-
      new(
        "LM",
        matrix = M,
        direction = D,
        vector = V,
        cost = C ,
        binary = grepl("Op_", coord(object))
      )

    return(m)
  }else{return(new("LM"))}
  }

  stop(paste("Attributes  of", object@name, "are missing: (maxP_el_plus, maxP_el_minus, variables:P_el_plus, P_el_minus and coordinates) or (maxP_fuel_plus,maxP_fuel_minus , variables:P_fuel_plus, P_fuel_minus and coordinates) or (maxP_th_plus, maxP_th_minus, variables:P_th_plus, P_th_minus and coordinates)  are needed"))

})
