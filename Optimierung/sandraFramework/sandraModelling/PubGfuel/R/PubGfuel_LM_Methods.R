#----Overview----------------------------------------------
#LM.Constraints

#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a PubGfuel by binding
#'
#'@importFrom Root LM.Constraints
#'@export


setMethod("LM.Constraints", signature(object = "PubGfuel"),function(object){
  if((length(maxP_fuel_minus(object))*
     length(minP_fuel_minus(object))*length(coord(object))>0)|(length(maxP_fuel_plus(object))*
     length(minP_fuel_plus(object))*
     length(coord(object))>0)){

    co<-coord(object)

    n<-length(coord(object))
    M <- matrix(0,0,n)
    colnames(M)<-co
    V <- numeric()
    D <- character()

    C <- rep(0,n)
    names(C)<-co

    if(length(maxP_fuel_plus(object)>0)){
      if(maxP_fuel_plus(object)!=-1){
      M1 <- LM.maxP_fuel_plus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }}

    if(length(minP_fuel_plus(object)>0)){
      if(minP_fuel_plus(object)>0){
      M1 <- LM.minP_fuel_plus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
      }
    }

    if(length(maxP_fuel_minus(object)>0)){
      if(maxP_fuel_minus(object)!=-1 ){
      M1 <- LM.maxP_fuel_minus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }}


    if(length(minP_fuel_minus(object)>0)){
      if(minP_fuel_minus(object)>0){
      M1 <- LM.minP_fuel_minus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
      }
    }



  if(length(M)>0){
    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)}else{return(new("LM"))}
  }
  stop(paste("Attributes of", object@name, "are missing: at least maxP_fuel_minus, minP_fuel_minus and coordinates are needed or minP_fuel_plus, maxP_fuel_plus and coordinates are needed"))

})

