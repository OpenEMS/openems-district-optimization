#----Overview----------------------------------------------
#LM.Constraints

#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a PubGel by binding
#'@importFrom Root LM.Constraints
#'
#'@export


setMethod("LM.Constraints", signature(object = "PubGel"),function(object){
  if((length(maxP_el_minus(object))*
      length(minP_el_minus(object))*length(coord(object))>0)|(length(maxP_el_plus(object))*
                                                                length(minP_el_plus(object))*
                                                                length(coord(object))>0)){

    co<-coord(object)

    n<-length(co)
    M <- matrix(0,0,n)
    colnames(M)<-co
    V <- numeric()
    D <- character()

      if(length(maxP_el_plus(object)>0)){
        if(maxP_el_plus(object)!=-1){
        M1 <- LM.maxP_el_plus(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost
      }}

      if(length(minP_el_plus(object)>0)){
        if(minP_el_plus(object)>0){
        M1 <- LM.minP_el_plus(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost}
      }

      if( length(maxP_el_minus(object)>0)){
        if(maxP_el_minus(object)!=-1){
        M1 <- LM.maxP_el_minus(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost}
      }


      if( length(minP_el_minus(object)>0)){
        if(minP_el_minus(object)>0){
        M1 <- LM.minP_el_minus(object)
        M <- rbind(M,M1@matrix)
        V <- c(V,M1@vector)
        D <- c(D, M1@direction)
        C <- C+M1@cost}
      }


    if(length(M)>0){

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
    
    }else{
      return(new("LM"))
      }
  }
  stop(paste("Attributes of", object@name, "are missing: at least maxP_el_minus, minP_el_minus and coordinates are needed or minP_el_plus, maxP_el_plus and coordinates are needed"))

})

