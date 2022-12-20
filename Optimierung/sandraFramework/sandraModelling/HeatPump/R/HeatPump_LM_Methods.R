#----Overview----------------------------------------------
# LM.Constraints
#
#---LM.Constraints-----------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a heat pump by binding
#'all needed constraints together
#'
#'@importFrom Root LM.Constraints
#'@export

setMethod("LM.Constraints", signature(object = "HeatPump"),function(object){

  if(length(maxP_th_plus(object))*
     length(maxP_el_minus(object))*
     length(nominalP_th_plus(object))*
     length(COP(object))*
     length(COP_nominal(object))*
     length(coord(object))>0){

    n<-length(coord(object))

    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

  
    M1 <- LM.Transform_el_th_COP(object)
    M <- rbind(M,M1@matrix)
    V <- c(V,M1@vector)
    D <- c(D, M1@direction)
    C <- C+M1@cost
    
    M2<-LM.maxP_th_plus_nominal(object)


    M<-rbind(M,
             M2@matrix)

    V<-c(V,
         M2@vector)

    D<-c(D,
         M2@direction
    )

    C<-C+M2@cost
    
    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }

  stop(paste("Attributes of", object@name , "are missing: nominalP_th_plus, maxPel_plus, COP, maxPth_plus, COP_nominal and coord are needed"))

})
