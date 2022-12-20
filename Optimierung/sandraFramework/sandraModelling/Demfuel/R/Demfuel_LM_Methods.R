#----Overview----------------------------------------------
# LM.Constraints
#

#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a Demth by binding
#'
#'@importFrom Root LM.Constraints
#'@export

setMethod("LM.Constraints", signature(object = "Demfuel"),function(object){
  if(length(load_15min_fuel(object))*
     length(coord(object))>0 |  length(coord(object))*length(load_abstract_fuel(object))>0){


    n<-length(coord(object))
    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    M1<-LM.Profile_fuel_minus(object)

    M<-rbind(M,M1@matrix);
    V<-c(V,M1@vector);
    D<-c(D,M1@direction);

    C<-C+M1@cost#+price_fuel_minus(object)*colSums(P_fuel_minus(object));

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }
  stop(paste("Attributes of", object@name, "are missing: maxP_fuel_minus, coordinates are needed"))

})


