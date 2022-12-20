#----Overview----------------------------------------------
# LM.Constraints
#

#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a Prodfuel by binding
#'
#'@importFrom Root LM.Constraints
#'@export

setMethod("LM.Constraints", signature(object = "Prodfuel"),function(object){
  if(length(load_15min_fuel(object))*
     length(coord(object))>0 |
     length(load_abstract_fuel(object))*
     length(coord(object))>0){

    n<-length(coord(object))
    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    M1<-LM.Profile_fuel_plus(object)

    M<-rbind(M,M1@matrix);
    V<-c(V,M1@vector);
    D<-c(D,M1@direction);

    C<-C+M1@cost#+price_fuel_plus(object)*colSums(P_fuel_plus(object));

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }
  print(paste("Attributes of" ,object@name, "are missing: load_15min_fuel and coordinates or load_abstract_fuel are needed"))
  return(new("LM"))
})

