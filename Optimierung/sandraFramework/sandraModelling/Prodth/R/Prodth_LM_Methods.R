#----Overview----------------------------------------------
# LM.Constraints
#

#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a Prodth by binding
#'
#'@importFrom Root LM.Constraints
#'@export

setMethod("LM.Constraints", signature(object = "Prodth"),function(object){
  if(length(load_15min_th(object))*
     length(coord(object))>0 |
     length(load_abstract_th(object))*
     length(coord(object))>0){

    n<-length(coord(object))
    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    M1<-LM.Profile_th_plus(object)

    M<-rbind(M,M1@matrix);
    V<-c(V,M1@vector);
    D<-c(D,M1@direction);

    C<-C+M1@cost#+price_th_plus(object)*colSums(P_th_plus(object));

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }
  print(paste("Attributes of" ,object@name, "are missing: load_15min_th and coordinates or load_abstract_th are needed"))
  return(new("LM"))
})

