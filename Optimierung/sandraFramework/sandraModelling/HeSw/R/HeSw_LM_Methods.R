#----Overview----------------------------------------------
# LM.Constraints
#
#---LM.Constraints-----------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a HeSw by binding
#'
#'@importFrom Root LM.Constraints
#'@export
setMethod("LM.Constraints", signature(object = "HeSw"),function(object){

  if(length(maxP_el_minus(object))*
     length(effMaxP_th_plus(object))*
     length(coord(object))>0){

    n<-length(coord(object))

    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    if(maxP_th_plus(object)!=-1){
      M1 <- LM.maxP_th_plus(object)
      M <- rbind(M,M1@matrix)
      V <- c(V,M1@vector)
      D <- c(D, M1@direction)
      C <- C+M1@cost
    }

    M2<-LM.Transform_el_th(object)


    M<-rbind(M,
             M2@matrix);

    V<-c(V,
         M2@vector);

    D<-c(D,
         M2@direction
         );

    C<-C+M2@cost#+price_el_minus(object)*colSums(P_el_minus(object))+price_th_plus(object)*colSums(P_th_plus(object));

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }

  stop(paste("Attributes of", object@name ,"are missing: maxP_el_minus, effMaxP_th_plus and coordinates are needed"))

})
