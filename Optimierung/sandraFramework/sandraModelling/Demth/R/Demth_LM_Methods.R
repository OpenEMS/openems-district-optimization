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
#'@examples
#'demth<-new.Demth()
#'load_15min_th(demth) <- c(10, 4.2, 5, 6)
#'price_th_minus(demth) <- 0.08
#'name(demth) <- "Heat demand"
#'timegrid(demth) <- c(5,10,15,30)
#'x<-LM.Constraints(demth)
#'a<-as.data.frame.LM(x)
#'a
#'
setMethod("LM.Constraints", signature(object = "Demth"),function(object){
  if(length(load_15min_th(object))*
     length(coord(object))>0| length(coord(object))*length(load_abstract_th(object))>0){

    n<-length(coord(object))
    M <- matrix(0,0,n)
    V <- numeric()
    D <- character()
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co


    M1<-LM.Profile_th_minus(object)

    M<-rbind(M,M1@matrix);
    V<-c(V,M1@vector);
    D<-c(D,M1@direction);

    C<-C+M1@cost#+price_th_minus(object)*colSums(P_th_minus(object));

    m<-new("LM",matrix = M,direction = D,vector=V,cost =C ,binary=grepl("Op_",coord(object)))
    return(m)
  }
  stop(paste("Attributes of", object@name, "are missing: load_15min_th or load_abstract_th and coordinates are needed"))
})

