#----Overview----------------------------------------------
#LM.Constraints

#----LM.Constraints-------------------------------------------------
#'LM.Constraints
#'
#'Build up the constraints for the linear modell of a Assel by binding
#'
#'@importFrom Root LM.Constraints
#'@export

setMethod("LM.Constraints", signature(object = "Assel"),function(object){


    n<-length(coord(object));
    M<-matrix(0,0,n);
    co<-coord(object)
    C <- rep(0,n)
    names(C)<-co

    return(new("LM",
           matrix = M,
           direction = character(),
           vector = numeric(),
           cost = C,
           binary = rep(F,n)))

})
