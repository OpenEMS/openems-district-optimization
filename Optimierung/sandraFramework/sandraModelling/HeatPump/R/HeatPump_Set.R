#----Overview----------------------------------------------
# COP
# maxP_el_minus
# nominalP_th_plus
#----------------------------------------------------------

#----COP-----------------------------------------
#'COP (set)
#'
#'Set the COP of class HeatPump 
#'
#'@importFrom Root COP<-
#'@param object A HeatPump
#'@param value A numeric
#'@return HeatPump
#'@examples
#'HeatPump <- new.HeatPump()
#'COP(HeatPump) <- 2.8
#'@export
setMethod("COP<-", signature(object = "HeatPump", value="numeric"), function(object, value) {

  errors<-character()

  if(any(value < 0)) {
    msg <- "All values of COP have to be non-negativ"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@COP <- value
    if (length(COP_nominal(object))*
        length(COP(object))>0){
      object@maxP_th_plus <- (value )/(COP_nominal(object))
    }
    validObject(object)
    
    
    
  }else{
    print(errors)
  }
  return(object)
})



#----nominalP_th_plus------------------------------------------
#nominalP_th_plus (set)
#'
#'Set the nominalP_th_plus of class HeatPump in kW. 
#'
#'@importFrom Root nominalP_th_plus<-
#'@param object A HeatPump
#'@param value A numeric
#'@return HeatPump
#'@examples
#'HeatPump <- new.HeatPump()
#'nominalP_th_plus(HeatPump) <- 10
#'@export

setMethod("nominalP_th_plus<-", signature(object = "HeatPump",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "nominalP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "nominalP_th_plus has to be non-negativ"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@nominalP_th_plus <- value
    if (length(maxP_el_minus(object))>0){
      object@COP_nominal <- (value )/(maxP_el_minus(object))
    }
    
    if (length(COP_nominal(object))*
        length(COP(object))>0){
      object@maxP_th_plus <- (COP(object) )/(COP_nominal(object))
    }
  }else{
    print(errors)

  }
  return(object)
})


#----maxP_el_minus------------------------------------------
#maxP_el_minus (set)
#'
#'Set the maxP_el_minus of class HeatPump in kW. 
#'
#'@importFrom Root maxP_el_minus<-
#'@param object A HeatPump
#'@param value A numeric
#'@return HeatPump
#'@examples
#'HeatPump <- new.HeatPump()
#'maxP_el_minus(HeatPump) <- 5
#'@export

setMethod("maxP_el_minus<-", signature(object = "HeatPump",value="numeric"), function(object, value) {
  
  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }
  
  if (value < 0) {
    msg <- "maxP_el_minus has to be non-negativ"
    errors <- c(msg, errors)
  }
  
  
  if (length(errors)==0) {
    object@maxP_el_minus <- value
    if (length(nominalP_th_plus(object))>0){
      object@COP_nominal <- (nominalP_th_plus(object))/(value)
    }
    
    if (length(COP_nominal(object))*
        length(COP(object))>0){
      object@maxP_th_plus <- (COP(object) )/(COP_nominal(object))
    }
  }else{
    print(errors)
    
  }
  return(object)
})
