#----maxP_th_plus-----------------------------------------
#'maxP_th_plus (set)
#'
#'Set the maxP_th_plus of class GaBo in kW. If the maxP_th_plus is set to -1, the GaBo can produce thermal energy unlimited.
#'
#'@importFrom Root maxP_th_plus<-
#'@param object A GaBo
#'@param value A numeric
#'@return GaBo
#'@export
#'@examples
#'gabo <- new.GaBo()
#'maxP_th_plus(gabo) <- 10
setMethod("maxP_th_plus<-", signature(object = "GaBo",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_th_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_th_plus <- value
    if (length(effMaxP_th_plus(object))==1){
      object@maxP_fuel_minus <- value / effMaxP_th_plus(object)
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#---effMaxP_th_plus------------------------------------------
#'effMaxP_th_plus (set)
#'
#'Set the effMaxP_th_plus of class GaBo in \% (has to be bigger than 0)
#'
#'@importFrom Root effMaxP_th_plus<-
#'@param object A GaBo
#'@param value A numeric
#'@return GaBo
#'@examples
#'gabo <- new.GaBo()
#'effMaxP_th_plus(gabo) <- 0.33
#'@export
setMethod("effMaxP_th_plus<-", signature(object = "GaBo",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "effMaxP_th_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if(all(value > 1 | value <= 0)) {
    msg <- "effMaxP_th_plus has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@effMaxP_th_plus <- value
    if (length(maxP_th_plus(object))==1){
      object@maxP_fuel_minus <- maxP_th_plus(object) / value
    }
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})
