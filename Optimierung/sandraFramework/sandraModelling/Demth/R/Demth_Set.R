#----load_15min_th-----------------------------------------
#'load_15min_th (set)
#'
#'Set the load_15min_th of class Demth in kW per 15 min
#'
#'@importFrom Root load_15min_th<-
#'@param object A Demth
#'@param value A numeric
#'@return Demth
#'@export

setMethod("load_15min_th<-", signature(object = "Demth",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A demand always has to be positive or zero"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@load_15min_th <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})



#----load_abstract_th-----------------------------------------
#'load_abstract_th (set)
#'
#'Set the load_abstract_th of class Demth in kW per 15 min
#'
#'@importFrom Root load_abstract_th<-
#'@param object A Demth
#'@param value A numeric
#'@return Demth
#'@export

setMethod("load_abstract_th<-", signature(object = "Demth",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A demand always has to be positive or zero"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@load_abstract_th <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})
