#----load_15min_fuel-----------------------------------------
#'load_15min_fuel (set)
#'
#'Set the load_15min_fuel of class Prodfuel in kW per 15 min
#'
#'@importFrom Root load_15min_fuel<-
#'@param object A Prodfuel
#'@param value A numeric
#'@return Prodfuel
#'@export

setMethod("load_15min_fuel<-", signature(object = "Prodfuel",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A production always has to be positive or zero"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@load_15min_fuel <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})




#----load_abstract_fuel-----------------------------------------
#'load_abstract_fuel (set)
#'
#'Set the load_abstract_fuel of class Prodfuel in kW per 15 min
#'
#'@importFrom Root load_abstract_fuel<-
#'@param object A Prodfuel
#'@param value A numeric
#'@return Prodfuel
#'@export

setMethod("load_abstract_fuel<-", signature(object = "Prodfuel",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A production always has to be positive or zero"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@load_abstract_fuel <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})

