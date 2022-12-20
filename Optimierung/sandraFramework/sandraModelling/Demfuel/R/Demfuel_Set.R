#----load_15min_fuel---------------------------------------
#'load_15min_fuel (set)
#'
#'Set the load_15min_fuel of class Demfuel in kW per 15 min
#'
#'@importFrom Root load_15min_fuel<-
#'@param object A Demfuel
#'@param value A numeric
#'@return Demfuel
#'@examples
#'demfuel<-new.Demfuel()
#'load_15min_fuel(demfuel) <- c(10, 4.2, 5, 6)
#'@export

setMethod("load_15min_fuel<-", signature(object = "Demfuel",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A demand always has to be positive or zero"
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


#----load_abstract_fuel---------------------------------------
#'load_abstract_fuel (set)
#'
#'Set the load_abstract_fuel of class Demfuel in kW per 15 min
#'
#'@importFrom Root load_abstract_fuel<-
#'@param object A Demfuel
#'@param value A numeric
#'@return Demfuel
#'@examples
#'demfuel<-new.Demfuel()
#'load_abstract_fuel(demfuel) <- c(10, 4.2, 5, 6)
#'@export

setMethod("load_abstract_fuel<-", signature(object = "Demfuel",value="numeric"), function(object, value) {

  errors<-character();
  if (!all(value>=0)) {
    msg <- "A demand always has to be positive or zero"
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

