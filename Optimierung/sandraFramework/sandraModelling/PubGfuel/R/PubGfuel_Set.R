# ---maxP_fuel_minus------------------------------------------------------
#' maxP_fuel_minus (set)
#'
#'Set the maxP_fuel_minus of class PubGfuel in kW. If the maxP_fuel_minus is set to -1, the PubGfuel can consume fuel unlimited.
#'
#'@importFrom Root maxP_fuel_minus<-
#'@param object A PubGfuel
#'@param value A numeric
#'@return PubGfuel
#'@examples
#'pubgfuel <- new.PubGfuel()
#'#add variable "P_fuel_minus"
#'variables(pubgfuel) <- "P_fuel_minus"
#'maxP_fuel_minus(pubgfuel) <- 10
#'@export

setMethod("maxP_fuel_minus<-", signature(object = "PubGfuel",value="numeric"), function(object, value) {
  if("P_fuel_minus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_fuel_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_fuel_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_fuel_minus <- value
    validObject(object)
  }else{
    print(errors)

  }
  return(object)}else{stop("The variable P_fuel_minus is needed")}
})
# ---minP_fuel_minus -------------------------------------------------
#'  minP_fuel_minus (set)
#'
#'Set the  minP_fuel_minus of class PubGfuel in kW.
#'
#'@importFrom Root minP_fuel_minus<-
#'@param object A PubGfuel
#'@param value A numeric
#'@return PubGfuel
#'@examples
#'pubgfuel <- new.PubGfuel()
#'#add variable "P_fuel_minus"
#'variables(pubgfuel) <- "P_fuel_minus"
#'minP_fuel_minus(pubgfuel) <- 5
#'@export


setMethod("minP_fuel_minus<-", signature(object = "PubGfuel",value="numeric"), function(object, value) {
  if("P_fuel_minus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "minP_fuel_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "minP_fuel_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }



  if (length(errors)==0) {
    object@minP_fuel_minus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)}else{stop("The variable P_fuel_minus is needed")}
})


# ---maxP_fuel_plus------------------------------------------------------
#' maxP_fuel_plus (set)
#'
#'Set the maxP_fuel_plus of class PubGfuel in kW. If the maxP_fuel_plus is set to -1, the PubGfuel can produce fuel unlimited.
#'
#'@importFrom Root maxP_fuel_plus<-
#'@param object A PubGfuel
#'@param value A numeric
#'@return PubGfuel
#'@examples
#'pubgfuel <- new.PubGfuel()
#'#add variable "P_fuel_plus"
#'variables(pubgfuel) <- "P_fuel_plus"
#'maxP_fuel_plus(pubgfuel) <- 15
#'@export


setMethod("maxP_fuel_plus<-", signature(object = "PubGfuel",value="numeric"), function(object, value) {
  if("P_fuel_plus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_fuel_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_fuel_plus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_fuel_plus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)}else{stop("The variable P_fuel_plus is needed")}
})
# ---minP_fuel_plus------------------------------------------------------
#' minP_fuel_plus (set)
#'
#'Set the minP_fuel_plus of class PubGfuel
#'
#'@importFrom Root minP_fuel_plus<-
#'@param object A PubGfuel
#'@param value A numeric
#'@return PubGfuel
#'@examples
#'pubgfuel <- new.PubGfuel()
#'variables(pubgfuel) <- "P_fuel_plus"
#'minP_fuel_plus(pubgfuel) <- 5
#'@export

setMethod("minP_fuel_plus<-", signature(object = "PubGfuel",value="numeric"), function(object, value) {
  if("P_fuel_plus" %in% variables(object)){
  errors<-character();
  if (length(value) != 1) {
    msg <- "minP_fuel_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "minP_fuel_plus has to be positiv"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@minP_fuel_plus <- value
    validObject(object)
  }else{
    print(errors)
  }
  return(object)}else{stop("The variable P_fuel_plus is needed")}
})

