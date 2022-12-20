# ---maxE_el------------------------------------------------------
#' maxE_el (set)
#'
#'Set the maxE_el of class Bat
#'
#'@importFrom Root maxE_el<-
#'@param object A Bat
#'@param value A numeric
#'@return Bat
#'@examples
#'bat<-new.Bat()
#'maxE_el(bat)<-30
#'@export


setMethod("maxE_el<-", signature(object = "Bat",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxE_el has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "maxE_el has to be positiv"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxE_el <- value
    if (length(percentageStart(object))==1){
      object@EnergyStart <- maxE_el(object) * percentageStart(object)
    }
    if (length(percentageEnd(object))==1){
      object@EnergyEnd <- maxE_el(object) * percentageEnd(object)
    }
  }else{
    print(errors)

  }
  return(object)
})

# ---maxP_el_plus------------------------------------------------------
#' maxP_el_plus (set)
#'
#'Set the maxP_el_plus of class Bat in kW. If the maxP_el_plus is set to -1, the battery can produce electric energy unlimited.
#'
#'@importFrom Root maxP_el_plus<-
#'@param object A Bat
#'@param value A numeric
#'@return Bat
#'@examples
#'bat<-new.Bat()
#'maxP_el_plus(bat)<- -1
#'@export


setMethod("maxP_el_plus<-", signature(object = "Bat",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_el_plus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_el_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxP_el_plus <- value
  }else{
    print(errors)

  }
  return(object)
})


# ---maxP_el_minus------------------------------------------------------
#' maxP_el_minus (set)
#'
#'Set the maxP_el_minus of class Bat. If the maxP_el_minus is set to -1, the battery can consume electric energy unlimited.
#'
#'@importFrom Root maxP_el_minus<-
#'@param object A Bat
#'@param value A numeric
#'@return Bat
#'@examples
#'bat <- new.Bat()
#'maxP_el_minus(bat) <- -1
#'@export


setMethod("maxP_el_minus<-", signature(object = "Bat",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }
  if (value < 0 & value != -1) {
    msg <- "maxP_el_minus has to be positiv or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_el_minus <- value
  }else{
    print(errors)

  }
  return(object)
})


# ---effMaxP_el_minus------------------------------------------------------
#' effMaxP_el_minus (set)
#'
#'Set the effMaxP_el_minus of class Bat in \% (has to bigger than 0)
#'
#'@importFrom Root effMaxP_el_minus<-
#'@param object A Bat
#'@param value A numeric
#'@return Bat
#'@examples
#'bat <- new.Bat()
#'effMaxP_el_minus(bat) <- 0.9
#'@export


setMethod("effMaxP_el_minus<-", signature(object = "Bat",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "effMaxP_el_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if(all(value > 1 | value <= 0)) {
    msg <- "effMaxP_el_minus has to be bigger than 0 and  <= 1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@effMaxP_el_minus <- value
  }else{
    print(errors)

  }
  return(object)
})

# ---eff_losses------------------------------------------------------
#' eff_losses (set)
#'
#'Set the eff_losses of class Bat in \% (has to be bigger than 0)
#'
#'@importFrom Root eff_losses<-
#'@param object A Bat
#'@param value A numeric
#'@return Bat
#'@examples
#'bat<-new.Bat()
#'eff_losses(bat)<-0.9
#'@export


setMethod("eff_losses<-", signature(object = "Bat",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "eff_losses has to be of length one"
    errors <- c(msg, errors)
  }

  if(all(value > 1 | value <= 0)) {
    msg <- "eff_losses has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@eff_losses <- value
  }else{
    print(errors)

  }
  return(object)
})

# ---percentageEnd------------------------------------------------------
#' percentageEnd (set)
#'
#'Set the percentageEnd of class Bat in \%
#'
#'@importFrom Root percentageEnd<-
#'@param object A Bat
#'@param value A numeric
#'@return Bat
#'@examples
#'bat<-new.Bat()
#'percentageEnde(bat)<-0.9
#'@export


setMethod("percentageEnd<-", signature(object = "Bat",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "percentageEnd has to be of length one"
    errors <- c(msg, errors)
  }

  if(!((value <= 1 & value >= 0)|value == -1)) {
    msg <- "percentageEnd has to be between 0 and 1 or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@percentageEnd <- value
    if (length(maxE_el(object))==1){
      object@EnergyEnd <- maxE_el(object) * percentageEnd(object)
      validObject(object)
    }
  }else{
    print(errors)

  }
  return(object)
})

# ---percentageStart------------------------------------------------------
#' percentageStart (set)
#'
#'Set the percentageStart of class Bat
#'
#'@importFrom Root percentageStart<-
#'@param object A Bat
#'@param value A numeric
#'@return Bat
#'@examples
#'bat<-new.Bat()
#'percentageStart(bat)<-0.8
#'@export


setMethod("percentageStart<-", signature(object = "Bat",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "percentageStart has to be of length one"
    errors <- c(msg, errors)
  }

  if(!((value <= 1 & value >= 0)|value == -1)) {
    msg <- "percentageStart has to be between 0 and 1 or -1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@percentageStart <- value
    if (length(maxE_el(object))==1){
      object@EnergyStart <- maxE_el(object) * percentageStart(object)
      validObject(object)
    }
  }else{
    print(errors)

  }
  return(object)
})


#----lifeTime---------------------------------------------
#'lifeTime (get)
#'
#'Get lifeTime of Bat in years
#'
#'@param object A Bat
#'@return  lifeTime of Bat
#'@export
#'
setMethod("lifeTime<-", signature(object = "Bat"), function(object,value) {

  object@lifeTime<-value
  return(object)
})


#----fullChargingCycles---------------------------------------------
#'fullChargingCycles (get)
#'
#'Get fullChargingCycles of Bat in years
#'
#'@param object A Bat
#'@return  fullChargingCycles of Bat
#'@export
#'
setMethod("fullChargingCycles<-", signature(object = "Bat"), function(object,value) {
  object@fullChargingCycles <- value
  return(object)
})




