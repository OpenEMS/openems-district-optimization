#----volume------------------------------------------------
#'volume (set)
#'
#'Set the volume of class TS in Liter
#'
#'@importFrom Root volume<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'volume(ts) <- 500
#'@export
setMethod("volume<-", signature(object = "TS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "volume has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0) {
    msg <- "volume has to be positiv"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@volume <- value
    if (length(maxTemp(object))*length(minTemp(object)) > 0){
      Energy <- value * 0.001163 * (maxTemp(object) - minTemp(object))
      object@maxE_th <- Energy
      if (length(percentageStart(object))==1){
        object@EnergyStart <- Energy * percentageStart(object)
      }
      if (length(percentageEnd(object))==1){
        if (percentageEnd(object)>=0) {
          object@EnergyEnd <- Energy * percentageEnd(object)
        }
        if (percentageEnd(object)==-1) {
          object@EnergyEnd <- -1
        }
      }
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
#----maxTemp-----------------------------------------------
#'maxTemp (set)
#'
#'Set the maxTemp of class TS in °C
#'
#'@importFrom Root maxTemp<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'maxTemp(ts) <- 90
#'@export

setMethod("maxTemp<-", signature(object = "TS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxTemp has to be of length one"
    errors <- c(msg, errors)
  }

  if(all(value > 100 | value < 0)) {
    msg <- "maxTemp has to be between 0 and 100"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@maxTemp <- value
    if (length(volume(object))*length(minTemp(object)) > 0){
      Energy <- volume(object) * 0.001163 * (value - minTemp(object))
      object@maxE_th <- Energy
      if (length(percentageStart(object))==1){
        object@EnergyStart <- Energy * percentageStart(object)
      }
      if (length(percentageEnd(object))==1){
        if (percentageEnd(object)>=0) {
          object@EnergyEnd <- Energy * percentageEnd(object)
        }
        if (percentageEnd(object)==-1) {
          object@EnergyEnd <- -1
        }
      }
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})

#----minTemp-----------------------------------------------
#'minTemp (set)
#'
#'Set the minTemp of class TS in °C
#'
#'@importFrom Root minTemp<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'minTemp(ts) <- 50
#'@export

setMethod("minTemp<-", signature(object = "TS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "minTemp has to be of length one"
    errors <- c(msg, errors)
  }

  if(all(value > 100 | value < 0)) {
    msg <- "minTemp has to be between 0 and 100"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@minTemp <- value
    if (length(maxTemp(object))*length(volume(object)) > 0){
      Energy <- volume(object) * 0.001163 * (maxTemp(object) - value)
      object@maxE_th <- Energy
      if (length(percentageStart(object))==1){
        object@EnergyStart <- Energy * percentageStart(object)
      }
      if (length(percentageEnd(object))==1){
        if (percentageEnd(object)>=0) {
          object@EnergyEnd <- Energy * percentageEnd(object)
        }
        if (percentageEnd(object)==-1) {
          object@EnergyEnd <- -1
        }
      }
    }
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})
#----maxP_th_plus------------------------------------------
#'maxP_th_plus (set)
#'
#'Set the maxP_th_plus of class TS in kW. If the maxP_th_plus is set to -1, the TS can produce thermal energy unlimited.
#'
#'@importFrom Root maxP_th_plus<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'maxP_th_plus(ts) <- 10
#'@export

setMethod("maxP_th_plus<-", signature(object = "TS",value="numeric"), function(object, value) {

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
  }else{
    print(errors)

  }
  return(object)
})
#----maxP_th_minus-----------------------------------------
#'maxP_th_minus (set)
#'
#'Set the maxP_th_minus of class TS in kW. If the maxP_th_minus is set to -1, the TS can consume thermal energy unlimited.
#'
#'@importFrom Root maxP_th_minus<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'maxP_th_minus(ts) <- 10
#'@export

setMethod("maxP_th_minus<-", signature(object = "TS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_th_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_th_minus has to be positive or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_th_minus <- value
  }else{
    print(errors)
  }
  return(object)
})
#----effMaxP_th_minus----------------------------------------
#'effMaxP_th_minus (set)
#'
#'Set the eff_P_th_minus of class TS in \% (has to be bigger than 0)
#'
#'@importFrom Root effMaxP_th_minus<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'effMaxP_th_minus(ts) <- 0.98
#'@export

setMethod("effMaxP_th_minus<-", signature(object = "TS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "effMaxP_th_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if(all(value > 1 | value <= 0)) {
    msg <- "effMaxP_th_minus has to be bigger than 0 and <= 1"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@effMaxP_th_minus <- value
  }else{
    print(errors)

  }
  return(object)
})
#----eff_losses--------------------------------------------
#'eff_losses (set)
#'
#'Set the eff_losses of class TS in \% (100 \% = no losses). Has to be bigger than 0.
#'
#'@importFrom Root eff_losses<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'eff_losses(ts) <- 0.98
#'@export

setMethod("eff_losses<-", signature(object = "TS",value="numeric"), function(object, value) {

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
# #----percentageOverheating---------------------------------
# #'percentageOverheating (set)
# #'
# #'Set the percentageOverheating of class TS in \%
# #'
# #'@param object A TS
# #'@param value A numeric
# #'@return TS
# #'@export
#
# setMethod("percentageOverheating<-", signature(object = "TS",value="numeric"), function(object, value) {
#
#   errors<-character();
#   if (length(value) != 1) {
#     msg <- "percentageOverheating has to be of length one"
#     errors <- c(msg, errors)
#   }
#
#   if(all(value > 1 | value < 0)) {
#     msg <- "percentageOverheating has to be between 0 and 1"
#     errors <- c(msg, errors)
#   }
#
#   if (length(errors)==0) {
#     object@percentageOverheating <- value
#   }else{
#     print(errors)
#   }
#   return(object)
# })

#---percentageEnd------------------------------------------
#'percentageEnd (set)
#'
#'Set the percentageEnd of the energy  of class TS in \%
#'
#'@importFrom Root percentageEnd<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'percentageEnd(ts) <- 0.5
#'@export

setMethod("percentageEnd<-", signature(object = "TS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "percentageEnd has to be of length one"
    errors <- c(msg, errors)
  }

  if(!((value <= 1 & value >= 0) | value == -1)) {
    msg <- "percentageEnd has to be between 0 and 1 or -1. -1 means deactivated"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@percentageEnd <- value
    if (length(maxE_th(object))==1){
      if (value>=0) {
        object@EnergyEnd <- maxE_th(object) * percentageEnd(object)
      }
      if (value==-1) {
        object@EnergyEnd <- -1
      }
    }
    validObject(object)
  }else{
    print(errors)

  }
  return(object)
})

#----percentageStart---------------------------------------
#'percentageStart (set)
#'
#'Set the percentageStart  of energy of class TS in \%
#'
#'@importFrom Root percentageStart<-
#'@param object A TS
#'@param value A numeric
#'@return TS
#'@examples
#'ts <- new.TS()
#'percentageStart(ts) <- 0.5
#'@export

setMethod("percentageStart<-", signature(object = "TS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "percentageStart has to be of length one"
    errors <- c(msg, errors)
  }

  if(!((value <= 1 & value >= 0) | value == -1)) {
    msg <- "percentageStart has to be between 0 and 1, -1 means E_start = E_end"
    errors <- c(msg, errors)
  }

  if (length(errors)==0) {
    object@percentageStart <- value
    if (length(maxE_th(object))==1){
      object@EnergyStart <- maxE_th(object) * percentageStart(object)
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
