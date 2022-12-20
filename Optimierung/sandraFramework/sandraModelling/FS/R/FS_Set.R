#----volume------------------------------------------------
#'volume (set)
#'
#'Set the volume of class FS in Liter
#'
#'@importFrom Root volume<-
#'@param object A FS
#'@param value A numeric
#'@return FS
#'@examples
#'fs <- new.FS()
#'volume(fs) <- 500
#'@export
setMethod("volume<-", signature(object = "FS",value="numeric"), function(object, value) {

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
    if (length(cal(object)!=0)){
      Energy <- value *cal(object) 
      object@maxE_fuel <- Energy
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
#----cal-----------------------------------------------
#'cal (set)
#'
#'Set the cal of class FS 
#'
#'@importFrom Root cal<-
#'@param object A FS
#'@param value A numeric
#'@return FS
#'@examples
#'ts <- new.FS()
#'cal(ts) <- 90
#'@export

setMethod("cal<-", signature(object = "FS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "cal has to be of length one"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@cal <- value
    if (length(volume(object)) > 0){
      Energy <- volume(object) *(value )
      object@maxE_fuel <- Energy
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

#----maxP_fuel_plus------------------------------------------
#'maxP_fuel_plus (set)
#'
#'Set the maxP_fuel_plus of class FS in kW. If the maxP_fuel_plus is set to -1, the FS can produce thermal energy unlimited.
#'
#'@importFrom Root maxP_fuel_plus<-
#'@param object A FS
#'@param value A numeric
#'@return FS
#'@examples
#'ts <- new.FS()
#'maxP_fuel_plus(ts) <- 10
#'@export

setMethod("maxP_fuel_plus<-", signature(object = "FS",value="numeric"), function(object, value) {

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
  }else{
    print(errors)

  }
  return(object)
})
#----maxP_fuel_minus-----------------------------------------
#'maxP_fuel_minus (set)
#'
#'Set the maxP_fuel_minus of class FS in kW. If the maxP_fuel_minus is set to -1, the FS can consume thermal energy unlimited.
#'
#'@importFrom Root maxP_fuel_minus<-
#'@param object A FS
#'@param value A numeric
#'@return FS
#'@examples
#'ts <- new.FS()
#'maxP_fuel_minus(ts) <- 10
#'@export

setMethod("maxP_fuel_minus<-", signature(object = "FS",value="numeric"), function(object, value) {

  errors<-character();
  if (length(value) != 1) {
    msg <- "maxP_fuel_minus has to be of length one"
    errors <- c(msg, errors)
  }

  if (value < 0 & value != -1) {
    msg <- "maxP_fuel_minus has to be positive or -1"
    errors <- c(msg, errors)
  }


  if (length(errors)==0) {
    object@maxP_fuel_minus <- value
  }else{
    print(errors)
  }
  return(object)
})

#----eff_losses--------------------------------------------
#'eff_losses (set)
#'
#'Set the eff_losses of class FS in \% (100 \% = no losses). Has to be bigger than 0.
#'
#'@importFrom Root eff_losses<-
#'@param object A FS
#'@param value A numeric
#'@return FS
#'@examples
#'ts <- new.FS()
#'eff_losses(ts) <- 0.98
#'@export

setMethod("eff_losses<-", signature(object = "FS",value="numeric"), function(object, value) {

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
# #'Set the percentageOverheating of class FS in \%
# #'
# #'@param object A FS
# #'@param value A numeric
# #'@return FS
# #'@export
#
# setMethod("percentageOverheating<-", signature(object = "FS",value="numeric"), function(object, value) {
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
#'Set the percentageEnd of the energy  of class FS in \%
#'
#'@importFrom Root percentageEnd<-
#'@param object A FS
#'@param value A numeric
#'@return FS
#'@examples
#'ts <- new.FS()
#'percentageEnd(ts) <- 0.5
#'@export

setMethod("percentageEnd<-", signature(object = "FS",value="numeric"), function(object, value) {

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
    if (length(maxE_fuel(object))==1){
      if (value>=0) {
        object@EnergyEnd <- maxE_fuel(object) * percentageEnd(object)
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
#'Set the percentageStart  of energy of class FS in \%
#'
#'@importFrom Root percentageStart<-
#'@param object A FS
#'@param value A numeric
#'@return FS
#'@examples
#'ts <- new.FS()
#'percentageStart(ts) <- 0.5
#'@export

setMethod("percentageStart<-", signature(object = "FS",value="numeric"), function(object, value) {

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
    if (length(maxE_fuel(object))==1){
      object@EnergyStart <- maxE_fuel(object) * percentageStart(object)
    }
    validObject(object)
  }else{
    print(errors)
  }
  return(object)
})
