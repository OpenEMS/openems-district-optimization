#----maxE_el-------------------------
#' maxE_el (get)
#'
#' Get maxE_el of Bat
#'@importFrom Root maxE_el
#'@param object A Bat
#'@return  maxE_el of Bat
#'@export
#'
setMethod("maxE_el", signature(object = "Bat"), function(object) {
  return(object@maxE_el)
})
#
# #----minE_el-------------------------
# # minE_el (get)
# #'
# # Get minE_el of Bat
# #'
# #'@param object A Bat
# #'@return  minE_el of Bat
# #'@export
# #'
# setMethod("minE_el", signature(object = "Bat"), function(object) {
#   return(object@minE_el)
# })
#

#----maxP_el_minus-------------------------
#' maxP_el_minus (get)
#'
#' Get maxP_el_minus of Bat
#'
#'@importFrom Root maxP_el_minus
#'@param object A Bat
#'@return  maxP_el_minus of Bat
#'@export
#'
setMethod("maxP_el_minus", signature(object = "Bat"), function(object) {
  return(object@maxP_el_minus)
})

#----maxP_el_plus-------------------------
#' maxP_el_plus (get)
#'
#' Get maxP_el_plus of Bat
#'
#'@importFrom Root maxP_el_plus
#'@param object A Bat
#'@return  maxP_el_plus of Bat
#'@export
#'
setMethod("maxP_el_plus", signature(object = "Bat"), function(object) {
  return(object@maxP_el_plus)
})

#----effMaxP_el_minus-------------------------
#' effMaxP_el_minus (get)
#'
#' Get effMaxP_el_minus of Bat
#'
#'@importFrom Root effMaxP_el_minus
#'@param object A Bat
#'@return  effMaxP_el_minus of Bat
#'@export
#'
setMethod("effMaxP_el_minus", signature(object = "Bat"), function(object) {
  return(object@effMaxP_el_minus)
})
#----eff_losses-------------------------
#' eff_losses (get)
#'
#' Get eff_losses of Bat
#'
#'@importFrom Root eff_losses
#'@param object A Bat
#'@return  eff_losses of Bat
#'@export
#'
setMethod("eff_losses", signature(object = "Bat"), function(object) {
  return(object@eff_losses)
})


#----percentageEnd-------------------------
#' percentageEnd (get)
#'
#' Get percentageEnd of Bat
#'
#'@importFrom Root percentageEnd
#'
#'@param object A Bat
#'@return  percentageEnd of Bat
#'@export
#'
setMethod("percentageEnd", signature(object = "Bat"), function(object) {
  return(object@percentageEnd)
})

#----percentageStart-------------------------
#' percentageStart (get)
#'
#' Get percentageStart of Bat
#'
#'@importFrom Root percentageStart
#'@param object A Bat
#'@return  percentageStart of Bat
#'@export
#'
setMethod("percentageStart", signature(object = "Bat"), function(object) {
  return(object@percentageStart)
})

#----EnergyStart-------------------------------------------
#'EnergyStart (get)
#'
#'Get EnergyStart of Bat in kWh
#'
#'@importFrom Root EnergyStart
#'@param object A Bat
#'@return  EnergyStart of Bat
#'@export
#'
setMethod("EnergyStart", signature(object = "Bat"), function(object) {
  return(object@EnergyStart)
})
#----EnergyEnd---------------------------------------------
#'EnergyEnd (get)
#'
#'Get EnergyEnd of Bat in kWh
#'
#'@importFrom Root EnergyEnd
#'@param object A Bat
#'@return  EnergyEnd of Bat
#'@export
#'
setMethod("EnergyEnd", signature(object = "Bat"), function(object) {
  return(object@EnergyEnd)
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
setMethod("lifeTime", signature(object = "Bat"), function(object) {
  return(object@lifeTime)
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
setMethod("fullChargingCycles", signature(object = "Bat"), function(object) {
  return(object@fullChargingCycles)
})



