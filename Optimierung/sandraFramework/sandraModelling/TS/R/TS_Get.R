#----volume------------------------------------------------
#'volume (get)
#'
#'Get volume of TS in liter
#'
#'@importFrom Root volume
#'@param object A TS
#'@return  volume of TS
#'@export
#'
setMethod("volume", signature(object = "TS"), function(object) {
  return(object@volume)
})
#----maxE_th---------------------------------------------
#'maxE_th (get)
#'
#'Get maxE_th of TS in kWh
#'
#'@importFrom Root maxE_th
#'@param object A TS
#'@return  maxE_th of TS
#'@export
#'
setMethod("maxE_th", signature(object = "TS"), function(object) {
  return(object@maxE_th)
})
#----maxTemp-----------------------------------------------
#'maxTemp (get)
#'
#'Get maxTemp of TS in °C
#'
#'@importFrom Root maxTemp
#'@param object A TS
#'@return  maxTemp of TS
#'@export
#'
setMethod("maxTemp", signature(object = "TS"), function(object) {
  return(object@maxTemp)
})
#----minTemp-----------------------------------------------
#'minTemp (get)
#'
#'Get minTemp of TS in °C
#'
#'@importFrom Root minTemp
#'@param object A TS
#'@return  minTemp of TS
#'@export
#'
setMethod("minTemp", signature(object = "TS"), function(object) {
  return(object@minTemp)
})
#----maxP_th_minus-----------------------------------------
#'maxP_th_minus (get)
#'
#'Get maxP_th_minus of TS in kW
#'
#'@importFrom Root maxP_th_minus
#'@param object A TS
#'@return  maxP_th_minus of TS
#'@export
#'
setMethod("maxP_th_minus", signature(object = "TS"), function(object) {
  return(object@maxP_th_minus)
})
#----maxP_th_plus------------------------------------------
#'maxP_th_plus (get)
#'
#'Get maxP_th_plus of TS in kW
#'
#'@importFrom Root maxP_th_plus
#'@param object A TS
#'@return  maxP_th_plus of TS
#'@export
#'
setMethod("maxP_th_plus", signature(object = "TS"), function(object) {
  return(object@maxP_th_plus)
})
#----effMaxP_th_minus----------------------------------------
#'effMaxP_th_minus (get)
#'
#'Get eff_P_th_minus of TS in \%
#'
#'@importFrom Root effMaxP_th_minus
#'@param object A TS
#'@return  effMaxP_th_minus of TS
#'@export
#'
setMethod("effMaxP_th_minus", signature(object = "TS"), function(object) {
  return(object@effMaxP_th_minus)
})
#----eff_losses--------------------------------------------
#'eff_losses (get)
#'
#'Get eff_losses of TS in \% (100 \% = no losses)
#'
#'@importFrom Root eff_losses
#'@param object A TS
#'@return  eff_losses of TS
#'@export
#'
setMethod("eff_losses", signature(object = "TS"), function(object) {
  return(object@eff_losses)
})
# #----percentageOverheating---------------------------------
# #'percentageOverheating (get)
# #'
# #'Get percentageOverheating of TS
# #'
# #'@param object A TS
# #'@return  percentageOverheating of TS
# #'@export
# #'
# setMethod("percentageOverheating", signature(object = "TS"), function(object) {
#   return(object@percentageOverheating)
# })
#----percentageEnd-----------------------------------------
#'percentageEnd (get)
#'
#'Get percentageEnd of TS in \%
#'
#'@importFrom Root percentageEnd
#'@param object A TS
#'@return  percentageEnd of TS
#'@export
#'
setMethod("percentageEnd", signature(object = "TS"), function(object) {
  return(object@percentageEnd)
})
#----percentageStart---------------------------------------
#'percentageStart (get)
#'
#'Get percentageStart of TS in \%
#'
#'@importFrom Root percentageStart
#'@param object A TS
#'@return  percentageStart of TS
#'@export
#'
setMethod("percentageStart", signature(object = "TS"), function(object) {
  return(object@percentageStart)
})
#----EnergyStart-------------------------------------------
#'EnergyStart (get)
#'
#'Get EnergyStart of TS in kWh
#'
#'@importFrom Root EnergyStart
#'@param object A TS
#'@return  EnergyStart of TS
#'@export
#'
setMethod("EnergyStart", signature(object = "TS"), function(object) {
  return(object@EnergyStart)
})
#----EnergyEnd---------------------------------------------
#'EnergyEnd (get)
#'
#'Get EnergyEnd of TS in kWh
#'
#'@importFrom Root EnergyEnd
#'@param object A TS
#'@return  EnergyEnd of TS
#'@export
#'
setMethod("EnergyEnd", signature(object = "TS"), function(object) {
  return(object@EnergyEnd)
})
