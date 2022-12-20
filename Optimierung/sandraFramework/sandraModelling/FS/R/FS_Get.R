#----volume------------------------------------------------
#'volume (get)
#'
#'Get volume of FS in liter
#'
#'@importFrom Root volume
#'@param object A FS
#'@return  volume of FS
#'@export
#'
setMethod("volume", signature(object = "FS"), function(object) {
  return(object@volume)
})
#----maxE_fuel---------------------------------------------
#'maxE_fuel (get)
#'
#'Get maxE_fuel of FS in kWh
#'
#'@importFrom Root maxE_fuel
#'@param object A FS
#'@return  maxE_fuel of FS
#'@export
#'
setMethod("maxE_fuel", signature(object = "FS"), function(object) {
  return(object@maxE_fuel)
})
#----cal-----------------------------------------------
#'cal (get)
#'
#'Get cal of FS in kWh/l
#'
#'@importFrom Root cal
#'@param object A FS
#'@return  cal of FS
#'@export
#'
setMethod("cal", signature(object = "FS"), function(object) {
  return(object@cal)
})

#----maxP_fuel_minus-----------------------------------------
#'maxP_fuel_minus (get)
#'
#'Get maxP_fuel_minus of FS in kW
#'
#'@importFrom Root maxP_fuel_minus
#'@param object A FS
#'@return  maxP_fuel_minus of FS
#'@export
#'
setMethod("maxP_fuel_minus", signature(object = "FS"), function(object) {
  return(object@maxP_fuel_minus)
})
#----maxP_fuel_plus------------------------------------------
#'maxP_fuel_plus (get)
#'
#'Get maxP_fuel_plus of FS in kW
#'
#'@importFrom Root maxP_fuel_plus
#'@param object A FS
#'@return  maxP_fuel_plus of FS
#'@export
#'
setMethod("maxP_fuel_plus", signature(object = "FS"), function(object) {
  return(object@maxP_fuel_plus)
})

#----eff_losses--------------------------------------------
#'eff_losses (get)
#'
#'Get eff_losses of FS in \% (100 \% = no losses)
#'
#'@importFrom Root eff_losses
#'@param object A FS
#'@return  eff_losses of FS
#'@export
#'
setMethod("eff_losses", signature(object = "FS"), function(object) {
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
#'@param object A FS
#'@return  percentageEnd of FS
#'@export
#'
setMethod("percentageEnd", signature(object = "FS"), function(object) {
  return(object@percentageEnd)
})
#----percentageStart---------------------------------------
#'percentageStart (get)
#'
#'Get percentageStart of FS in \%
#'
#'@importFrom Root percentageStart
#'@param object A FS
#'@return  percentageStart of FS
#'@export
#'
setMethod("percentageStart", signature(object = "FS"), function(object) {
  return(object@percentageStart)
})
#----EnergyStart-------------------------------------------
#'EnergyStart (get)
#'
#'Get EnergyStart of FS in kWh
#'
#'@importFrom Root EnergyStart
#'@param object A FS
#'@return  EnergyStart of FS
#'@export
#'
setMethod("EnergyStart", signature(object = "FS"), function(object) {
  return(object@EnergyStart)
})
#----EnergyEnd---------------------------------------------
#'EnergyEnd (get)
#'
#'Get EnergyEnd of FS in kWh
#'
#'@importFrom Root EnergyEnd
#'@param object A FS
#'@return  EnergyEnd of FS
#'@export
#'
setMethod("EnergyEnd", signature(object = "FS"), function(object) {
  return(object@EnergyEnd)
})
