# ---------------------------------------------------------
# the validity function of TS
check.TS <- function(object) {
  errors <- character()

  if (length(object@maxTemp) == 1 &  length(object@minTemp) == 1) {
    if (object@maxTemp < object@minTemp) {
      msg <- "maxTemp has to bigger than minTemp"
      errors <- c(msg, errors)
    }
  }




  if (length(errors) == 0) {
    return(TRUE)
  }else{

    return(errors)
  }
}
# ---------------------------------------------------------
#' Class TS
#'
#'The TS represents the class thermal storage system
#'
#'@slot volume the volume of TS in liter
#'@slot maxE_th maximal energy of TS in kWh
#'@slot maxTemp Maximum temperature of thermal storage system in °C
#'@slot minTemp Minimum temperature of thermal storage system in °C
#'@slot maxP_th_minus Maximum power for loading the thermal storage system in kW
#'@slot maxP_th_plus Maximum power for unloading the thermal storage system in kW
#'@slot effMaxP_th_minus Efficiency factor for loading the thermal storage system in \%
#'@slot eff_losses Efficiency factor of losses in \% over 24 hours(100 \% = no losses)
# #'@slot percentageOverheating overheatin? in \%
#'@slot percentageEnd Filling level at the end of simulation time in \%
#'@slot percentageStart Filling level at the start of simulation time in \%
#'@slot EnergyStart Stored energy at the start of simulation in kWh
#'@slot EnergyEnd Stored energy at the end of simulation in kWh
#'
#'@export
setClass(
  "TS",
  slots = list(
    volume = "numeric" ,
    maxE_th = "numeric" ,
    maxTemp = "numeric" ,
    minTemp = "numeric" ,
    maxP_th_minus = "numeric" ,
    maxP_th_plus = "numeric" ,
    effMaxP_th_minus = "numeric" ,
    eff_losses = "numeric" ,
    #percentageOverheating = "numeric" ,
    percentageEnd = "numeric" ,
    percentageStart = "numeric" ,
    EnergyStart = "numeric",
    EnergyEnd = "numeric"
  ),
  contains = "Root",
  validity = check.TS
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.TS
#'
#'Constructor of TS creates a new obejct of class TS
#'
#'@return New object of class TS
#'@export
new.TS <- function() {
  return(new(Class = "TS", variables= c("P_th_plus", "P_th_minus", "E_th")
  ))
}
