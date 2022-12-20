# ---------------------------------------------------------
# the validity function of TS
check.FS <- function(object) {
  errors <- character()






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
#'@slot maxE_fuel maximal energy of TS in kWh
#'@slot cal caloric value of gas in kWh/l
#'@slot maxP_fuel_minus Maximum power for loading the storage system in kW
#'@slot maxP_fuel_plus Maximum power for unloading the storage system in kW
#'@slot eff_losses Efficiency factor of losses in \% over 24 hours(100 \% = no losses)
# #'@slot percentageOverheating overheatin? in \%
#'@slot percentageEnd Filling level at the end of simulation time in \%
#'@slot percentageStart Filling level at the start of simulation time in \%
#'@slot EnergyStart Stored energy at the start of simulation in kWh
#'@slot EnergyEnd Stored energy at the end of simulation in kWh
#'
#'@export
setClass(
  "FS",
  slots = list(
    volume = "numeric" ,
    maxE_fuel = "numeric" ,
    cal = "numeric" ,
    maxP_fuel_minus = "numeric" ,
    maxP_fuel_plus = "numeric" ,
    eff_losses = "numeric" ,
    #percentageOverheating = "numeric" ,
    percentageEnd = "numeric" ,
    percentageStart = "numeric" ,
    EnergyStart = "numeric",
    EnergyEnd = "numeric"
  ),
  contains = "Root",
  validity = check.FS
)
# constructor ---------------------------------------------
# ---------------------------------------------------------
#' new.TS
#'
#'Constructor of TS creates a new obejct of class TS
#'
#'@return New object of class FS
#'@export
new.FS <- function() {
  return(new(Class = "FS", variables= c("P_fuel_plus", "P_fuel_minus", "E_fuel")
  ))
}
